module Arhelk.Lexer.Grammar(
    arhelkLexer
  , arhelkLexerParse
  , arhelkLexerParseFile
  -- | Testing
  , prop_emptyWord
  , prop_parseSingleWord
  , prop_parseWords
  , prop_punctuationSimple1
  , prop_punctuationSimple2
  ) where 

import Prelude as P 
import Arhelk.Lexer.Language
import Arhelk.Lexer.Token 
import Control.Monad 
import Data.Monoid
import Data.Text as T 
import qualified Data.Text.IO as T
import Test.QuickCheck 
import Text.Parsec 
import Text.Parsec.Text

-- | Token or space, used only internaly
data SpacedToken = NoSpace Token | Space 
  deriving Show 

-- | Converts to token with space
spaced :: Token -> SpacedToken
spaced = NoSpace

-- | Concats and removes all space tokens
concatSpaced :: [[SpacedToken]] -> [Token]
concatSpaced = cutSpace . P.concat 
  where 
    cutSpace [] = []
    cutSpace (NoSpace t : xs) = t : (cutSpace xs)
    cutSpace (Space : xs) = cutSpace xs 

-- | Generic lexer that is customized with given language specification.
-- The lexer cuts the input into words, detects punctuation and quotation (including nested quotes). 
arhelkLexer :: LexerLanguage -> Parser [Token]
arhelkLexer lang@(LexerLanguage {..}) = concatSpaced <$> someToken `sepBy` many1 spacing
  where 
    punctuation = fmap spaced <$> lexerPunctuation
    inWords = fmap spaced <$> lexerInwordMarks

    someToken :: Parser [SpacedToken]
    someToken = choice $ punctuationTokens ++ quotationTokens ++ wordTokens
      where
        punctuationTokens, quotationTokens, wordTokens :: [Parser [SpacedToken]]
        punctuationTokens = fmap P.concat  . aglutted (wordWith inWords) . many1 <$> punctuation
        quotationTokens = [aglutted (choice punctuation) quotation]
        wordTokens = [fmap P.concat $ aglutted (fmap single $ choice punctuation) $ wordWith inWords]

    wordWith :: [Parser SpacedToken] -> Parser [SpacedToken]
    wordWith ps = do 
      w <- many1 (choice $ ps++[word])
      return $ supplantTokens w

    -- | Supplant non-word tokens within word tokens
    supplantTokens :: [SpacedToken] -> [SpacedToken]
    supplantTokens [] = []
    supplantTokens [t] = [t]
    supplantTokens ((NoSpace (Word t1)):(NoSpace (Word t2)):xs) = (NoSpace $ Word $ t1 <> t2):(supplantTokens xs)
    supplantTokens ((NoSpace (Word t1)):t2:(NoSpace (Word t3)):xs) = (NoSpace $ Word $ t1 <> t3):t2:(supplantTokens xs)
    supplantTokens (t1:t2:xs) = t1:t2:(supplantTokens xs)

    word :: Parser SpacedToken
    word = fmap (NoSpace . Word . T.pack) $ many1 $ do 
      let 
        testQuotation (b, e) = do -- Interested in start and end chars only
          _ <- b <|> e 
          return $ NoSpace $ Quotation []
      notFollowedBy (choice $ spacing : punctuation ++ fmap testQuotation lexerQuotation)
      anyChar

    spacing :: Parser SpacedToken
    spacing = oneOf (T.unpack lexerSpaces) >> return Space

    quotation :: Parser SpacedToken
    quotation = NoSpace . Quotation <$> choice (mkQuot <$> lexerQuotation)
      where mkQuot (b, e) = between b e $ arhelkLexer lang

-- | Just turns elemen into list
single :: a -> [a]
single t = [t]

-- | Parser with followed tokens without spaces
aglutted :: Parser a -> Parser a -> Parser [a]
aglutted aglutp p = p `sepByEndWith1` aglutp

-- | Same as sepByEnd but saves separators
--sepByEndWith p sep = sepByEndWith1 p sep <|> return []
-- | Same as sepByEnd1 but saves separators
sepByEndWith1 :: Parser a -> Parser a -> Parser [a]
sepByEndWith1 p sep = do 
  x <- p 
  xs <- many $ do 
    s <- many1 sep
    y <- optionMaybe p 
    return $ maybe s ((s ++).single) y
  return (x : P.concat xs)

-- | Parses given input
arhelkLexerParse :: LexerLanguage -> Text -> Either ParseError [Token]
arhelkLexerParse l = parse (arhelkLexer l) "(stdin)"

-- | Parses given file
arhelkLexerParseFile :: LexerLanguage -> FilePath -> IO (Either ParseError [Token])
arhelkLexerParseFile l n = arhelkLexerParse l <$> T.readFile n 

newtype SpaceText = SpaceText Text
  deriving Show 

instance Arbitrary SpaceText where 
  arbitrary = SpaceText <$> do 
    NonNegative n <- arbitrary 
    str <- replicateM n $ elements " \t\r\n"
    return $ T.pack str

prop_emptyWord :: SpaceText -> Bool 
prop_emptyWord (SpaceText t) = case arhelkLexerParse defaultLexer t of 
  Right [] -> True
  Left err -> error $ show err
  _ -> False

prop_parseSingleWord :: SomeWord -> Bool
prop_parseSingleWord (SomeWord (Word t1)) = case arhelkLexerParse defaultLexer t1 of 
  Right [Word t2] -> t1 == t2
  Left err -> error $ show err
  _ -> False
prop_parseSingleWord _ = True

prop_parseWords :: [SomeWord] -> SpaceText -> Bool
prop_parseWords ws (SpaceText spacing) = case arhelkLexerParse defaultLexer str of 
  Right ws2 -> let
    ws2' = fmap (\(Word t) -> t) ws2
    in ws1 == ws2'
  Left err -> error $ show err
  where 
    str = T.intercalate (" " <> spacing) $ (\(SomeWord (Word t)) -> t) <$> ws
    ws1 = (\(SomeWord (Word t)) -> t) <$> ws

prop_punctuationSimple1 :: SomeWord -> SomeWord -> SomePunctuation -> SpaceText -> Bool
prop_punctuationSimple1 (SomeWord (Word t1)) (SomeWord (Word t2)) (SomePunctuation pchar pt) (SpaceText spacing) = 
  case arhelkLexerParse lexer t of 
    Right [Word t1', pt', Word t2'] -> t1' == t1 && t2' == t2 && pt' == pt
    Left err -> error $ show err
    _ -> False
  where 
    t = t1 <> spacing <> T.pack [pchar] <> spacing <> t2
    lexer = defaultLexer {
      lexerPunctuation = [char pchar >> return pt]
    }
prop_punctuationSimple1 _ _ _ _ = True

prop_punctuationSimple2 :: SomePunctuation -> SomeWord -> Bool
prop_punctuationSimple2 (SomePunctuation pchar pt) (SomeWord (Word t1)) =
  case arhelkLexerParse lexer t of 
    Right [pt', Word t1'] -> t1' == t1 && pt' == pt
    Left err -> error $ show err
    _ -> False
  where 
    t = T.pack [pchar] <> t1
    lexer = defaultLexer {
      lexerPunctuation = [char pchar >> return pt]
    }
prop_punctuationSimple2 _ _ = True