module Arhelk.Lexer.Grammar(
    arhelkLexer
  , arhelkLexerParse
  , arhelkLexerParseFile
  -- | Testing
  , prop_emptyWord
  , prop_parseSingleWord
  , prop_parseWords
  ) where 

import Arhelk.Lexer.Language
import Arhelk.Lexer.Token 
import Control.Monad 
import Data.Monoid
import Data.Text as T 
import qualified Data.Text.IO as T
import Test.QuickCheck 
import Test.QuickCheck.Modifiers
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
concatSpaced = cutSpace . Prelude.concat 
  where 
    cutSpace [] = []
    cutSpace (NoSpace t : xs) = t : (cutSpace xs)
    cutSpace (Space : xs) = cutSpace xs 

-- | Generic lexer that is customized with given language specification.
-- The lexer cuts the input into words, detects punctuation and quotation (including nested quotes). 
arhelkLexer :: LexerLanguage -> Parser [Token]
arhelkLexer lang@(LexerLanguage {..}) = concatSpaced <$> someToken `sepBy` spaces
  where 
    punctuation = fmap spaced <$> lexerPunctuation
    inWords = fmap spaced <$> lexerInwordMarks

    someToken :: Parser [SpacedToken]
    someToken = choice $ (many1 <$> punctuation) ++
      (fmap (aglutted $ choice punctuation) [single <$> quotation, wordWith inWords])

    wordWith :: [Parser SpacedToken] -> Parser [SpacedToken]
    wordWith ps = do 
      w <- many (choice $ ps++[word])
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
      notFollowedBy (choice $ spaces : punctuation ++ fmap testQuotation lexerQuotation)
      anyChar

    spaces :: Parser SpacedToken
    spaces = oneOf (T.unpack lexerSpaces) >> return Space

    quotation :: Parser SpacedToken
    quotation = NoSpace . Quotation <$> choice (mkQuot <$> lexerQuotation)
      where mkQuot (b, e) = between b e $ arhelkLexer lang

    -- | Parser with followed tokens without spaces
    aglutted aglutp p = do 
      pres <- p 
      gluts <- many aglutp 
      return $ pres ++ gluts

    single t = [t]

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
prop_emptyWord (SpaceText spaces) = case arhelkLexerParse defaultLexer spaces of 
  Right [] -> True
  _ -> False

prop_parseSingleWord :: SomeWord -> Bool
prop_parseSingleWord (SomeWord (Word t1)) = case arhelkLexerParse defaultLexer t1 of 
  Right [Word t2] -> t1 == t2
  _ -> False

prop_parseWords :: [SomeWord] -> SpaceText -> Bool
prop_parseWords ws (SpaceText spaces) = case arhelkLexerParse defaultLexer str of 
  Right ws2 -> let
    ws2' = fmap (\(Word t) -> t) ws2
    in ws1 == ws2'
  _ -> False
  where 
    str = T.intercalate (" " <> spaces) $ (\(SomeWord (Word t)) -> t) <$> ws
    ws1 = (\(SomeWord (Word t)) -> t) <$> ws
