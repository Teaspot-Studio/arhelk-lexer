module Arhelk.Lexer.Grammar(
    arhelkLexer
  , arhelkLexerParse
  , arhelkLexerParseFile
  ) where 

import Prelude as P 
import Arhelk.Lexer.Language
import Arhelk.Lexer.Token 
import Data.Monoid
import Data.Text as T 
import qualified Data.Text.IO as T
import Text.Parsec 
import Text.Parsec.Text

-- | Token or space, used only internaly
data SpacedToken a = NoSpace (Token a) | Space 
  deriving Show 

-- | Converts to token with space
spaced :: Token a -> SpacedToken a
spaced = NoSpace

-- | Concats and removes all space tokens
concatSpaced :: [[SpacedToken a]] -> [Token a]
concatSpaced = cutSpace . P.concat 
  where 
    cutSpace [] = []
    cutSpace (NoSpace t : xs) = t : (cutSpace xs)
    cutSpace (Space : xs) = cutSpace xs 

-- | Generic lexer that is customized with given language specification.
-- The lexer cuts the input into words, detects punctuation and quotation (including nested quotes). 
arhelkLexer :: forall a . Show a => LexerLanguage a -> Parser [Token a]
arhelkLexer lang@(LexerLanguage {..}) = do
  skipMany spacing 
  res <- concatSpaced <$> someToken `sepEndBy` many1 spacing
  skipMany spacing 
  return res
  where 
    inWords = fmap spaced <$> lexerInwordMarks
    punctuation = inWords <> (fmap spaced <$> lexerPunctuation)

    someToken :: Parser [SpacedToken a]
    someToken = choice $ punctuationTokens ++ quotationTokens ++ wordTokens
      where
        punctuationTokens, quotationTokens, wordTokens :: [Parser [SpacedToken a]]
        punctuationTokens = fmap P.concat . aglutted (wordWith inWords) . many1 <$> punctuation
        quotationTokens = [aglutted (choice punctuation) quotation]
        wordTokens = [fmap P.concat $ aglutted (fmap single $ choice punctuation) $ wordWith inWords]

    wordWith :: [Parser (SpacedToken a)] -> Parser [SpacedToken a]
    wordWith ps = do 
      w <- many1 (choice $ ps++[word])
      return $ supplantTokens w

    -- | Supplant non-word tokens within word tokens
    supplantTokens :: [SpacedToken a] -> [SpacedToken a]
    supplantTokens [] = []
    supplantTokens [t] = [t]
    supplantTokens ((NoSpace (Word t1)):(NoSpace (Word t2)):xs) = (NoSpace $ Word $ t1 <> t2):(supplantTokens xs)
    supplantTokens ((NoSpace (Word t1)):t2:(NoSpace (Word t3)):xs) = (NoSpace $ Word $ t1 <> t3):t2:(supplantTokens xs)
    supplantTokens (t1:t2:xs) = t1:t2:(supplantTokens xs)

    word :: Parser (SpacedToken a)
    word = fmap (NoSpace . Word . T.pack) $ many1 $ do 
      let 
        testQuotation (b, e) = do -- Interested in start and end chars only
          _ <- b <|> e 
          return $ NoSpace $ Quotation []
      notFollowedBy (choice $ spacing : punctuation ++ fmap testQuotation lexerQuotation)
      anyChar

    spacing :: Parser (SpacedToken a)
    spacing = oneOf (T.unpack lexerSpaces) >> return Space

    quotation :: Parser (SpacedToken a)
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
arhelkLexerParse :: Show a => LexerLanguage a -> Text -> Either ParseError [Token a]
arhelkLexerParse l = parse (arhelkLexer l) "(stdin)"

-- | Parses given file
arhelkLexerParseFile :: Show a => LexerLanguage a -> FilePath -> IO (Either ParseError [Token a])
arhelkLexerParseFile l n = arhelkLexerParse l <$> T.readFile n 