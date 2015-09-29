module Arhelk.Lexer.Grammar(
    arhelkLexer
  ) where 

import Arhelk.Lexer.Language
import Arhelk.Lexer.Token 
import Control.Monad 
import Data.Monoid
import Data.Text as T 
import Text.Parsec 
import Text.Parsec.Text 

arhelkLexer :: LexerLanguage -> Parser [Token]
arhelkLexer lang@(LexerLanguage {..}) = Prelude.concat <$> someToken `sepBy` spaces
  where 
    someToken :: Parser [Token]
    someToken = choice $ (many1 <$> lexerPunctuation) ++
      (fmap (aglutted $ choice lexerPunctuation) [single <$> quotation, wordWith lexerInwordMarks])

    wordWith :: [Parser Token] -> Parser [Token]
    wordWith ps = do 
      w <- many (choice $ ps++[word])
      return $ supplantTokens w

    -- | Supplant non-word tokens within word tokens
    supplantTokens :: [Token] -> [Token]
    supplantTokens [] = []
    supplantTokens [t] = [t]
    supplantTokens ((Word t1):(Word t2):xs) = (Word $ t1 <> t2):(supplantTokens xs)
    supplantTokens ((Word t1):t2:(Word t3):xs) = (Word $ t1 <> t3):t2:(supplantTokens xs)
    supplantTokens (t1:t2:xs) = t1:t2:(supplantTokens xs)

    word :: Parser Token
    word = fmap (Word . T.pack) $ many1 $ do 
      let quotationChars = (\(b,e) -> (b <|> e) >> return (Quotation [])) <$> lexerQuotation
      notFollowedBy (choice $ spaces : lexerPunctuation ++ quotationChars)
      anyChar

    spaces = oneOf (T.unpack lexerSpaces) >> return Space

    quotation = Quotation <$> choice (mkQuot <$> lexerQuotation)
      where mkQuot (b, e) = between b e $ arhelkLexer lang

    -- | Parser with followed tokens without spaces
    aglutted aglutp p = do 
      pres <- p 
      gluts <- many aglutp 
      return $ pres ++ gluts

    single t = [t]