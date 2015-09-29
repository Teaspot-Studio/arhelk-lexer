module Arhelk.Lexer.Grammar(
    arhelkLexer
  ) where 

import Arhelk.Lexer.Token 
import Control.Monad 
import Data.Monoid
import Data.Text as T 
import Text.Parsec 
import Text.Parsec.Text 

arhelkLexer :: Parser [Token]
arhelkLexer = Prelude.concat <$> someToken `sepBy` spaces
  where 
    someToken :: Parser [Token]
    someToken = (singleton <$> endSent) 
      <|> (singleton <$> question)
      <|> (singleton <$> quotation)
      <|> wordWith [endSent, question, quotation]

    singleton t = [t]

    wordWith :: [Parser Token] -> Parser [Token]
    wordWith ps = do 
      w <- many (choice $ ps++[word])
      return $ concatTokens w 

    concatTokens [] = []
    concatTokens [t] = [t]
    concatTokens ((Word t1):(Word t2):xs) = (Word $ t1 <> t2):(concatTokens xs)
    concatTokens ((Word t1):t2:(Word t3):xs) = (Word $ t1 <> t3):t2:(concatTokens xs)
    concatTokens (t1:t2:xs) = t1:t2:(concatTokens xs)

    word :: Parser Token
    word = Word . T.pack <$> many1 (noneOf " ։՞«»\n")
    spaces = oneOf " \n\t\r" >> return Space
    endSent = char '։' >> return EndSentence 
    question = char '՞' >> return QuestionMark
    quotation = Quotation <$> between (char '«') (char '»') arhelkLexer
