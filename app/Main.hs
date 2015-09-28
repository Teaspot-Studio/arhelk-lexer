{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.Text as T
import Text.Parsec.Text
import Text.Parsec 
import Data.Monoid 
import Control.Monad 

data Token =
    Word Text 
  | EndSentence
  | QuestionMark
  | DependentMark
  | Comma
  | Citation
  | Space
  | Quotation [Token]
  deriving Show

showTokens :: [Token] -> Text
showTokens = T.unlines . fmap go 
  where 
    go r = case r of 
      Word t -> "W " <> t 
      EndSentence -> "E"
      QuestionMark -> "Q"
      DependentMark -> "D"
      Comma -> "CO"
      Citation -> "CI"
      Quotation t -> "QS\n" <> showTokens t  <> "QE"

arLexer :: Parser [Token]
arLexer = Prelude.concat <$> someToken `sepBy` spaces
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
    quotation = Quotation <$> between (char '«') (char '»') arLexer

parseCSV :: Text -> Either ParseError [Token]
parseCSV input = parse arLexer "(unknown)" input

bible :: Text
bible = "Ի սկզբանէ Աստուած ստեղծեց երկինքն ու երկիրը։ Երկիրն անձեւ ու անկազմ էր, խաւար էր տիրում անհունի վրայ, եւ Աստծու հոգին շրջում էր ջրերի վրայ։ Եւ Աստուած ասաց. «Թող լոյս լինի»։ Եւ լոյս եղաւ։ Աստուած տեսաւ, որ լոյսը լաւ է, եւ Աստուած լոյսը բաժանեց խաւարից։ Աստուած լոյսը կոչեց ցերեկ, իսկ խաւարը կոչեց գիշեր։ Եւ եղաւ երեկոյ, եւ եղաւ առաւօտ՝ օր առաջին։ Աստուած ասաց. «Թող տարածութիւն առաջանայ ջրերի միջեւ, եւ ջրերը թող բաժանուեն ջրերից»։ Եւ եղաւ այդպէս։ Աստուած ստեղծեց տարածութիւնը, որով Աստուած տարածութեան ներքեւում եղած ջրերը անջրպետեց տարածութեան վրայ եղած ջրերից։ ։։"

main :: IO ()
main = do
  let es = parseCSV bible
  case es of 
    Left err -> print err
    Right res -> putStrLn $ T.unpack $ showTokens res