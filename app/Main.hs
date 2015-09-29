{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text as T
import Text.Parsec.Text
import Text.Parsec 
import Data.Monoid 
import Control.Monad 

import Arhelk.Lexer.Token
import Arhelk.Lexer.Grammar
import Arhelk.Lexer.Language
import TextShow 

parseInput :: LexerLanguage -> Text -> Either ParseError [Token]
parseInput l  input = parse (arhelkLexer l) "(unknown)" input

bibleArm :: Text
bibleArm = "Ի սկզբանէ Աստուած ստեղծեց երկինքն ու երկիրը։ Երկիրն անձեւ ու անկազմ էր, խաւար էր տիրում անհունի վրայ, եւ Աստծու հոգին շրջում էր ջրերի վրայ։ Եւ Աստուած ասաց. «Թող լոյս լինի»։ Եւ լոյս եղաւ։ Աստուած տեսաւ, որ լոյսը լաւ է, եւ Աստուած լոյսը բաժանեց խաւարից։ Աստուած լոյսը կոչեց ցերեկ, իսկ խաւարը կոչեց գիշեր։ Եւ եղաւ երեկոյ, եւ եղաւ առաւօտ՝ օր առաջին։ Աստուած ասաց. «Թող տարածութիւն առաջանայ ջրերի միջեւ, եւ ջրերը թող բաժանուեն ջրերից»։ Եւ եղաւ այդպէս։ Աստուած ստեղծեց տարածութիւնը, որով Աստուած տարածութեան ներքեւում եղած ջրերը անջրպետեց տարածութեան վրայ եղած ջրերից։ ։։"

bibleEsp :: Text 
bibleEsp = "En la komenco Dio kreis la cxielon kaj la teron. Kaj la tero estis senforma kaj dezerta, kaj mallumo estis super la abismo; kaj la spirito de Dio sxvebis super la akvo. Kaj Dio diris: Estu lumo; kaj farigxis lumo. Kaj Dio vidis la lumon, ke gxi estas bona; kaj Dio apartigis la lumon de la mallumo. Kaj Dio nomis la lumon Tago, kaj la mallumon Li nomis Nokto. Kaj estis vespero, kaj estis mateno, unu tago."

armenianLang :: LexerLanguage 
armenianLang = defaultLexer {
    lexerPunctuation = [
        char '։' >> return EndSentence
      , char ',' >> return Comma
      , (char '․' <|> char '.') >> return Citation
      , char '՝' >> return DependentMark
      ] 
      ++ inwords
  , lexerInwordMarks = inwords
  }
  where
    inwords = [
        char '՞' >> return QuestionMark
      , char '՜' >> return ExclamationMark
      , char '՛' >> return MotiveMark
      ]

esperantoLang :: LexerLanguage
esperantoLang = defaultLexer {
    lexerPunctuation = [
        char '.' >> return EndSentence
      , char ',' >> return Comma 
      , char ':' >> return Citation
      , char ';' >> return Semicolon
      , char '!' >> return ExclamationMark
      , char '?' >> return QuestionMark
      , (char '—' <|> string "--") >> return Dash
      ]
  }

main :: IO ()
main = do
  let es = parseInput armenianLang bibleArm
  -- let es = parseInput esperantoLang bibleEsp
  case es of 
    Left err -> print err
    Right res -> putStrLn $ T.unpack $ T.unlines $ showt <$> res