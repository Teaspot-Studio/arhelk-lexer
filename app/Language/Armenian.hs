module Language.Armenian(
    armenianLanguage
  ) where

import Arhelk.Lexer
import Text.Parsec

-- | Lexer specification for Armenian language
armenianLanguage :: LexerLanguage 
armenianLanguage = defaultLexer {
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