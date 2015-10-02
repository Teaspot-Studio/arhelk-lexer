module Language.Armenian(
    armenianLanguage
  , ArmenianToken(..)
  ) where

import Arhelk.Lexer
import Text.Parsec
import TextShow 

-- | Extenstion to Token AST
data ArmenianToken = MotiveMark | DependentMark
  deriving (Eq, Show)

instance TextShow ArmenianToken where 
  showb t = case t of 
    MotiveMark -> "M"
    DependentMark -> "DE"

-- | Lexer specification for Armenian language
armenianLanguage :: LexerLanguage ArmenianToken
armenianLanguage = defaultLexer {
    lexerPunctuation = [
        char '։' >> return EndSentence
      , char ',' >> return Comma
      , (char '․' <|> char '.') >> return Citation
      , char '՝' >> return (ExtToken DependentMark)
      ] 
  , lexerInwordMarks = [
        char '՞' >> return QuestionMark
      , char '՜' >> return ExclamationMark
      , char '՛' >> return (ExtToken MotiveMark)
      ]
  }