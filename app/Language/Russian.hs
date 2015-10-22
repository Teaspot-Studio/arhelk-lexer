module Language.Russian(
    russianLanguage
  ) where 

import Arhelk.Lexer
import Text.Parsec

-- | Definition for Russian language lexer
russianLanguage :: LexerLanguage ()
russianLanguage = defaultLexer {
    lexerPunctuation = [
        char '.' >> return EndSentence
      , char ',' >> return Comma 
      , char ':' >> return Citation
      , char ';' >> return Semicolon
      , char '!' >> return ExclamationMark
      , char '?' >> return QuestionMark
      , (string "—" <|> try (string "--") <|> string "-" ) >> return Dash
      ]
  }