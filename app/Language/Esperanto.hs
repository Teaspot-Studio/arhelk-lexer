module Language.Esperanto(
    esperantoLanguage
  ) where 

import Arhelk.Lexer
import Text.Parsec

-- | Definition for Esperanto language lexer
esperantoLanguage :: LexerLanguage
esperantoLanguage = defaultLexer {
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