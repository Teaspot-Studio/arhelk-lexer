module Language(
    bultinLanguages
  , module X
  ) where

import Arhelk.Lexer
import Data.Text 
import Language.Armenian as X 
import Language.Esperanto as X 

-- | List of builtin lexer languages
bultinLanguages :: [(Text, LexerLanguage)]
bultinLanguages = [
    ("Armenian", armenianLanguage)
  , ("Esperanto", esperantoLanguage)
  ]
