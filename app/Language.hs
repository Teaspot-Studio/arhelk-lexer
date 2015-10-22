{-# LANGUAGE ExistentialQuantification #-}
module Language(
    bultinLanguages
  , SomeLexerLanguage(..)
  , module X
  ) where

import Arhelk.Lexer
import Data.Text 
import Language.Armenian as X 
import Language.Esperanto as X 
import Language.Russian as X 
import TextShow

data SomeLexerLanguage = forall a . (Show a, TextShow a) => 
  SomeLexerLanguage (LexerLanguage a)

-- | List of builtin lexer languages
bultinLanguages :: [(Text, SomeLexerLanguage)]
bultinLanguages = [
    ("Armenian", SomeLexerLanguage armenianLanguage)
  , ("Esperanto", SomeLexerLanguage esperantoLanguage)
  , ("Russian", SomeLexerLanguage russianLanguage)
  ]
