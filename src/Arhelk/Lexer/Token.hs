module Arhelk.Lexer.Token(
    Token(..)
  ) where

import Data.Monoid
import Data.Text as T
import TextShow 

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

instance TextShow Token where 
  showb tok = case tok of 
    Word t -> "W" <> showbSpace <> fromText t
    EndSentence -> "E"
    QuestionMark -> "Q"
    DependentMark -> "D"
    Comma -> "CO"
    Citation -> "CI"
    Quotation t -> "QS\n" <> unlinesB (showb <$> t) <> "QE"