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
  | ExclamationMark
  | MotiveMark
  | DependentMark
  | Comma
  | Semicolon
  | Citation
  | Space -- TODO: remove this
  | Quotation [Token]
  -- | TODO: direct speech
  deriving Show

instance TextShow Token where 
  showb tok = case tok of 
    Word t -> "W" <> showbSpace <> fromText t
    EndSentence -> "ES"
    QuestionMark -> "Q"
    ExclamationMark -> "EX"
    MotiveMark -> "M"
    DependentMark -> "D"
    Comma -> "CO"
    Semicolon -> "S"
    Citation -> "CI"
    Quotation t -> "QS\n" <> unlinesB (showb <$> t) <> "QE"