module Arhelk.Lexer.Token(
    Token(..)
  ) where

import Data.Monoid
import Data.Text as T
import TextShow 

-- | Kinds of tokens in generic lexer
-- Some tokens could be not used in particular languages.
data Token a =
  -- | Sequence on non space and non punctuational symbols
    Word Text 
  -- | End of sentence. Example '.'
  | EndSentence
  -- | Question sign, could also mark end of sentence. Example '?'
  | QuestionMark
  -- | Exclamation sign, could also mark end of sentence. Example '!'
  | ExclamationMark
  -- | Comma sign. Example ','
  | Comma
  -- | Semicolon sign. Example ';'
  | Semicolon
  -- | Citation sign. Example ':'
  | Citation
  -- | Dash sign. Example '—'
  | Dash
  -- | Quotation region. Example '‘’'
  | Quotation [Token a]
  -- | Tokens that are very specific for language
  | ExtToken a 
  deriving (Eq)

instance Show a => Show (Token a) where 
  show tok = case tok of 
    Word t -> "Word \"" <> T.unpack t <> "\""
    EndSentence -> "EndSentence"
    QuestionMark -> "QuestionMark"
    ExclamationMark -> "ExclamationMark"
    Comma -> "Comma"
    Semicolon -> "Semicolon"
    Citation -> "Citation"
    Dash -> "Dash"
    Quotation t -> "Quotation " <> show t
    ExtToken a -> show a

instance TextShow a => TextShow (Token a) where 
  showb tok = case tok of 
    Word t -> "W" <> showbSpace <> fromText t
    EndSentence -> "ES"
    QuestionMark -> "Q"
    ExclamationMark -> "EX"
    Comma -> "CO"
    Semicolon -> "S"
    Citation -> "CI"
    Dash -> "DA"
    Quotation t -> "QS\n" <> unlinesB (showb <$> t) <> "QE"
    ExtToken a -> showb a