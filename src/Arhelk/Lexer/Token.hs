module Arhelk.Lexer.Token(
    Token(..)
  -- | Testing
  , SomeWord(..)
  ) where

import Data.Maybe 
import Data.Monoid
import Data.Text as T
import Test.QuickCheck 
import Test.QuickCheck.Instances()
import TextShow 

-- | Kinds of tokens in generic lexer
-- Some tokens could be not used in particular languages.
data Token =
  -- | Sequence on non space and non punctuational symbols
    Word Text 
  -- | End of sentence. Example '.'
  | EndSentence
  -- | Question sign, could also mark end of sentence. Example '?'
  | QuestionMark
  -- | Exclamation sign, could also mark end of sentence. Example '!'
  | ExclamationMark
  -- | Special sign that marks motive inclination. Example '՛'
  | MotiveMark
  -- | Special sign that marks dependent clause. Example '՝'
  | DependentMark
  -- | Comma sign. Example ','
  | Comma
  -- | Semicolon sign. Example ';'
  | Semicolon
  -- | Citation sign. Example ':'
  | Citation
  -- | Dash sign. Example '—'
  | Dash
  -- | Quotation region. Example '‘’'
  | Quotation [Token]
  -- | Direct speech region. Example '—'. 
  | DirectSpeech [Token]
  deriving Show

instance TextShow Token where 
  showb tok = case tok of 
    Word t -> "W" <> showbSpace <> fromText t
    EndSentence -> "ES"
    QuestionMark -> "Q"
    ExclamationMark -> "EX"
    MotiveMark -> "M"
    DependentMark -> "DE"
    Comma -> "CO"
    Semicolon -> "S"
    Citation -> "CI"
    Dash -> "DA"
    Quotation t -> "QS\n" <> unlinesB (showb <$> t) <> "QE"
    DirectSpeech t -> "DS\n" <> unlinesB (showb <$> t) <> "DE"
    
newtype SomeWord = SomeWord Token
  deriving Show 

instance Arbitrary SomeWord where 
  arbitrary = SomeWord . Word <$> suchThat arbitrary cond
    where 
      cond :: Text -> Bool
      cond a = not (T.null a) && and ((not . (a `contains`)) <$> punctuation)

      contains :: Text -> Char -> Bool
      contains t c = isJust $ T.find (== c) t

      punctuation :: [Char]
      punctuation = " \n\t\r`~!@#$%^&*()-=_+<>|\\/?[]{}'\";:։․.,«»“”„“‘’‹›"
