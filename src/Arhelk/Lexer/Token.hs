module Arhelk.Lexer.Token(
    Token(..)
  -- | Testing
  , SomeWord(..)
  , SomePunctuation(..)
  , SomeWordWithPos(..)
  ) where

import Data.Maybe 
import Data.Monoid
import Data.Text as T
import Test.QuickCheck 
import Test.QuickCheck.Instances()
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

punctuation :: [Char]
punctuation = "`~¡!@#$%^&*()-=_+<>|/?[]{}'\";:։․.,«»“”„“‘’‹›"

spaces :: [Char]
spaces = " \n\r\t"

punctuationTokens :: [Token ()]
punctuationTokens = [
    EndSentence
  , QuestionMark
  , ExclamationMark
  , Comma
  , Semicolon
  , Citation
  , Dash ]

newtype SomeWord = SomeWord (Token ())
  deriving Show

instance Arbitrary SomeWord where 
  arbitrary = SomeWord . Word <$> suchThat arbitrary cond
    where 
      cond :: Text -> Bool
      cond a = T.length a > 0 && and ((not . (a `contains`)) <$> spaces ++ punctuation)

      contains :: Text -> Char -> Bool
      contains t c = isJust $ T.find (== c) t

-- | Some non-empty word withot punctuation
data SomePunctuation = SomePunctuation Char (Token ())

instance Show SomePunctuation where 
  show (SomePunctuation c t) = "SomePunctuation " <> [c] <> " " <> show t

instance Arbitrary SomePunctuation where
  arbitrary = SomePunctuation <$> elements punctuation <*> elements punctuationTokens

-- | Word with position from [1 .. length of word]
data SomeWordWithPos = SomeWordWithPos Int (Token ()) 
  deriving Show 

instance Arbitrary SomeWordWithPos where
  arbitrary = do 
    SomeWord (Word t) <- arbitrary
    i <- suchThat arbitrary (\i -> i <= T.length t && i > 0)
    return $ SomeWordWithPos i (Word t)