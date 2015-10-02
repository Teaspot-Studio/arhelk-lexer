module Test.Arhelk.Lexer.Token(
    SomeWord(..)
  , SomePunctuation(..)
  , SomeWordWithPos(..)
  , module X
  ) where 

import Arhelk.Lexer.Token as X
import Data.Maybe 
import Data.Monoid
import Data.Text as T
import Test.QuickCheck 
import Test.QuickCheck.Instances()

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