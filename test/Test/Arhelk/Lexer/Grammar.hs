module Test.Arhelk.Lexer.Grammar(
    prop_emptyWord
  , prop_parseSingleWord
  , prop_parseWords
  , prop_trailingSpaces
  , prop_punctuationSimple1
  , prop_punctuationSimple2
  , prop_punctuationSimple3
  , prop_punctuationSimple4
  , prop_supplant1
  , prop_supplant2
  , prop_armenianTest1
  , module X
  ) where

import Arhelk.Lexer.Grammar as X
import Arhelk.Lexer.Language
import Control.Monad 
import Data.Monoid
import Data.Text as T 
import Prelude as P
import Test.Arhelk.Lexer.Token 
import Test.QuickCheck 
import Text.Parsec 

newtype SpaceText = SpaceText Text
  deriving Show 

instance Arbitrary SpaceText where 
  arbitrary = SpaceText <$> do 
    NonNegative n <- arbitrary 
    str <- replicateM n $ elements " \t\r\n"
    return $ T.pack str

testLexer :: LexerLanguage ()
testLexer = defaultLexer

prop_emptyWord :: SpaceText -> Bool 
prop_emptyWord (SpaceText t) = case arhelkLexerParse testLexer t of 
  Right [] -> True
  Left err -> error $ show err
  _ -> False

prop_parseSingleWord :: SomeWord -> Bool
prop_parseSingleWord (SomeWord (Word t1)) = case arhelkLexerParse testLexer t1 of 
  Right [Word t2] -> t1 == t2
  Left err -> error $ show err
  _ -> False
prop_parseSingleWord _ = True

prop_trailingSpaces :: SomeWord -> SpaceText -> Bool
prop_trailingSpaces (SomeWord (Word t1)) (SpaceText spacing) = case arhelkLexerParse testLexer t of 
  Right [Word t2] -> t1 == t2
  Left err -> error $ show err
  _ -> False
  where 
    t = spacing <> t1 <> spacing
prop_trailingSpaces _ _ = True

prop_parseWords :: [SomeWord] -> SpaceText -> Bool
prop_parseWords ws (SpaceText spacing) = case arhelkLexerParse testLexer str of 
  Right ws2 -> let
    ws2' = fmap (\(Word t) -> t) ws2
    in if ws1 == ws2' then True 
      else error $ show ws2
  Left err -> error $ show err
  where 
    str = T.intercalate (" " <> spacing) $ (\(SomeWord (Word t)) -> t) <$> ws
    ws1 = (\(SomeWord (Word t)) -> t) <$> ws

prop_punctuationSimple1 :: SomeWord -> SomeWord -> SomePunctuation -> SpaceText -> Bool
prop_punctuationSimple1 (SomeWord (Word t1)) (SomeWord (Word t2)) (SomePunctuation pchar pt) (SpaceText spacing) = 
  case arhelkLexerParse lexer t of 
    Right [Word t1', pt', Word t2'] -> t1' == t1 && t2' == t2 && pt' == pt
    Left err -> error $ show err
    _ -> False
  where 
    t = t1 <> spacing <> T.pack [pchar] <> spacing <> t2
    lexer = testLexer {
      lexerPunctuation = [char pchar >> return pt]
    }
prop_punctuationSimple1 _ _ _ _ = True

prop_punctuationSimple2 :: SomePunctuation -> SomeWord -> Bool
prop_punctuationSimple2 (SomePunctuation pchar pt) (SomeWord (Word t1)) =
  case arhelkLexerParse lexer t of 
    Right [pt', Word t1'] -> t1' == t1 && pt' == pt
    Left err -> error $ show err
    _ -> False
  where 
    t = T.pack [pchar] <> t1
    lexer = testLexer {
      lexerPunctuation = [char pchar >> return pt]
    }
prop_punctuationSimple2 _ _ = True

prop_punctuationSimple3 :: SomePunctuation -> SomeWord -> Bool
prop_punctuationSimple3 (SomePunctuation pchar pt) (SomeWord (Word t1)) =
  case arhelkLexerParse lexer t of 
    Right [Word t1', pt'] -> t1' == t1 && pt' == pt
    Left err -> error $ show err
    _ -> False
  where 
    t = t1 <> T.pack [pchar]
    lexer = testLexer {
      lexerPunctuation = [char pchar >> return pt]
    }
prop_punctuationSimple3 _ _ = True

prop_punctuationSimple4 :: SomePunctuation -> SomeWord -> SomeWord -> Bool
prop_punctuationSimple4 (SomePunctuation pchar pt) (SomeWord (Word t1)) (SomeWord (Word t2)) =
  case arhelkLexerParse lexer t of 
    Right [Word t1', pt', Word t2'] -> t1' == t1 && pt' == pt && t2' == t2
    Left err -> error $ show err
    _ -> False
  where 
    t = t1 <> T.pack [pchar] <> " " <> t2
    lexer = testLexer {
      lexerPunctuation = [char pchar >> return pt]
    }
prop_punctuationSimple4 _ _ _ = True


prop_supplant1 :: SomeWordWithPos -> SomePunctuation -> Bool 
prop_supplant1 (SomeWordWithPos i (Word t1)) (SomePunctuation pchar pt) = 
  case arhelkLexerParse lexer t of 
    Right [Word t1', pt'] -> t1' == t1 && pt' == pt
    Left err -> error $ show err
    _ -> False
  where 
    t = T.take i t1 <> T.pack [pchar] <> T.drop i t1 
    lexer = testLexer {
      lexerInwordMarks = [char pchar >> return pt]
    }
prop_supplant1 _ _ = True

prop_supplant2 :: SomeWordWithPos -> SomePunctuation -> SomeWord -> Bool 
prop_supplant2 (SomeWordWithPos i (Word t1)) (SomePunctuation pchar pt) (SomeWord (Word t2)) = 
  case arhelkLexerParse lexer t of 
    Right [Word t1', pt', Word t2'] -> t1' == t1 && pt' == pt && t2' == t2
    Left err -> error $ show err
    _ -> False
  where 
    t = T.take i t1 <> T.pack [pchar] <> T.drop i t1 <> " " <> t2
    lexer = testLexer {
      lexerInwordMarks = [char pchar >> return pt]
    }
prop_supplant2 _ _ _ = True

data ArmenianToken = DependentMark | MotiveMark
  deriving (Show, Eq)

prop_armenianTest1 :: Bool 
prop_armenianTest1 = case arhelkLexerParse lexer t of 
  Right ws@[Word t1, Word t2, p1, Word t3, p2] -> if 
         t1 == "Ի" 
      && t2 == "սկզբանէ" 
      && p1 == QuestionMark 
      && p2 == ExclamationMark 
      && t3 == "Աստուած" 
    then True
    else error $ show ws
  Left err -> error $ show err
  _ -> False
  where 
    t = "Ի սկզ՞բանէ Ա՜ստուած"
    lexer = defaultLexer {
        lexerPunctuation = [
            char '։' >> return EndSentence
          , char ',' >> return Comma
          , (char '․' <|> char '.') >> return Citation
          , char '՝' >> return (ExtToken DependentMark)
          ] 
      , lexerInwordMarks = [
            char '՞' >> return QuestionMark
          , char '՜' >> return ExclamationMark
          , char '՛' >> return (ExtToken MotiveMark)
          ]
      }