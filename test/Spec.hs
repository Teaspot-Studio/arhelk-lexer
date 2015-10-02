import Test.Arhelk.Lexer.Grammar
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMainWithOpts [
    testProperty "empty word" prop_emptyWord
  , testProperty "single word" prop_parseSingleWord 
  , testProperty "word list" prop_parseWords
  , testProperty "simple punctuation" prop_punctuationSimple1
  , testProperty "leading punctuation" prop_punctuationSimple2
  , testProperty "leading and trailing spaces" prop_trailingSpaces
  , testProperty "trailing punctuation" prop_punctuationSimple3
  , testProperty "trailing punctuation within text" prop_punctuationSimple4
  , testProperty "supplant punctuation" prop_supplant1
  , testProperty "supplant punctuation within text" prop_supplant2
  , testProperty "Armenian 1" prop_armenianTest1
  ] mempty