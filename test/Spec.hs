import Arhelk.Lexer.Grammar
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMainWithOpts [
    testProperty "empty word" prop_emptyWord
  , testProperty "single word" prop_parseSingleWord 
  , testProperty "word list" prop_parseWords
  , testProperty "simple punctuation" prop_punctuationSimple1
  , testProperty "leading punctuation" prop_punctuationSimple2
  ] mempty