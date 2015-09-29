import Arhelk.Lexer.Grammar
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Instances

main :: IO ()
main = defaultMainWithOpts [
    testProperty "empty word" prop_emptyWord
  , testProperty "single word" prop_parseSingleWord 
  , testProperty "word list" prop_parseWords
  ] mempty