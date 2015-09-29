import Arhelk.Lexer.Grammar
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Instances

main :: IO ()
main = defaultMainWithOpts [
    testProperty "single word" prop_parseSingleWord 
  ] mempty