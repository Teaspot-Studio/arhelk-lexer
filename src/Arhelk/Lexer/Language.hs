module Arhelk.Lexer.Language(
    LexerLanguage(..)
  , defaultLexer
  ) where 

import Arhelk.Lexer.Token 
import Data.Text as T
import Text.Parsec
import Text.Parsec.Text

-- | Holds language specific features for lexer
-- Actually the structure is a config for the arhelk lexer
data LexerLanguage a = LexerLanguage {
  -- | List of parsers for all special signs like ".", ":", "^" that aren't part of word
  -- Quotation marks isn't listed here, they are considered in lexerQuotation field
  lexerPunctuation :: [Parser (Token a)]
  -- | Enumerates delimitiers (space, tab, new line, e.t.c.)
, lexerSpaces :: Text 
  -- | Defines parsers for signs that should be factored out from word.
  -- Example: սկզբան՞է => [W սկզբանէ, Q]
, lexerInwordMarks :: [Parser (Token a)]
  -- | Defines open quotation parsers and closing quotation parsers
, lexerQuotation :: [(Parser (), Parser ())]
}

-- | Lexer that defines common space symbols and common quotation styles
defaultLexer :: LexerLanguage a
defaultLexer = LexerLanguage {
    lexerPunctuation = []
  , lexerSpaces = " \n\t\r"
  , lexerInwordMarks = []
  , lexerQuotation = [
      mkQuotation '«' '»'
    , mkQuotation '“' '”'
    , mkQuotation '„' '“'
    , mkQuotation '"' '"'
    , mkQuotation '\'' '\''
    , mkQuotation '‘' '’'
    , mkQuotation '‹' '›']
  }
  where 
    mkQuotation s e = (char s >> return (), char e >> return ())