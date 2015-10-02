{-# LANGUAGE TemplateHaskell #-}
module Main where

import Arhelk.Lexer
import Data.Text as T
import Data.Version 
import Language
import Options.Applicative.Simple
import qualified Data.Text.IO as T
import System.Exit 
import TextShow 

-- | Runtime options to lexer tool
data LexOptions = LexOptions {
  language :: Text
, inputFile :: Maybe FilePath
, outputFile :: Maybe FilePath
}

-- | Parser for runtime parameters for lexing command
parseLexOptions :: Parser LexOptions 
parseLexOptions = LexOptions
  <$> (T.pack <$> strOption (long "language" <> help "Specify language that is used to parse input"))
  <*> (nothing <$> strOption (long "input" <> short 'i' <> help "Input text file" <> value ""))
  <*> (nothing <$> strOption (long "output" <> short 'o' <> help "Output text file" <> value ""))
  where 
    nothing t = if Prelude.null t then Nothing else Just t

main :: IO ()
main = do
  ((), runCommand) <-
    simpleOptions $(simpleVersion $ Version [0, 2, 0, 0] [])
      "Lexer for Arhelk project, splits input into tokens."
      "" 
      (pure ()) $ do
        addCommand "lex" "Perform parsing input into tokens"
          performLexing parseLexOptions
        addCommand "languages" "Displays list of supported languages"
          (const showLanguages) (pure ())
  runCommand

performLexing :: LexOptions -> IO ()
performLexing LexOptions{..} = do
  case lookup language bultinLanguages of 
    Nothing -> do 
      T.putStrLn $ "Error: Don't know language '" <> language <> "', list builtin languages with `arhelk-lexer languages` command."
      exitFailure 
    Just (SomeLexerLanguage lang) -> do 
      input <- maybe T.getContents T.readFile inputFile 
      let output = maybe T.putStr T.writeFile outputFile
      case arhelkLexerParse lang input of 
        Left err -> do 
          T.putStrLn $ "Error: " <> T.pack (show err)
          exitFailure
        Right toks -> do 
          output $ T.unlines $ showt <$> toks 
          exitSuccess

showLanguages :: IO ()
showLanguages = T.putStr $ T.unlines $ fst <$> bultinLanguages