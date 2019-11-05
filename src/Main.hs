module Main (main) where

import Control.Exception
import Data.List
import System.Environment
import System.Exit
import System.IO

import Args
import CLI
import Dispatch
import SequenceMatcher as SeqMatch

exitWithErr :: String -> Int -> IO a
exitWithErr msg code = hPutStrLn stderr msg >> exitWith (ExitFailure code)

handleError :: SomeException -> IO ()
handleError e =
  exitWithErr ("An error occurred: " ++ (show e)) 1

invalidOpts :: [String] -> String
invalidOpts l =
  "Invalid option(s): " ++ (intercalate ", " l) ++ "\nSee: hilda --help"

invalidCommands :: [String] -> String
invalidCommands l =
  "Invalid commands sequence: " ++ (intercalate " " l) ++ "\nSee: hilda --help"

unrecognizedCommands :: [String] -> String
unrecognizedCommands l =
  "Unrecognized command sequence: " ++ (intercalate ", " l) ++ "\nSee: hilda --help"

usage :: String
usage =
  unlines [
     "USAGE: hilda [OPTIONS] [COMMANDS]"
    , ""
    , "  The hilda command line tool."
    , ""
    , "OPTIONS:"
    , "  -h, --help   Display this help."
    , ""
    , "COMMANDS:"
    , ""
    , "  foo bar"
    ]

run :: IO ()
run = do
  putStrLn "Running hilda. Done."

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = parse rawArgs optSpecs defaults
  let opts = options args
  let invalids = invalid args
  let pos = positional args
  case length invalids > 0 of
    True -> exitWithErr (invalidOpts invalids) 1
    False -> case help opts of
      True -> exitWithErr usage 1
      False ->
        let result = dispatch pos patterns
        in case status result of
          UnrecognizedSample x ->
            exitWithErr (invalidCommands x) 1
          Matched ->
            case match result of
              Nothing -> exitWithErr (unrecognizedCommands pos) 1
              Just x -> handle handleError ((SeqMatch.handler x) args)
