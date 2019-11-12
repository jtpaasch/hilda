module Main (main) where

import Control.Exception
import Data.List
import System.Environment
import System.Exit
import System.IO

import qualified Lib.CommandLine.Args as Args
import qualified Lib.CommandLine.Cmd as Cmd

import qualified Conf.Constants as Config
import qualified Conf.CLI as CLI
import qualified Conf.Dispatch as Dispatch

exitWithErr :: String -> Int -> IO a
exitWithErr msg code = hPutStrLn stderr msg >> exitWith (ExitFailure code)

handleError :: SomeException -> IO ()
handleError e = exitWithErr ("An error occurred: " ++ (show e)) 1

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
     "USAGE: " ++ Config.app ++ " [OPTIONS] [COMMANDS]"
    , ""
    , "  The " ++ Config.app ++ " command line tool."
    , ""
    , "OPTIONS:"
    , "  -h, --help   Display this help."
    , ""
    , "COMMANDS:"
    , ""
    , "  foo"
    , "  bar biz"
    ]

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = Args.parse rawArgs CLI.optSpecs CLI.defaults
  let opts = Args.options args
  let invalids = Args.invalid args
  let pos = Args.positional args
  case length invalids > 0 of
    True -> exitWithErr (invalidOpts invalids) 1
    False -> case CLI.help opts of
      True -> exitWithErr usage 1
      False ->
        let result = Cmd.dispatch pos Dispatch.patterns
        in case Cmd.status result of
          Cmd.UnrecognizedSample x ->
            exitWithErr (invalidCommands x) 1
          Cmd.Matched ->
            case Cmd.match result of
              Nothing -> exitWithErr (unrecognizedCommands pos) 1
              Just x -> handle handleError ((Cmd.handler x) args)
