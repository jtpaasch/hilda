module Main (main) where

import Control.Exception
import Data.List
import System.Environment
import System.Exit
import System.IO

import qualified Lib.CommandLine.Args as Args
import qualified Lib.CommandLine.Cmd as Cmd

import qualified Lib.Utils.Result as R

import qualified Conf.Constants as Config
import qualified Conf.CLI as CLI
import qualified Conf.Dispatch as Dispatch

import qualified Handlers.Utils as H

exitWithErr :: String -> Int -> IO a
exitWithErr msg code = hPutStrLn stderr msg >> exitWith (ExitFailure code)

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
    , "- " ++ Config.app ++ " template create --name [name] --file [path]"
    , "  Create a template called [name] from the file located at [path]."
    , ""
    , "- " ++ Config.app ++ " template list"
    , "  List all registered templates."
    , ""
    , "- " ++ Config.app ++ " template delete --name [name]"
    , "  Delete the template called [name]."
    , ""
    , "- " ++ Config.app ++ " template details --name [name]"
    , "  Show the template called [name]."
    ]

handleInvalids :: [String] -> IO ()
handleInvalids invalids =
  case length invalids > 0 of
    True -> exitWithErr (invalidOpts invalids) 1
    False -> return ()

handleHelp :: CLI.OptSet -> IO ()
handleHelp opts =
  case CLI.help opts of
    True -> exitWithErr usage 1
    False -> return () 

findHandler :: [String] -> IO (Dispatch.PatternSpec)
findHandler pos = do
  case Cmd.dispatch pos Dispatch.patterns of
    Nothing -> exitWithErr (invalidCommands pos) 1
    Just result -> return (result)

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = Args.parse rawArgs CLI.optSpecs CLI.defaults
  let opts = Args.options args
  let invalids = Args.invalid args
  let pos = Args.positional args
  handleInvalids invalids
  handleHelp opts
  handle <- findHandler pos 
  result <- (Cmd.handler handle) args
  case result of
    R.Error e -> exitWithErr (show e) 1
    R.Ok output -> putStrLn output
