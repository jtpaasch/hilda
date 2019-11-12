module Handlers.Foo (run) where

{-| The 'Foo' module. -}

import qualified Lib.CommandLine.Args as Args
import qualified Conf.CLI as CLI

run :: Args.ParsedArgs CLI.OptSet -> IO ()
run args =
  putStrLn "Running foo"
