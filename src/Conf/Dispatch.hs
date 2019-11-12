module Conf.Dispatch 
  ( patterns
  ) where

{- | Specifies command sequences and handlers for each of them. -}

import qualified Lib.CommandLine.Args as Args
import qualified Lib.CommandLine.Cmd as Cmd
import qualified Conf.CLI as CLI

import qualified Handlers.Foo as Foo

{- | A list of 'Pattern' records.

Each record has a pattern (to match to command-line arguments),
and a handler (to call if the command line arguments match the pattern).
-}
patterns :: [Cmd.Pattern (Args.ParsedArgs CLI.OptSet -> IO ())]
patterns =
    [ Cmd.Pattern { Cmd.pattern = ["foo"], Cmd.handler = Foo.run }
    ]
