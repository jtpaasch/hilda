module Conf.Dispatch 
  ( PatternSpec
  , patterns
  ) where

{- | Specifies command sequences and handlers for each of them. -}

import qualified Lib.CommandLine.Cmd as Cmd

import qualified Conf.CLI as CLI

import qualified Handlers.Utils as H
import qualified Handlers.Foo as Foo

{- | An nicer name for a 'Pattern' record. -}
type PatternSpec = Cmd.Pattern (CLI.AppArgs -> H.Result)

{- | A list of 'Pattern' records. -}
patterns :: [PatternSpec]
patterns =
    [ Cmd.Pattern { Cmd.pattern = ["foo"], Cmd.handler = Foo.run }
    ]
