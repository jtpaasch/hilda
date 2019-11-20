module Conf.Dispatch 
  ( PatternSpec
  , patterns
  ) where

{- | Specifies command sequences and handlers for each of them. -}

import qualified Lib.CommandLine.Cmd as Cmd

import qualified Conf.CLI as CLI

import qualified Handlers.Utils as H
import qualified Handlers.Scratch as Scratch

{- | An nicer name for a 'Pattern' record. -}
type PatternSpec = Cmd.Pattern (CLI.AppArgs -> H.Result)

{- | A list of 'Pattern' records. -}
patterns :: [PatternSpec]
patterns =
    [ Cmd.Pattern { Cmd.pattern = ["create"], Cmd.handler = Scratch.create }
    , Cmd.Pattern { Cmd.pattern = ["delete"], Cmd.handler = Scratch.delete }
    , Cmd.Pattern { Cmd.pattern = ["list"], Cmd.handler = Scratch.list }
    ]
