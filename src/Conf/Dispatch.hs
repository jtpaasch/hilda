module Conf.Dispatch 
  ( PatternSpec
  , patterns
  ) where

{- | Specifies command sequences and handlers for each of them. -}

import qualified Lib.CommandLine.Cmd as Cmd

import qualified Conf.CLI as CLI

import qualified Handler.Utils as H
import qualified Handler.Template as Template
import qualified Handler.Deploy as Deploy

{- | An nicer name for a 'Pattern' record. -}
type PatternSpec = Cmd.Pattern (CLI.AppArgs -> H.Result)

{- | A list of 'Pattern' records. -}
patterns :: [PatternSpec]
patterns =
    [ Cmd.Pattern { 
          Cmd.pattern = ["template", "create"]
        , Cmd.handler = Template.create 
        }
    , Cmd.Pattern { 
          Cmd.pattern = ["template", "delete"]
        , Cmd.handler = Template.delete 
        }
    , Cmd.Pattern { 
          Cmd.pattern = ["template", "list"] 
        , Cmd.handler = Template.list
        }
    , Cmd.Pattern { 
          Cmd.pattern = ["template", "details"]
        , Cmd.handler = Template.details 
        }
    , Cmd.Pattern {
          Cmd.pattern = ["deploy", "create"]
        , Cmd.handler = Deploy.create
        }
    , Cmd.Pattern {
          Cmd.pattern = ["deploy", "delete"]
        , Cmd.handler = Deploy.delete
        }
    ]
