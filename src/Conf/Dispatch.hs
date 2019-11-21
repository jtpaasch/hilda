module Conf.Dispatch 
  ( PatternSpec
  , patterns
  ) where

{- | Specifies command sequences and handlers for each of them. -}

import qualified Lib.CommandLine.Cmd as Cmd

import qualified Conf.CLI as CLI

import qualified Handlers.Utils as H
import qualified Handlers.Template as Template

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
    ]
