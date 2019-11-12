module Handlers.Foo 
  ( run ) where

{-| The 'Foo' module. -}

import qualified Lib.CommandLine.Args as Args
import qualified Lib.Utils.Result as R

import qualified Conf.CLI as CLI

import qualified Handlers.Utils as H

run :: CLI.AppArgs -> H.Result
run args =
  case True of
    False -> R.Error (H.Other "Bad argument list")
    True -> R.Ok "Running foo"
