module Handlers.Scratch
  ( run ) where

{-| A 'Scratch' module, for experimenting. -}

import qualified Lib.CommandLine.Args as Args
import qualified Lib.Utils.File as File
import qualified Lib.Utils.Result as R

import qualified Conf.CLI as CLI

import qualified Handlers.Utils as H

run :: CLI.AppArgs -> H.Result
run args =
  let opts = Args.options args
      file = CLI.file opts
  in case H.require file H.missingFileArgMsg of 
    R.Error err -> return $ R.Error err
    R.Ok path -> do
      result <- File.read path
      case H.handleRead result path of
        R.Error e -> return $ R.Error e
        R.Ok contents -> do
          putStrLn contents
          result' <- File.read path
          case H.handleRead result path of
            R.Error e -> return $ R.Error e
            R.Ok contents' -> return $ R.Ok contents'
