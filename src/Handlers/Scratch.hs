module Handlers.Scratch
  ( create 
  , delete 
  ) where

{-| A 'Scratch' module, for experimenting. -}

import qualified Lib.CommandLine.Args as Args
import qualified Lib.Utils.File as File
import qualified Lib.Utils.Result as R

import qualified Conf.CLI as CLI

import qualified Handlers.Utils as H

import qualified App.Utils.Artifact as Artifact


create :: CLI.AppArgs -> H.Result
create args =
  let opts = Args.options args
      file = CLI.file opts
      stack = CLI.stack opts
  in case H.require file H.missingFileArgMsg of 
    R.Error err -> return $ R.Error err
    R.Ok srcPath -> do
      case H.require stack H.missingStackArgMsg of
        R.Error err -> return $ R.Error err
        R.Ok stk -> do
          result <- Artifact.create srcPath stk
          case result of
            R.Error e -> return $ H.handleArtifactErr e
            R.Ok value -> return $ R.Ok value

delete :: CLI.AppArgs -> H.Result
delete args =
  let opts = Args.options args
      stack = CLI.stack opts
  in case H.require stack H.missingStackArgMsg of
    R.Error err -> return $ R.Error err
    R.Ok stk -> do
      result <- Artifact.delete stk
      case result of
        R.Error e -> return $ H.handleArtifactErr e
        R.Ok value -> return $ R.Ok value

-- TO DO:
-- Get App Data dir
-- Copy file to it, and handle errors
-- Get file from it, and handle errors
-- Only then, try the database
-- TO DO:
-- Make interfaces:
-- * App/Utils/Artifact.hs
-- * App/Utils/DB.hs
-- Those will catch the lower level File errors 
-- and reraise as ArtifactError or DBError.
-- E.g., "CantReadTable" or "NoSuchArtifact".
