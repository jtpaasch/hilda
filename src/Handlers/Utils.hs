module Handlers.Utils
  ( Error (..)
  , Result
  , missingFileArgMsg
  , missingStackArgMsg
  , missingArgErr
  , noAppDirErr
  , alreadyExistsErr
  , noFileErr
  , noPermErr
  , inUseErr
  , diskFullErr
  , otherErr
  , require
  , handleArtifactErr
  ) where

{- | Common functions/types for all 'Handler' modules. -}

import System.IO (FilePath)

import qualified Lib.Utils.File as File
import qualified Lib.Utils.Result as R

import qualified App.Utils.Artifact as Artifact

{- | Errors that handlers can return. -}
data Error =
    MissingArg String
  | NoAppDir String
  | AlreadyExists String
  | NoFile FilePath
  | NoPerm FilePath
  | InUse FilePath
  | DiskFull FilePath
  | Other String

instance Show Error where
  show (MissingArg msg) = msg
  show (NoAppDir msg) = msg
  show (AlreadyExists msg) = msg
  show (NoFile msg) = msg
  show (NoPerm msg) = msg
  show (InUse msg) = msg
  show (DiskFull msg) = msg
  show (Other msg) = msg

{- | A handler result is either a known error, or string output. -}
type Result = IO (R.Result Error String)

{- | Some canned messages. -}
missingFileArgMsg = "You must specify a file. Use '--file path'."
missingStackArgMsg = "You must specify a stack. Use '--stack name'."

{- | Some canned errors. -}
missingArgErr msg =
  R.Error (MissingArg msg)
noAppDirErr =
  R.Error (NoAppDir $ "Could not identify an app data directory.")
alreadyExistsErr name =
  R.Error (AlreadyExists $ "'" ++ name ++ "' already exists.")
noFileErr path = 
  R.Error (NoFile $ "No such file: '" ++ path ++ "'.")
noPermErr path = 
  R.Error (NoPerm $ "Can't access file (no permission): '" ++ path ++ "'.")
inUseErr path = 
  R.Error (InUse $ "Can't access file (file in use): '" ++ path ++ "'.")
diskFullErr path = 
  R.Error (DiskFull $ "Can't access file (disk full): '" ++ path ++ "'.")
otherErr msg =
  R.Error (Other msg)

{- | Require a command line argument. -}
require :: Maybe String -> String -> R.Result Error String
require opt errMsg =
  case opt of
    Nothing -> missingArgErr errMsg
    Just val -> R.Ok val

{- | Handle 'Artifact' errors. -}
handleArtifactErr e =
  case e of
    Artifact.NoAppDataDir -> noAppDirErr
    Artifact.AlreadyExists name -> alreadyExistsErr name 
    Artifact.NoFile file -> noFileErr file
    Artifact.NoPerm file -> noPermErr file
    Artifact.InUse file -> inUseErr file
    Artifact.DiskFull file -> diskFullErr file
    Artifact.Other msg -> otherErr msg

