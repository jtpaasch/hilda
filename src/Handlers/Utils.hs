module Handlers.Utils
  ( Error (..)
  , RawResult
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
  , noTableErr
  , noTablePermErr
  , tableInUseErr
  , dbDiskFullErr
  , dbOtherErr
  , templateAlreadyExistsErr
  , require
  , handleArtifactErr
  , handleDBErr
  , handleTemplateErr
  ) where

{- | Common functions/types for all 'Handler' modules. -}

import System.IO (FilePath)

import qualified Lib.IO.File as File
import qualified Lib.Utils.Result as R

import qualified App.IO.Artifact as Artifact
import qualified App.IO.DB as DB

import qualified App.Data.Template as Template

{- | Errors that handlers can return. -}
data Error =
    MissingArg String
  | NoAppDir String
  | AlreadyExists FilePath
  | NoFile FilePath
  | NoPerm FilePath
  | InUse FilePath
  | DiskFull FilePath
  | Other String
  | NoTable FilePath
  | NoTablePerm FilePath
  | TableInUse FilePath
  | DBDiskFull FilePath
  | DBOther String
  | TemplateAlreadyExists String

instance Show Error where
  show (MissingArg msg) = msg
  show (NoAppDir msg) = msg
  show (AlreadyExists msg) = msg
  show (NoFile msg) = msg
  show (NoPerm msg) = msg
  show (InUse msg) = msg
  show (DiskFull msg) = msg
  show (Other msg) = msg
  show (NoTable msg) = msg
  show (NoTablePerm msg) = msg
  show (TableInUse msg) = msg
  show (DBDiskFull msg) = msg
  show (DBOther msg) = msg
  show (TemplateAlreadyExists msg) = msg

{- | A handler result is either a known error, or string output. -}
type RawResult = R.Result Error String
type Result = IO RawResult

{- | Some canned messages. -}
missingFileArgMsg = "You must specify a file. Use '--file path'."
missingStackArgMsg = "You must specify a stack. Use '--stack name'."

{- | Some canned errors. -}
missingArgErr msg = R.Error (MissingArg msg)
noAppDirErr =
  R.Error (NoAppDir $ "Could not identify an app data directory.")
alreadyExistsErr name =
  R.Error (AlreadyExists $ "An artifact '" ++ name ++ "' already exists.")
noFileErr path = 
  R.Error (NoFile $ "No such file: '" ++ path ++ "'.")
noPermErr path = 
  R.Error (NoPerm $ "Can't access file (no permission): '" ++ path ++ "'.")
inUseErr path = 
  R.Error (InUse $ "Can't access file (file in use): '" ++ path ++ "'.")
diskFullErr path = 
  R.Error (DiskFull $ "Can't access file (disk full): '" ++ path ++ "'.")
otherErr msg = R.Error (Other msg)
noTableErr path =
  R.Error (NoTable $ "No such DB table: '" ++ path ++ "'.")
noTablePermErr path = R.Error (
  NoTablePerm $ "Can't access DB table (no permission): '" ++ path ++ "'.")
tableInUseErr path = R.Error (
  TableInUse $ "Can't access DB table (file in use): '" ++ path ++ "'.")
dbDiskFullErr path = R.Error (
  DBDiskFull $ "Can't access DB table (disk full): '" ++ path ++ "'.")
dbOtherErr msg = R.Error (DBOther msg)
templateAlreadyExistsErr stack = R.Error (
  TemplateAlreadyExists 
    ("A record for the stack '" ++ stack ++ "' already exists."))

{- | Require a command line argument. -}
require :: Maybe String -> String -> R.Result Error String
require opt errMsg =
  case opt of
    Nothing -> missingArgErr errMsg
    Just val -> R.Ok val

{- | Handle 'Artifact' errors. -}
handleArtifactErr :: Artifact.Error -> RawResult
handleArtifactErr e =
  case e of
    Artifact.NoAppDataDir -> noAppDirErr
    Artifact.AlreadyExists name -> alreadyExistsErr name 
    Artifact.NoFile file -> noFileErr file
    Artifact.NoPerm file -> noPermErr file
    Artifact.InUse file -> inUseErr file
    Artifact.DiskFull file -> diskFullErr file
    Artifact.Other msg -> otherErr msg

{- | Handle 'DB' errors. -}
handleDBErr :: DB.Error -> RawResult
handleDBErr e =
  case e of
    DB.NoAppDataDir -> noAppDirErr
    DB.NoTable file -> noTableErr file
    DB.NoTablePerm file -> noTablePermErr file
    DB.TableInUse file -> tableInUseErr file
    DB.DiskFull file -> dbDiskFullErr file
    DB.Other msg -> dbOtherErr msg

{- | Handle 'Template' errors. -}
handleTemplateErr :: Template.Error -> RawResult
handleTemplateErr e =
  case e of
    Template.RecordAlreadyExists stack -> templateAlreadyExistsErr stack 

