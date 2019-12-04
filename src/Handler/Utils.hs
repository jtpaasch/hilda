module Handler.Utils
  ( Error (..)
  , RawResult
  , Result
  , missingFileArgMsg
  , missingNameArgMsg
  , missingTemplateArgMsg
  , missingProviderArgMsg
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
  , noRecordErr
  , noProviderErr
  , noProviderEndpointErr
  , networkAlreadyExistsErr
  , subnetAlreadyExistsErr
  , hostAlreadyExistsErr
  , require
  , handleArtifactErr
  , handleDBErr
  , handleTemplateErr
  , handleProviderErr
  , extractColumn
  , parseTemplate
  ) where

{- | Common functions/types for all 'Handler' modules. -}

import System.IO (FilePath)

import qualified Lib.DB.CSV as CSV
import qualified Lib.IO.File as File
import qualified Lib.Utils.Result as R
import qualified Lib.Utils.String as S

import qualified App.IO.Artifact as Artifact
import qualified App.IO.DB as DB
import qualified App.IO.Provider as Provider

import qualified App.Data.Template as Template

import qualified App.Template.Types as T
import qualified App.Template.Parse.Lexer as Lexer
import qualified App.Template.Parse.Parser as Parser
import qualified App.Template.Parse.Lifter as Lifter

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
  | NoRecord String
  | NoProvider String
  | NoProviderEndpoint String
  | NetworkAlreadyExists String
  | SubnetAlreadyExists String
  | HostAlreadyExists String

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
  show (NoRecord msg) = msg
  show (NoProvider msg) = msg
  show (NoProviderEndpoint msg) = msg
  show (NetworkAlreadyExists msg) = msg
  show (SubnetAlreadyExists msg) = msg
  show (HostAlreadyExists msg) = msg

{- | A handler result is either a known error, or string output. -}
type RawResult = R.Result Error String
type Result = IO RawResult

{- | Some canned messages. -}
missingFileArgMsg = "You must specify a file. Use '--file path'."
missingNameArgMsg = "You must specify a name. Use '--name value'."
missingTemplateArgMsg = "You must specify a template. Use '--template value'."
missingProviderArgMsg = "You must specify a provider. Use '--provider value'."

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
templateAlreadyExistsErr name = R.Error (
  TemplateAlreadyExists 
    ("A record for the template '" ++ name ++ "' already exists."))
noRecordErr name = R.Error (
  NoRecord ("No record of '" ++ name ++ "'."))
noProviderErr = R.Error $ NoProvider "No such provider."
noProviderEndpointErr =
  R.Error $ NoProviderEndpoint "Provider endpoint not accessible."
networkAlreadyExistsErr = 
  R.Error $ NetworkAlreadyExists "Network already exists."
subnetAlreadyExistsErr =
  R.Error $ SubnetAlreadyExists "Subnet already exists."
hostAlreadyExistsErr = 
  R.Error $ HostAlreadyExists "Host already exists."

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
    Template.RecordAlreadyExists name -> templateAlreadyExistsErr name 

{- | Handle 'Provider' errors. -}
handleProviderErr :: Provider.Error -> RawResult
handleProviderErr e =
  case e of
    Provider.NoSuchProvider -> noProviderErr
    Provider.NoProviderEndpoint -> noProviderEndpointErr
    Provider.NetworkAlreadyExists -> networkAlreadyExistsErr
    Provider.SubnetAlreadyExists -> subnetAlreadyExistsErr
    Provider.HostAlreadyExists -> hostAlreadyExistsErr
    Provider.Other msg -> otherErr msg

{- | Get the values for a specific column in a table. -}
extractColumn :: CSV.Table -> CSV.Column -> [String]
extractColumn table col =
  let theRows = CSV.rows table
  in map (\r -> case CSV.get r col of
        Nothing -> "n/a"
        Just value -> value) theRows

{- | Parse the contents of a template. -}
parseTemplate :: String -> T.Network
parseTemplate contents =
  let rawLines = lines contents
      trimmedLines = map S.trim rawLines
      tokens = Lexer.tokenize trimmedLines
      tree = Parser.parse tokens
  in Lifter.lift tree
