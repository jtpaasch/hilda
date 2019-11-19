module Conf.Paths
  ( Result
  , Error (..)
  , artifactPath
  ) where

{- | Paths used by the application. -}

import Control.Exception
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error

import qualified Conf.Constants as Constants
import qualified Lib.Utils.File as File
import qualified Lib.Utils.Result as R

{- | A result type for 'Path' operations. -}
type Result = IO (R.Result Error FilePath)

{- | Errors the 'Path ' module can throw. -}
data Error =
    NoHomeDir
  | NoFile FilePath
  | NoPerm FilePath
  | InUse FilePath
  | DiskFull FilePath
  | Other String

{- | Get the file associated with an IO error. -}
getFilename :: IOError -> FilePath
getFilename e =
  case ioeGetFileName e of
    Nothing -> "[Unknown file]"
    Just name -> name

{- | Handle errors for grabbing XDG app data dir location. -}
handleAppDirError :: IOError -> Maybe Error
handleAppDirError e 
  | isDoesNotExistError e = Just NoHomeDir
  | otherwise = Nothing

{- | Convert 'IOError's into 'Error's. -}
handleFileIOError :: IOError -> Maybe Error
handleFileIOError e
  | isDoesNotExistError e = Just $ NoFile (getFilename e)
  | isPermissionError e = Just $ NoPerm (getFilename e)
  | isAlreadyInUseError e = Just $ InUse (getFilename e)
  | isFullError e = Just $ DiskFull (getFilename e)
  | otherwise = Nothing

{- | Get the XDG app data dir location. -}
appData :: FilePath -> Result
appData appName = do
  result <- tryJust handleAppDirError (getAppUserDataDirectory appName)
  case result of
    Left e -> return (R.Error e)
    Right path -> do
      result <- tryJust handleFileIOError (File.mkPath path) 
      case result of
        Left e -> return (R.Error e)
        Right _ -> return (R.Ok path)

{- | Where to store app data, relative to the local filesystem. -}
store :: Result
store = appData Constants.app

{- | Where to store app artifacts, relative to 'store'. -}
artifacts :: Result
artifacts = do
  result <- store
  case result of
    R.Error e -> return (R.Error e)
    R.Ok value -> do
      let path = value </> "artifacts"
      result <- tryJust handleFileIOError (File.mkPath path) 
      case result of
        Left e -> return (R.Error e)
        Right _ -> return (R.Ok path)

{- | Construct an artifact path. -}
artifactPath :: String -> Result
artifactPath artifactName = do
  result <- artifacts
  case result of
    R.Error e -> return (R.Error e)
    R.Ok dir -> return (R.Ok $ dir </> artifactName)
