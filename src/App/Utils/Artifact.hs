module App.Utils.Artifact
  ( ArtifactURI
  , ArtifactName
  , Result
  , Error (..)
  , create
  , delete
  ) where

{- | An interface to app artifacts. -}

import qualified Lib.Utils.File as File
import qualified Lib.Utils.Result as R

import qualified Conf.Paths as Paths

{- | A nicer name for results. -}
type RawResultBool = R.Result Error Bool
type RawResult = R.Result Error String
type Result = IO RawResult

{- | Errors the application can return. -}
data Error =
    NoAppDataDir
  | AlreadyExists ArtifactName
  | NoFile FilePath
  | NoPerm FilePath
  | InUse FilePath
  | DiskFull FilePath
  | Other String
  deriving (Show)

{- | More convenient type names. -}
type ArtifactURI = FilePath
type ArtifactName = String

{- | Handle an attempt to read/write a file. -}
handleFileIO :: File.RawResult -> RawResult
handleFileIO result =
  case result of
    R.Error e -> case e of
      File.NoFile file -> R.Error $ NoFile file
      File.NoPerm file -> R.Error $ NoPerm file
      File.InUse file -> R.Error $ InUse file
      File.DiskFull file -> R.Error $ DiskFull file
      File.Other msg -> R.Error $ Other msg
    R.Ok x -> R.Ok x

{- | Handle IO Bool results. -}
handleFileIOBool :: File.RawResultBool -> RawResultBool
handleFileIOBool result =
  case result of
    R.Error e -> case e of
      File.NoFile file -> R.Error $ NoFile file
      File.NoPerm file -> R.Error $ NoPerm file
      File.InUse file -> R.Error $ InUse file
      File.DiskFull file -> R.Error $ DiskFull file
      File.Other msg -> R.Error $ Other msg
    R.Ok x -> R.Ok x

{- | Take a 'src' artifact and store it with the given name. -}
create :: ArtifactURI -> ArtifactName -> Result
create src name = do
  -- Get the app data dir.
  result <- Paths.artifactPath name
  case result of
    R.Error e -> 
      case e of
        Paths.NoHomeDir -> return $ R.Error NoAppDataDir
    R.Ok path -> do
      -- Make sure the artifact doesn't already exist.
      result <- File.exists path
      case handleFileIOBool result of
        R.Error e -> return $ R.Error e
        R.Ok doesExist ->
          case doesExist of
            True -> return $ R.Error (AlreadyExists name)
            False -> do
              -- Read the 'src' file.
              result <- File.read src
              case handleFileIO result of
                R.Error e -> return $ R.Error e
                R.Ok contents -> do
                  -- Write the contents to the target path.
                  result <- File.write path contents
                  case handleFileIO result of
                    R.Error e -> return $ R.Error e
                    R.Ok output -> return $ R.Ok output

{- | Delete an artifact. -}
delete :: ArtifactName -> Result
delete name = do
  -- Get the app data dir.
  result <- Paths.artifactPath name
  case result of
    R.Error e ->
      case e of
        Paths.NoHomeDir -> return $ R.Error NoAppDataDir
    R.Ok path -> do
      -- Delete the artifact.
      result <- File.rm path True
      case handleFileIO result of
        R.Error e -> return $ R.Error e
        R.Ok output -> return $ R.Ok output

