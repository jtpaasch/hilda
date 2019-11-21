module App.IO.Artifact
  ( ArtifactURI
  , ArtifactName
  , ArtifactKind (..)
  , Result
  , Error (..)
  , create
  , delete
  , details
  ) where

{- | An interface to app artifacts. -}

import qualified Lib.IO.File as File
import qualified Lib.Utils.Result as R

import qualified Conf.Paths as Paths

{- | A nicer name for results. -}
type RawResultBool = R.Result Error Bool
type RawResult = R.Result Error String
type Result = IO RawResult

{- | Errors that can occur while working with artifacts. -}
data Error =
    NoAppDataDir
  | AlreadyExists FilePath
  | NoFile FilePath
  | NoPerm FilePath
  | InUse FilePath
  | DiskFull FilePath
  | Other String

{- | More convenient type names. -}
type ArtifactURI = FilePath
type ArtifactName = String

{- | Types of artifacts. -}
data ArtifactKind = 
    Template

{- | Functions that look up storage paths for kinds of artifacts. -}
artifactPath :: ArtifactKind -> (ArtifactName -> Paths.Result)
artifactPath kind =
  case kind of
    Template -> Paths.templatePath

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
create :: ArtifactKind -> ArtifactURI -> ArtifactName -> Result
create kind src name = do
  -- Get the app data dir.
  let pathFinder = artifactPath kind
  result <- pathFinder name
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
            True -> return $ R.Error (AlreadyExists path)
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
                    R.Ok _ -> return $ R.Ok path

{- | Delete an artifact. -}
delete :: ArtifactKind -> ArtifactName -> Result
delete kind name = do
  -- Get the app data dir.
  let pathFinder = artifactPath kind
  result <- pathFinder name
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

{- | Get the contents of an artifact. -}
details :: FilePath -> Result
details path = do
  result <- File.read path
  case handleFileIO result of
    R.Error e -> return $ R.Error e
    R.Ok contents -> return $ R.Ok contents
