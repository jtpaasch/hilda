module Lib.IO.File
  ( RawResultBool
  , ResultBool
  , RawResult
  , Result
  , Error (..)
  , mkPath
  , rm
  , exists
  , Lib.IO.File.read
  , Lib.IO.File.write
  ) where

{- | File handling. -}

import Control.Exception
import System.Directory
import System.IO
import System.IO.Error

import qualified Lib.Utils.Result as R

{- | A result type for 'File' operations. -}
type RawResultBool = R.Result Error Bool
type ResultBool = IO RawResultBool
type RawResult = R.Result Error String
type Result = IO RawResult

{- | Explicit errors. -}
data Error =
    NoFile FilePath
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

{- | Convert 'IOError's into 'Error's. -} 
handleError :: IOError -> Maybe Error
handleError e
  | isDoesNotExistError e = Just $ NoFile (getFilename e)
  | isPermissionError e = Just $ NoPerm (getFilename e)
  | isAlreadyInUseError e = Just $ InUse (getFilename e)
  | isFullError e = Just $ DiskFull (getFilename e)
  | otherwise = Nothing

{- | Create a directory if it doesn't exist. -}
mkPath :: FilePath -> Result
mkPath path = do
  result <- tryJust handleError (createDirectoryIfMissing True path)
  case result of
    Left err -> return (R.Error err)
    Right _ -> return (R.Ok "Ok")

{- | Delete a file. -}
rm :: FilePath -> Bool -> Result
rm path force = do
  result <- tryJust handleError (removeFile path)
  case result of
    Left e -> 
      case e of
        NoFile _ -> 
          case force of
            True -> return (R.Ok "Ok")
            False -> return (R.Error e)
        _ -> return (R.Error e)
    Right _ -> return (R.Ok "Ok")

{- | Check if a file exists. -}
exists :: FilePath -> ResultBool
exists path = do
  result <- tryJust handleError (doesFileExist path)
  case result of
    Left err -> return (R.Error err)
    Right doesExist -> return (R.Ok doesExist)

{- | Read a file and return the result. -}
read :: FilePath -> Result
read path = do
  result <- tryJust handleError (readFile path)
  case result of
    Left err -> return (R.Error err)
    Right contents -> return (R.Ok contents)

{- | Write a file and return the result. -}
write :: FilePath -> String -> Result
write path contents = do
  result <- tryJust handleError (writeFile path contents)
  case result of
    Left err -> return (R.Error err)
    Right _ -> return (R.Ok "Ok")
