module Lib.Utils.File
  ( Result
  , Error (..)
  , Lib.Utils.File.read
  ) where

{- | File handling. -}

import Control.Exception
import System.IO
import System.IO.Error

import qualified Lib.Utils.Result as R

{- | A result type for 'File' operations. -}
type Result = IO (R.Result Error String)

{- | Explicit errors. -}
data Error =
    NoFile
  | NoPerm
  | InUse
  | DiskFull
  | Other String

{- | Convert 'IOError's into 'Error's. -} 
handleError :: IOError -> Maybe Error
handleError e
  | isDoesNotExistError e = Just NoFile
  | isPermissionError e = Just NoPerm
  | isAlreadyInUseError e = Just InUse
  | isFullError e = Just DiskFull
  | otherwise = Nothing

{- | Read a file and return the result. -}
read :: FilePath -> Result
read path = do
  result <- tryJust handleError (readFile path)
  case result of
    Left err -> return (R.Error err)
    Right contents -> return (R.Ok contents)
