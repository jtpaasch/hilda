module Handlers.Utils
  ( Error (..)
  , Result
  , missingFileArgMsg
  , missingArgErr
  , noFileErr
  , noPermErr
  , inUseErr
  , diskFullErr
  , otherErr
  , require
  , handleRead
  ) where

{- | Common functions/types for all 'Handler' modules. -}

import qualified Lib.Utils.File as File
import qualified Lib.Utils.Result as R

{- | Errors that handlers can return. -}
data Error =
    MissingArg String
  | NoFile String
  | NoPerm String
  | InUse String
  | DiskFull String
  | Other String

instance Show Error where
  show (MissingArg msg) = msg
  show (NoFile msg) = msg
  show (NoPerm msg) = msg
  show (InUse msg) = msg
  show (DiskFull msg) = msg
  show (Other msg) = msg

{- | A handler result is either a known error, or string output. -}
type Result = IO (R.Result Error String)

{- | Some canned messages. -}
missingFileArgMsg = "You must specify a file. Use '--file path'."

{- | Some canned errors. -}
missingArgErr msg =
  R.Error (MissingArg msg)
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

{- | Handle an attempt to read from a file. -}
handleRead :: R.Result File.Error String -> FilePath -> R.Result Error String
handleRead result path =
  case result of
    R.Error e -> case e of
      File.NoFile -> noFileErr path
      File.NoPerm -> noPermErr path
      File.InUse -> inUseErr path
      File.DiskFull -> diskFullErr path
      File.Other msg -> otherErr msg
    R.Ok x -> R.Ok x
