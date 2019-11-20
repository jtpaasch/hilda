module App.IO.DB
  ( Result
  , Error (..)
  , load
  , save
  ) where

{- | An intreface to DB operations. -}

import qualified Lib.IO.File as File
import qualified Lib.Utils.Result as R
import qualified Lib.DB.CSV as DB
import qualified Conf.Paths as Paths

{- | A nicer name for results. -}
type RawFileResult = R.Result Error String
type RawResult = R.Result Error DB.Table
type Result = IO RawResult

{- | Errors that can occur while working with the DB. -}
data Error =
    NoAppDataDir
  | NoTable FilePath
  | NoTablePerm FilePath
  | TableInUse FilePath
  | DiskFull FilePath
  | Other String

{- | Handle an attempt to read/write a file. -}
handleFileIO :: File.RawResult -> RawFileResult
handleFileIO result =
  case result of
    R.Error e -> case e of
      File.NoFile file -> R.Error $ NoTable file
      File.NoPerm file -> R.Error $ NoTablePerm file
      File.InUse file -> R.Error $ TableInUse file
      File.DiskFull file -> R.Error $ DiskFull file
      File.Other msg -> R.Error $ Other msg
    R.Ok x -> R.Ok x

{- | Load a table.

If the table doesn't exist, this function returns an empty 
table with the specified headers.

-}
load :: DB.TableName -> [DB.Column] -> Result
load name headers = do
  -- Get the location of the table.
  result <- Paths.dbPath name
  case result of
    R.Error e ->
      case e of
        Paths.NoHomeDir -> return $ R.Error NoAppDataDir
    R.Ok path -> do
      -- Load the file.
      result <- File.read path
      case handleFileIO result of
        R.Error e ->
          case e of
            NoTable _ -> return $ R.Ok (DB.empty headers)
            _ -> return $ R.Error e
        R.Ok contents -> do
          let rawData = lines contents
          return $ R.Ok (DB.parse rawData)

{- | Save a table. -}
save :: DB.TableName -> DB.Table -> Result
save name table = do
  -- Get the location of the table.
  result <- Paths.dbPath name
  case result of
    R.Error e ->
      case e of
        Paths.NoHomeDir -> return $ R.Error NoAppDataDir
    R.Ok path -> do
      -- Save the file.
      let csv = DB.toCSV table
      result <- File.write path $ unlines csv
      case handleFileIO result of
        R.Error e -> return $ R.Error e
        R.Ok _ -> return $ R.Ok table
