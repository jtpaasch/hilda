module App.Data.Template
  ( Result
  , Error (..)
  , get
  , create
  , delete
  ) where

import qualified Lib.Utils.Result as R
import qualified Lib.DB.CSV as CSV

{- | A nicer name for results. -}
type Result = R.Result Error CSV.Table

{- | Errors that can happen when working with template records. -}
data Error =
    RecordAlreadyExists String

{- | Find a record. -}
find :: CSV.Table -> CSV.Column -> (CSV.Value -> Bool) -> Maybe CSV.Row
find table col f =
  let result = CSV.query table col f
      rows = CSV.rows result
  in case length rows of
    0 -> Nothing
    _ -> Just $ head (rows)

{- | Get a value. -}
get :: CSV.Table -> String -> CSV.Column -> Maybe CSV.Value
get table stack col =
  case find table "id" (== stack) of
    Nothing -> Nothing
    Just result -> lookup col (CSV.rawRow result)

{- | Add a template record to the templates table. -}
create :: CSV.Table -> String -> FilePath -> Result
create table stack path =
  let result = find table "id" (== stack)
  in case result of
    Just row -> R.Error $ RecordAlreadyExists stack
    Nothing -> 
      let csv = CSV.rawTable table
          newCsv = csv ++ [[("id", stack), ("path", path)]]
      in R.Ok $ CSV.mkTable newCsv

{- | Delete a template record from the templates table. -}
delete :: CSV.Table -> String -> Result
delete table stack =
  let headers = CSV.headers table
      csv = CSV.rawTable table
      newCsv = (filter (\row -> case lookup "id" row  of 
        Just x -> if x == stack then False else True
        Nothing -> True) csv)
  in case length newCsv of
      0 -> R.Ok $ CSV.empty headers
      _ -> R.Ok $ CSV.mkTable newCsv
