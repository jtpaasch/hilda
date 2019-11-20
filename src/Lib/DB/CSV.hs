module Lib.DB.CSV 
  ( TableName
  , CSVLine
  , CSV
  , Column
  , Value
  , Cell(..)
  , Row(..)
  , Table(..)
  , mkCell
  , mkRow
  , mkTable
  , headers
  , empty
  , toCSV
  , get
  , query
  , parse
  ) where

{- | A file-based local DB. Converts 'CSV' data into tables/rows/cells. -}

import Data.List

import Lib.Utils.String

-- | Table names are just strings.
type TableName = String

-- | A 'CSVLine' is a single line from a CSV file.
-- It is a string with comma separated values.
type CSVLine = String

-- | A 'CSV' is a list of the lines from a CSV file.
-- The first line is treated as the headers, and the rest of the lines the data.
type CSV = [CSVLine]

-- | 'Column's and 'Value's are just strings.
type Column = String
type Value = String

-- | A 'Cell' is a column, value pair.
data Cell = Cell
  { rawCell :: (Column, Value)
  , column :: Column
  , value :: Value
  }

instance Show Cell where
  show x = "{column = " ++ column x ++ ", value = " ++ value x ++ "}" 

-- | A 'Row' is a list of 'Cell's.
data Row = Row
  { rawRow :: [(Column, Value)]
  , cells :: [Cell]
  }

instance Show Row where
  show x = "{cells = " ++ show (cells x) ++ "}"

-- | A 'Table' is a list of 'Row's.
data Table = Table
  { rawTable :: [[(Column, Value)]]
  , rows :: [Row]
  }

instance Show Table where
  show x = "{rows = " ++ show (rows x) ++ "}"  

{- | Constructs a 'Cell' from a 'Column', 'Value' pair.

>>> mkCell ("Name", "Alice McGovern")
{column = "Name", value = "Alice McGovern"}

-}
mkCell :: (Column, Value) -> Cell
mkCell (c, v) = Cell { rawCell = (c, v), column = c, value = v }

{- | Constructs a 'Row' from a list of 'Column', 'Value' pairs.

>>> mkRow [("Name", "Alice McGovern"), ("Phone", "555-345-7786")]
{cells = [{column = "Name", value = "Alice McGovern"}, {column = "Phone", value = "555-345-7786"}]}

-}
mkRow :: [(Column, Value)] -> Row
mkRow [] = Row { rawRow = [], cells = [] } 
mkRow rawRow = Row { rawRow = rawRow, cells = map mkCell rawRow }

{- | Constructs a 'Table' from a list of '[(Column, Value)]' rows.

>>> mkTable [[("id", "1"), ("item", "Pen")], [("id", "2"), ("item", "Ink")]]
{rows = [{cells = [{column = "id", value = "1"}, {column = "item", value = "Pen"}]}, {cells = [{column = "id", value = "2"}, {column = "item", value = "Ink"}]}]}

-}
mkTable :: [[(Column, Value)]] -> Table
mkTable [[]] = Table { rawTable = [], rows = [] }
mkTable rawTable = Table { rawTable = rawTable, rows = map mkRow rawTable } 

{- | Get the headers in a table. -}
headers :: Table -> [Column]
headers table =
  let r = rows table
  in case length r of
    0 -> []
    _ -> map (\cell -> column cell) (cells (head (r)))

{- | Construct an empty table with specified columns/headers.

>>> empty ["A", "B"]
{rows = []} -- But table has columns/headers named "A" and "B"}
-}
empty :: [Column] -> Table
empty headers =
  let cols = map (\col -> (col, "")) headers
   in Table { rawTable = [cols], rows = [] }

{- | Converts a 'Row' to a 'CSVLine'.

>>> toLine $ Row {cells = [{column = "id", value = "1"}, {column = "item", value = "Pen"}]}
"1,pen"
-}
toCSVLine :: Row -> CSVLine
toCSVLine row =
  let cellData = cells row
      values = map (\cell -> value cell) cellData
   in intercalate "," values

{- | Converts a 'Table' to a 'CSV'. -}
toCSV :: Table -> CSV
toCSV table =
  let rowData = rows table
  in case length rowData > 0 of
    False -> []
    True ->
      let header = intercalate "," (headers table)
          theRows = map (\r -> toCSVLine r) rowData
      in header:theRows

{- | Get the value for a named column in a row.

>>> get {cells = [{column = "id", value = "34"}]} "id"
"34"

-}
get :: Row -> Column -> Maybe Value
get row col = lookup col (rawRow row)

{- | Determines if the value for a specified column in a row satisfies a predicate.

>>> satisfies {cells = [{column = "id", value = "34"}]} "id" (== "34")
True

-}
satisfies :: Row -> Column -> (Value -> Bool) -> Bool
satisfies row col f =
  case lookup col (rawRow row) of
    Nothing -> False
    Just v -> f v

{- | Returns a new table containing only those rows with values 
     in the specified column that satisfy the specified predicate.

>>> query {rows = [{cells = [{column = "id", value = "2"}]}, {cells = [{column = "id", value = "3"}]}]} "id" (== "3")
{rows = [{cells = [{column = "id", value = "3"]}]}

-}
query :: Table -> Column -> (Value -> Bool) -> Table
query table col f = 
  let newRows = filter (\row -> satisfies row col f) (rows table)
   in table { rows = newRows }

{- | Given a 'CSV', convert each row into a list of 'Column', 'Value' pairs. -}
parseRaw :: CSV -> [[(Column, Value)]]
parseRaw [] = []
parseRaw (headings:rows) =
  let headers = split ',' headings
   in map (\row -> let values = split ',' row in zip headers values) rows

{- | Takes a 'CSV' and converts it into a 'Table'. -}
parse :: CSV -> Table
parse [] = mkTable [[]]
parse rawData =
  let cleanedData = filter (\record -> length record > 0) rawData
      parsedData = parseRaw cleanedData
   in mkTable parsedData
