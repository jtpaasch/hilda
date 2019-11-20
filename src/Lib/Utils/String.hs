module Lib.Utils.String where

{- | Some string utilities. -}

import Data.Char (isSpace)

blank :: String -> Bool
blank s = all isSpace s

trimLeft :: String -> String
trimLeft = dropWhile (== ' ') 

trimRight :: String -> String
trimRight s = 
  let s' = reverse s
      s'' = trimLeft s'
   in reverse s''

trim :: String -> String
trim = trimLeft . trimRight

splitter :: Char -> String -> [String]
splitter delimiter s =
  case span (/= delimiter) s of
    ("", "") -> []
    (match, theRest) -> 
      case length theRest of
        0 -> [match]
        _ -> 
          case head theRest == delimiter of
            True -> (match : splitter delimiter (tail theRest))
            False -> (match : splitter delimiter theRest)

split :: Char -> String -> [String]
split delimiter s = map trim (splitter delimiter s)
