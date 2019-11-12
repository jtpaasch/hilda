module Lib.Utils.Result
  ( Result (..)
  ) where

{- | A simple result type. -}

{- | Has an error, or a successful result. -}
data Result a b = 
    Error a
  | Ok b

{- | Get the 'Error' field from a 'Result'. -}
error :: Result a b -> Maybe a
error result = case result of
  Error a -> Just a
  Ok b -> Nothing

{- | Get the 'Ok' field from a 'Result'. -}
ok :: Result a b -> Maybe b
ok result = case result of
  Error a -> Nothing
  Ok b -> Just b
