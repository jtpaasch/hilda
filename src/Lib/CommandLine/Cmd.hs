module Lib.CommandLine.Cmd
  ( Pattern (..)
  , dispatch
  ) where

{- | Matches sequences of positional arguments, and calls a handler. -}

{- | A string list to match against patterns. -}
type Sample = [String]

{- | Has a pattern, and a handler. -}
data Pattern a = Pattern
  { pattern :: Sample
  , handler :: a
  }

{- | Matches a sample in a list of patterns and returns the first match. -} 
dispatch :: Sample -> [Pattern a] -> Maybe (Pattern a)
dispatch sample patterns =
  let candidates = filter (\record -> sample == (pattern record)) patterns
   in case length candidates of
     0 -> Nothing
     _ -> Just (head candidates)
