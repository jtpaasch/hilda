module Lib.CommandLine.Cmd
  ( Pattern(..)
  , MatchStatus(..)
  , Match(..)
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

{- | The status of a match attempt. -}
data MatchStatus =
    Matched
  | UnrecognizedSample Sample

{- | A match contains a (possible) matched pattern, and the status. -}
data Match a = Match
  { match :: Maybe (Pattern a)
  , status :: MatchStatus
  }

{- | Matches a sample in a list of patterns and returns the first match. -} 
dispatch :: Sample -> [Pattern a] -> Match a
dispatch sample patterns =
  let candidates = filter (\record -> sample == (pattern record)) patterns
   in case length candidates of
     0 -> Match { match = Nothing, status = UnrecognizedSample sample }
     _ -> Match { match = Just (head candidates), status = Matched }
