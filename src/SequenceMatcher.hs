module SequenceMatcher 
  ( Pattern(..)
  , MatchStatus(..)
  , Match(..)
  , dispatch
  ) where

-- | A sample is a string list to match against some patterns of string lists.
type Sample = [String]

-- | A type that has a pattern, and a handler to return if the pattern is matched.
data Pattern a = Pattern
  { pattern :: Sample
  , handler :: a
  }

-- | The status of a match attempt.
data MatchStatus =
    Matched
  | UnrecognizedSample Sample

-- | A match contains a (possible) matched pattern, and the status.
data Match a = Match
  { match :: Maybe (Pattern a)
  , status :: MatchStatus
  }

-- | Given a sample and a list of patterns, 
-- this function tries to match the sample to a pattern
-- and it returns the resulting match. 
dispatch :: Sample -> [Pattern a] -> Match a
dispatch sample patterns =
  let candidates = filter (\record -> sample == (pattern record)) patterns
   in case length candidates of
     0 -> Match { match = Nothing, status = UnrecognizedSample sample }
     _ -> Match { match = Just (head candidates), status = Matched }

