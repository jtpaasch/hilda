module SequenceMatcher 
  ( Pattern(..)
  , MatchStatus(..)
  , Match(..)
  , dispatch
  ) where

type Sample = [String]

data Pattern a = Pattern
  { pattern :: Sample
  , handler :: a
  }

data MatchStatus =
    Matched
  | UnrecognizedSample Sample

data Match a = Match
  { match :: Maybe (Pattern a)
  , status :: MatchStatus
  }

dispatch :: Sample -> [Pattern a] -> Match a
dispatch sample patterns =
  let candidates = filter (\record -> sample == (pattern record)) patterns
   in case length candidates of
     0 -> Match { match = Nothing, status = UnrecognizedSample sample }
     _ -> Match { match = Just (head candidates), status = Matched }

