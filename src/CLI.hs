module CLI (
    OptSet(..) 
  , defaults
  , optSpecs
  ) where

import Args

-- | A custom set of options and their types.
data OptSet = OptSet {
    help :: Bool
  } deriving (Show)

-- | Specs that the 'Args' module will use to find/parse 
-- options in the raw command line arguments.
optSpecs :: [Opt OptSet]
optSpecs = [
    Opt {
        ids = ["-h", "--help"]
      , flag = True
      , handler = \h optSet _ -> optSet { help = True }
      }
  ]

-- | A default 'ParsedArgs' record.
defaults :: ParsedArgs OptSet
defaults = ParsedArgs {
    options = OptSet {
        help = False
      }
  , invalid = []
  , positional = []
  }
