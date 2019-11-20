module Conf.CLI (
    OptSet (..)
  , AppArgs 
  , defaults
  , optSpecs
  ) where

import qualified Lib.CommandLine.Args as Args

{- | A custom set of options and their types. -}
data OptSet = OptSet {
    help :: Bool
  , stack :: Maybe String
  , file :: Maybe String
  } deriving (Show)

{- | An alias for 'ParsedArgs' parsed into an 'OptSet'. -}
type AppArgs = Args.ParsedArgs OptSet

{- | Specs for 'Args' to match raw command line arguments. -}
optSpecs :: [Args.Opt OptSet]
optSpecs = [
    Args.Opt {
        Args.ids = ["-h", "--help"]
      , Args.flag = True
      , Args.handler = \h optSet _ -> optSet { help = True }
      }
  , Args.Opt {
      Args.ids = ["--stack"]
    , Args.flag = False
    , Args.handler = \h optSet idents -> optSet { stack = Args.next idents }
    }
  , Args.Opt {
      Args.ids = ["--file"]
    , Args.flag = False
    , Args.handler = \h optSet idents -> optSet { file = Args.next idents }
    }
  ]

{- | A default 'ParsedArgs' record. -}
defaults :: Args.ParsedArgs OptSet
defaults = Args.ParsedArgs {
    Args.options = OptSet {
        help = False
      , stack = Nothing
      , file = Nothing
      }
  , Args.invalid = []
  , Args.positional = []
  }