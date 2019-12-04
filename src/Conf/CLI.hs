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
  , name :: Maybe String
  , file :: Maybe String
  , template :: Maybe String
  , provider :: Maybe String
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
      Args.ids = ["--name"]
    , Args.flag = False
    , Args.handler = \h optSet idents -> optSet { name = Args.next idents }
    }
  , Args.Opt {
      Args.ids = ["--file"]
    , Args.flag = False
    , Args.handler = \h optSet idents -> optSet { file = Args.next idents }
    }
  , Args.Opt {
      Args.ids = ["--template"]
    , Args.flag = False
    , Args.handler = \h optSet idents -> optSet { template = Args.next idents }
    }
  , Args.Opt {
      Args.ids = ["--provider"]
    , Args.flag = False
    , Args.handler = \h optSet idents -> optSet {provider = Args.next idents }
    }
  ]

{- | A default 'ParsedArgs' record. -}
defaults :: Args.ParsedArgs OptSet
defaults = Args.ParsedArgs {
    Args.options = OptSet {
        help = False
      , name = Nothing
      , file = Nothing
      , template = Nothing
      , provider = Nothing
      }
  , Args.invalid = []
  , Args.positional = []
  }
