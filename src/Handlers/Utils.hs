module Handlers.Utils
  ( Error (..)
  , Result
  ) where

{- | Common functions/types for all 'Handler' modules. -}

import qualified Lib.Utils.Result as R

{- | Errors that handlers can return. -}
data Error =
    Other String

instance Show Error where
  show (Other x) = x

{- | A handler result is either a known error, or string output. -}
type Result = R.Result Error String
