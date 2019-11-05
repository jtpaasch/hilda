module Foo (run) where

{-| The 'Foo' module. -}

import Args
import CLI

run :: ParsedArgs OptSet -> IO ()
run args =
  putStrLn "Running foo"
