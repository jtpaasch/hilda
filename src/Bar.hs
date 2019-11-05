module Bar (run) where

{-| The 'Bar' module. -}

import Args
import CLI

run :: ParsedArgs OptSet -> IO ()
run args =
  putStrLn "Running bar"
