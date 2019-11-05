module Dispatch where

import Args
import CLI
import SequenceMatcher as SeqMatch

patterns :: [Pattern (ParsedArgs OptSet -> IO ())]
patterns =
    [ Pattern { pattern = ["foo"], SeqMatch.handler = foo }
    , Pattern { pattern = ["bar", "biz"], SeqMatch.handler = bar }
    ]

foo :: ParsedArgs OptSet -> IO ()
foo args = putStrLn "Called foo"

bar :: ParsedArgs OptSet -> IO ()
bar args = putStrLn "Called bar"

