module Dispatch where

{- | Specifies command sequences and halders for each of them. -}

import Args
import CLI
import SequenceMatcher as SeqMatch

import Foo
import Bar

-- | A list of 'Patterns', which have a pattern to match to command-line arguments,
-- and a handler to call if the command line arguments match the pattern. 
patterns :: [Pattern (ParsedArgs OptSet -> IO ())]
patterns =
    [ Pattern { pattern = ["foo"], SeqMatch.handler = Foo.run }
    , Pattern { pattern = ["bar", "biz"], SeqMatch.handler = Bar.run }
    ]

