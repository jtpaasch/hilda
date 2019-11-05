module Args (
    parse
  , Opt(..)
  , ParsedArgs(..)
  ) where

-- | A type that holds all parsed args.
-- Parsed options are accessed by 'parsed',
-- invalid (unrecognized) options are accessed by 'invalid',
-- and  positional args are accessed as 'positional'.
data ParsedArgs a = ParsedArgs {
    options :: a
  , invalid :: [String]
  , positional :: [String]
}

-- A type to describe an option.
-- 'ids' is a list of identifiers for the option, e.g. '["-h", "--help"]'.
-- 'flag' indicates if the option is a flag. Flags take no arguments.
-- 'handler' is a function that parses the raw form of the argument into
-- the parsed form.
data Opt a = Opt {
    ids :: [String] 
  , flag :: Bool
  , handler :: String -> a -> [String] -> a
  }

-- | Check if a string is in option format (starts with a dash).
isOpt :: String -> Bool
isOpt [] = False
isOpt s =
  case head s of 
    '-' -> True
    _   -> False

-- | Given a string 'ident' identifier and a list of 'Opt's,
-- the 'findOpt' function will find the 'Opt' (if any)
-- that has 'ident' as one of its 'ids'.
findOpt :: String -> [Opt a] -> Maybe (Opt a)
findOpt ident [] = Nothing
findOpt ident (x:xs) =
  case ident `elem` (ids x) of
    True -> Just x
    False -> findOpt ident xs

-- | Given a list of raw command line arguments (strings),
-- a list of 'Opt's, and a default 'ParsedArgs' record,
-- the 'parse' function will return a 'ParsedArgs' record
-- with parsed 'options', 'invalid' (unrecognized) options,
-- and 'positional' arguments.
parse :: [String] -> [Opt a] -> ParsedArgs a -> ParsedArgs a
parse [] args acc = 
  let positionalArgs = reverse (positional acc)
   in acc { positional = positionalArgs }
parse (ident:idents) opts acc =
  case findOpt ident opts of
    Just opt ->
      let handle = handler opt
          optsSoFar = options acc
          newOpts = handle ident optsSoFar idents
          newAcc = acc { options = newOpts }
          remainingRawArgs = if flag opt then idents
                             else tail idents  
       in parse remainingRawArgs opts newAcc
    Nothing -> 
      case isOpt ident of
        True ->
          let newInvalid = ident:(invalid acc)
              newAcc = acc { invalid = newInvalid }
           in parse idents opts newAcc
        False ->
          let newPositional = ident:(positional acc)
              newAcc = acc { positional = newPositional } 
           in parse idents opts newAcc

