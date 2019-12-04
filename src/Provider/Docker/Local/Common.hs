module Provider.Docker.Local.Common
  ( Error (..)
  , RawResult
  , Result
  , runDockerCmd
  ) where

{- | Common utilities for local docker operations. -}

import Control.Exception
import System.IO.Error

import Data.List (isInfixOf)

import qualified System.Exit as Exit
import qualified System.Process as P

import qualified Lib.Utils.Result as R

{- | Errors that we can return. -}
data Error =
    NoDocker -- ^ Docker appears not to be installed.
  | InaccessibleDaemon -- ^ Cannot access the docker daemon.
  | NetworkAlreadyExists -- ^ Network already exists.
  | SubnetAlreadyExists -- ^ Subnet already exists.
  | HostAlreadyExists -- ^ Host already exists.
  | Other String -- ^ For unknown errors.
  deriving (Show)

{- | A docker result is either a known error, or string output. -}
type RawResult = R.Result Error String
type Result = IO RawResult

{- | Checks for specific kinds of docker errors. -}
isInaccessibleDaemonErr :: String -> Bool
isInaccessibleDaemonErr e =
  isInfixOf "Cannot connect to the Docker daemon" e
isNetworkAlreadyExistsErr e =
  isInfixOf "network with name" e && isInfixOf "already exists" e
isSubnetAlreadyExistsErr e =
  isInfixOf "Pool overlaps" e && isInfixOf "on this address space" e

{- | Convert 'IOError' into 'Error's. -}
handleError e
  | isDoesNotExistError e = Just $ NoDocker 
  | otherwise = Nothing 

{- | Run a docker command. -}
runDockerCmd cmd args input = do
  result <- tryJust handleError $ P.readProcessWithExitCode cmd args input
  case result of
    Left e -> return (R.Error e)
    Right (code, out, err) ->
      case code of
        Exit.ExitSuccess -> return $ R.Ok out
        Exit.ExitFailure i
          | isInaccessibleDaemonErr err -> 
            return (R.Error InaccessibleDaemon)
          | isNetworkAlreadyExistsErr err ->
            return (R.Error NetworkAlreadyExists)
          | isSubnetAlreadyExistsErr err ->
            return (R.Error SubnetAlreadyExists)
          | otherwise -> 
            return $ R.Error (Other ("Exit " ++ (show i) ++ ": " ++ err))
