module App.IO.Provider
  ( Result
  , Error (..)
  , Name
  , Network
  , Subnet
  , createNetwork
  , createHost
  , deleteNetwork
  , deleteHost
  ) where

{- | An interface to providers. -}

import qualified Lib.Utils.Result as R

import qualified Provider.Docker.Local.Common as Docker
import qualified Provider.Docker.Local.Network as DockerNet
import qualified Provider.Docker.Local.Container as DockerHost

type RawResult = R.Result Error String
type Result = IO RawResult

type Name = String
type Img = String
type Network = String
type Subnet = String
type Provider = String

{- | Errors that can occur while working with providers. -}
data Error = 
    NoSuchProvider -- ^ When the provider is not known.
  | NoProviderEndpoint -- ^ Can't access provider endpoint.
  | NetworkAlreadyExists -- ^ When the network already exists.
  | SubnetAlreadyExists -- ^ When the subnet already exists.
  | HostAlreadyExists -- When the host already exists.
  | Other String

handleDockerError e =
  case e of
    -- TO DO: pass messages with these errors so they're informative.
    Docker.NoDocker ->
      return $ R.Error NoProviderEndpoint
    Docker.InaccessibleDaemon ->
      return $ R.Error NoProviderEndpoint
    Docker.HostAlreadyExists ->
      return $ R.Error HostAlreadyExists
    Docker.NetworkAlreadyExists ->
      return $ R.Error NetworkAlreadyExists
    Docker.SubnetAlreadyExists ->
      return $ R.Error SubnetAlreadyExists
    Docker.Other msg ->
      return $ R.Error (Other msg)

{- | Create a network. -}
createNetwork :: Name -> Subnet -> Provider -> Result
createNetwork name subnet provider
  | provider == "docker" = do
      result <- DockerNet.create name subnet
      case result of
        R.Error e -> handleDockerError e
        R.Ok output -> return $ R.Ok output
  | otherwise = return $ R.Error NoSuchProvider

{- | Delete a network. -}
deleteNetwork :: Name -> Provider -> Result
deleteNetwork name provider
  | provider == "docker" = do
    result <- DockerNet.delete name
    case result of
      R.Error e -> handleDockerError e
      R.Ok output -> return $ R.Ok output
  | otherwise = return $ R.Error NoSuchProvider

{- | Create a host. -}
createHost :: Name -> Img -> Network -> Provider -> Result
createHost name img network provider
  | provider == "docker" = do
      result <- DockerHost.create name img network
      case result of
        R.Error e -> handleDockerError e
        R.Ok output -> return $ R.Ok output
  | otherwise = return $ R.Error NoSuchProvider

{- | Delete a host. -}
deleteHost :: Name -> Provider -> Result
deleteHost name provider
  | provider == "docker" = do
      result <- DockerHost.delete name
      case result of
        R.Error e -> handleDockerError e
        R.Ok output -> return $ R.Ok output
  | otherwise = return $ R.Error NoSuchProvider
