module Provider.Docker.Local.Network
  ( create
  , delete
  ) where

{- | Create/manage docker networks. -}

import qualified Lib.Utils.Result as R
import qualified Provider.Docker.Local.Common as C

type Name = String
type Subnet = String

{- | Create a network. -}
create :: Name -> Subnet -> C.Result 
create name subnet = do
  let cmd = "docker"
  let args = [
          "network"
        , "create"
        , "--driver", "bridge"
        , "--subnet", subnet
        , name
        ]
  let input = ""
  C.runDockerCmd cmd args input

{- | Delete a network. -}
delete :: Name -> C.Result
delete name = do
  let cmd = "docker"
  let args = [
          "network"
        , "rm"
        , name
        ]
  let input = ""
  C.runDockerCmd cmd args input
