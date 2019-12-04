module Provider.Docker.Local.Container
  ( create
  , delete
  ) where

{- | Create/manage docker containers. -}

import qualified Lib.Utils.Result as R
import qualified Provider.Docker.Local.Common as C

type Name = String
type Img = String
type Network = String

{- | Create a container. -}
create :: Name -> Img -> Network -> C.Result
create name img network = do
  let cmd = "docker"
  let args = [
          "run"
        , "-dit"
        , "--name", name
        , "--network", network
        , img
        ]
  let input = ""
  C.runDockerCmd cmd args input

{- | Delete a container. -}
delete :: Name -> C.Result
delete name = do
  let cmd = "docker"
  let args = [
          "container"
        , "rm"
        , "-f"
        , name
        ]
  let input = ""
  C.runDockerCmd cmd args input
