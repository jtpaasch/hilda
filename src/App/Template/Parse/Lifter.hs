module App.Template.Parse.Lifter
  ( lift
  ) where

{- | Lifts a parsed template tree into 'App.Template.Types'. -}

{- TO DO: Handle errors with Result types. -}

import qualified App.Template.Types as T 
import qualified App.Template.Parse.Parser as Parse

liftHosts :: Parse.Tree -> T.Host
liftHosts tree =
  case tree of
    Parse.HostNode nameTree bootImgTree ->
      case nameTree of
        Parse.HostNameNode name ->
          case bootImgTree of
            Parse.HostBootImgNode bootImg ->
              T.Host { T.name = name, T.bootImg = bootImg }
            _ -> error $ "Expected HostBootImgNode: " ++ (show bootImgTree)
        _ -> error $ "Expected HostNameNode: " ++ (show nameTree)
    _ -> error $ "Expected HostNode: " ++ (show tree)

liftLinks :: Parse.Tree -> T.Link
liftLinks tree =
  case tree of
    Parse.LinkNode srcTree dstTree ->
      case srcTree of
        Parse.LinkSrcNode src ->
          case dstTree of
            Parse.LinkDstNode dst ->
              T.Link { T.src = src, T.dst = dst }
            _ -> error $ "Expected LinkDstNode: " ++ (show tree)
        _ -> error $ "Expected LinkSrcNode: " ++ (show tree)
    _ -> error $ "Expected LinkNode: " ++ (show tree)

lift :: Parse.Tree -> T.Network
lift tree =
  case tree of
    Parse.NetworkNode subnetTree hostsTree linksTree ->
      let subnet = case subnetTree of
            Parse.SubnetNode cidr -> cidr
            _ -> error $ "Expected SubnetNode: " ++ (show tree)
          hosts = case hostsTree of
            Parse.HostsNode hosts -> map liftHosts hosts
            _ -> error $ "Expected HostsNode: " ++ (show tree)
          links = case linksTree of
            Parse.LinksNode links -> map liftLinks links
            _ -> error $ "Expected LinksNode: " ++ (show tree)
      in T.Network { T.subnet = subnet, T.hosts = hosts, T.links = links }
    _ -> error $ "Expected NetworkNode: " ++ (show tree)
