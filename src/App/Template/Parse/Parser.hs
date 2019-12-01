module App.Template.Parse.Parser (
    Tree (..)
  , parse
  ) where

{- | Parses template file token streams. -}

{- TO DO: Errors via the Result type. -}

import qualified App.Template.Types as T
import qualified App.Template.Parse.Lexer as Lex

data Tree =
    NetworkNode Tree Tree Tree
  | SubnetNode T.CIDR
  | HostsNode [Tree]
  | HostNode Tree Tree
  | HostNameNode T.HostName
  | HostBootImgNode T.BootImgName
  | LinksNode [Tree]
  | LinkNode Tree Tree
  | LinkSrcNode T.HostName
  | LinkDstNode T.HostName
  deriving (Show)

next :: [Lex.Token] -> Lex.Token
next [] = Lex.TokEnd
next (tok : toks) = tok

accept :: [Lex.Token] -> [Lex.Token]
accept [] = error "Nothing to accept"
accept (tok : toks) = toks

host :: [Lex.Token] -> (Tree, [Lex.Token])
host toks =
  case next toks of
    Lex.THostName name ->
      let toks' = accept toks
      in case next toks' of
        Lex.TBootImg img ->
          let nameNode = HostNameNode name
              bootImgNode = HostBootImgNode img
              hostNode = HostNode nameNode bootImgNode
          in (hostNode, accept toks')
        _ -> error $ "Expected TBootImg at: " ++ (show toks)
    _ -> error $ "Expected TNodeName at: " ++ (show toks)

parseHosts :: [Lex.Token] -> ([Tree], [Lex.Token])
parseHosts toks =
  case next toks of
    Lex.THost ->
      let (hostNode, toks') = host (accept toks)
      in case next toks' of
        Lex.THost ->
          let (nextHost, toks'') = parseHosts toks'
          in ([hostNode] ++ nextHost, toks'')
        _ -> ([hostNode], toks')
    _ -> error $ "Expected THost at: " ++ (show toks)

hosts :: [Lex.Token] -> (Tree, [Lex.Token])
hosts toks =
  case next toks of
    Lex.THosts ->
      let (hostsTree, toks') = parseHosts (accept toks)
      in (HostsNode hostsTree, toks')
    _ -> error $ "Expected THosts at: " ++ (show toks)

link :: [Lex.Token] -> (Tree, [Lex.Token])
link toks =
  case next toks of
    Lex.TLinkSrc src ->
      let toks' = accept toks
      in case next toks' of 
        Lex.TLinkDst dst ->
          let srcNode = LinkSrcNode src 
              dstNode = LinkDstNode dst
              linkNode = LinkNode srcNode dstNode
          in (linkNode, accept toks')
        _ -> error $ "Expected TLinkDst at: " ++ (show toks)
    _ -> error $ "Expected TLinkSrc at: " ++ (show toks)

parseLinks :: [Lex.Token] -> ([Tree], [Lex.Token])
parseLinks toks =
  case next toks of
    Lex.TLink ->
      let (linkNode, toks') = link (accept toks)
      in case next toks' of
        Lex.TLink -> 
          let (nextLink, toks'') = parseLinks toks'
          in ([linkNode] ++ nextLink, toks'')
        _ -> ([linkNode], toks')
    _ -> error $ "Expected TLink at: " ++ (show toks)

links :: [Lex.Token] -> (Tree, [Lex.Token])
links toks =
  case next toks of
    Lex.TLinks ->
      let (linksTree, toks') = parseLinks (accept toks)
      in (LinksNode linksTree, toks')
    _ -> error $ "Expected TLinks at: " ++ (show toks)

network :: [Lex.Token] -> (Tree, [Lex.Token])
network toks =
  case next toks of
    Lex.TNetwork ->
      let toks' = accept toks
      in case next toks' of
        Lex.TSubnet cidr ->
          let subnetNode = SubnetNode cidr
              (hostsTree, toks'') = hosts (accept toks')
              (linksTree, toks''') = links toks''
          in (NetworkNode subnetNode hostsTree linksTree, toks''')
        _ -> error $ "Expected TSubnet at: " ++ (show toks')
    _ -> error $ "Expected TNetwork at: " ++ (show toks)

parse :: [Lex.Token] -> Tree
parse toks =
  let (tree, toks') = network toks
  in case null toks' of
    True -> tree
    False -> error $ "Leftover tokens: " ++ (show toks')
