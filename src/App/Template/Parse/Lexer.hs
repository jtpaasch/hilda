module App.Template.Parse.Lexer
  ( Token (..)
  , tokenize
  ) where

{- | Tokenizes a template file. -}

import Data.List (isPrefixOf)

import qualified App.Template.Types as T
import qualified Lib.Utils.String as S

data Token =
    TNetwork
  | TSubnet T.CIDR
  | THosts
  | THost
  | THostName T.HostName
  | TBootImg T.BootImgName
  | TLinks
  | TLink
  | TLinkSrc T.HostName
  | TLinkDst T.HostName
  | TokEnd
  deriving (Show)

netPrefix = "- Network:"
subnetPrefix = "- Subnet:"
hostsPrefix = "- Hosts:"
linksPrefix = "- Links:"
hostPrefix = "- Host:"
hostNamePrefix = "Name:"
hostBootImgPrefix = "Boot:"
linkPrefix = "- Link:"
linkSrcPrefix = "Src:"
linkDstPrefix = "Dst:"

notLinks :: String -> Bool
notLinks line = not $ isPrefixOf linksPrefix line

notEnd :: String -> Bool
notEnd line = not $ null line

notHost :: String -> Bool
notHost line = not $ isPrefixOf hostPrefix line

notLink :: String -> Bool
notLink line = not $ isPrefixOf linkPrefix line

takeHost :: [String] -> ([String], [String])
takeHost rawLines = span notHost rawLines

takeLink :: [String] -> ([String], [String])
takeLink rawLines = span notLink rawLines

takeHosts :: [String] -> ([String], [String])
takeHosts rawLines = span notLinks rawLines

takeLinks :: [String] -> ([String], [String])
takeLinks rawLines = span notEnd rawLines 

tokenizeHostField :: String -> Token
tokenizeHostField line
  | isPrefixOf hostNamePrefix line =
    let value = S.trim $ drop (length hostNamePrefix) line
    in THostName value
  | isPrefixOf hostBootImgPrefix line =
    let value = S.trim $ drop (length hostBootImgPrefix) line
    in TBootImg value
  | otherwise = error $ "Cannot tokenize: " ++ line

tokenizeLinkField :: String -> Token
tokenizeLinkField line
  | isPrefixOf linkSrcPrefix line =
    let value = S.trim $ drop (length linkSrcPrefix) line
    in TLinkSrc value
  | isPrefixOf linkDstPrefix line =
    let value = S.trim $ drop (length linkDstPrefix) line
    in TLinkDst value
  | otherwise = error $ "Cannot tokenize: " ++ line

tokenizeHost :: [String] -> [Token]
tokenizeHost rawLines = map tokenizeHostField rawLines

tokenizeLink :: [String] -> [Token]
tokenizeLink rawLines = map tokenizeLinkField rawLines

tokenizeHosts :: [String] -> [Token]
tokenizeHosts [] = []
tokenizeHosts (line : theRest) =
  let (host, theRest') = takeHost theRest
      hostToks = tokenizeHost host
  in THost : (hostToks ++ tokenizeHosts theRest')

tokenizeLinks :: [String] -> [Token]
tokenizeLinks [] = []
tokenizeLinks (line : theRest) =
  let (link, theRest') = takeLink theRest
      linkToks = tokenizeLink link
  in TLink : (linkToks ++ tokenizeLinks theRest')

tokenizeSubnet :: String -> Token
tokenizeSubnet line = 
  let value = S.trim $ drop (length subnetPrefix) line
  in TSubnet value

tokenize :: [String] -> [Token]
tokenize [] = []
tokenize (line : theRest)
  | isPrefixOf netPrefix line = TNetwork : tokenize theRest
  | isPrefixOf subnetPrefix line = tokenizeSubnet line : tokenize theRest
  | isPrefixOf hostsPrefix line =
    let (hosts, theRest') = takeHosts theRest
        hostToks = tokenizeHosts hosts
    in THosts : (hostToks ++ (tokenize theRest'))
  | isPrefixOf linksPrefix line =
    let (links, theRest') = takeLinks theRest
        linkToks = tokenizeLinks links
    in TLinks : (linkToks ++ (tokenize theRest'))
  | otherwise = error $ "Cannot tokenize: " ++ line
