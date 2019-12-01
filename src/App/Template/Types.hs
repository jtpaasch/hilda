module App.Template.Types
  ( HostName
  , BootImgName
  , CIDR
  , Host (..)
  , Link (..)
  , Network (..)
  ) where

{- | Data types to represent the pieces of a template. -}

type HostName = String
type BootImgName = String
type CIDR = String

data Host = Host {
    name :: HostName
  , bootImg :: BootImgName
  } deriving (Show)

data Link = Link {
    src :: HostName
  , dst :: HostName
  } deriving (Show)

data Network = Network {
    subnet :: CIDR
  , hosts :: [Host]
  , links :: [Link]
  } deriving (Show)
