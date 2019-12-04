module Handler.Deploy
  ( create
  , delete
  ) where

{-| Handles requests for deploy. -}

{- TO DO: Store the state of the delpoyment in the DB.
   That way, the user doesn't have to provide the template/provider. -}

{- TO DO: Implement 'deploy list', which lists all live deployments. -}
{- TO DO: Implement 'deploy status', which checks the status of the items. -}

{- Note: We're not creating links explicitly yet. -}

import qualified Lib.CommandLine.Args as Args
import qualified Lib.Utils.Result as R

import qualified Conf.CLI as CLI
import qualified Conf.Constants as Consts

import qualified Handler.Utils as H

import qualified App.IO.Artifact as Artifact
import qualified App.IO.DB as DB
import qualified App.IO.Provider as P

import qualified App.Data.Template as Template
import qualified App.Template.Types as T

{- | Recursively create all the hosts in a list. -}
createHosts provider network [] = return $ R.Ok "Ok"
createHosts provider network (host:hosts) = do
  let name = T.name host
  let img = T.bootImg host
  result <- P.createHost name img network provider
  case result of
    R.Error e -> return $ R.Error e
    R.Ok _ -> createHosts provider network hosts

{- | Recursively delete all the hosts in a list. -}
deleteHosts provider [] = return $ R.Ok "Ok"
deleteHosts provider (host:hosts) = do
  let name = T.name host
  result <- P.deleteHost name provider
  case result of
    R.Error e -> return $ R.Error e
    R.Ok _ -> deleteHosts provider hosts

{- | Create a new deployment. -}
create :: CLI.AppArgs -> H.Result
create args =
  -- Make sure the right arguments were provided.
  let opts = Args.options args
      name = CLI.name opts
      template = CLI.template opts
      provider = CLI.provider opts
  in case H.require name H.missingNameArgMsg of
    R.Error err -> return $ R.Error err
    R.Ok name -> do
      case H.require template H.missingTemplateArgMsg of
        R.Error err -> return $ R.Error err
        R.Ok template -> do
          case H.require provider H.missingProviderArgMsg of
            R.Error err -> return $ R.Error err
            R.Ok provider -> do
              -- Get path to template.
              table <- DB.load Consts.templateTable Consts.templateTableHeaders
              case table of
                R.Error e -> return $ H.handleDBErr e
                R.Ok tbl -> do
                  let path = Template.get tbl template "path"
                  case path of
                    Nothing -> return $ H.noRecordErr template
                    Just path' -> do
                      result <- Artifact.details path'
                      case result of
                        R.Error e -> return $ H.handleArtifactErr e
                        R.Ok contents -> do
                          -- Parse and lift the template.  
                          let network = H.parseTemplate contents
                          let subnet = T.subnet network
                          let hosts = T.hosts network
                          -- Create the network
                          result <- P.createNetwork name subnet provider
                          case result of
                            R.Error e -> return $ H.handleProviderErr e
                            R.Ok output -> do
                              -- Create the hosts.
                              result <- createHosts provider name hosts
                              case result of
                                R.Error e -> return $ H.handleProviderErr e
                                R.Ok _ -> return $ R.Ok "Ok"

{- | Delete a deployment. -}
delete :: CLI.AppArgs -> H.Result
delete args =
  -- Make sure the right arguments were provided.
  let opts = Args.options args
      name = CLI.name opts
      template = CLI.template opts
      provider = CLI.provider opts
  in case H.require name H.missingNameArgMsg of
    R.Error err -> return $ R.Error err
    R.Ok name -> do
      case H.require template H.missingTemplateArgMsg of
        R.Error err -> return $ R.Error err
        R.Ok template -> do
          case H.require provider H.missingProviderArgMsg of
            R.Error err -> return $ R.Error err
            R.Ok provider -> do
              -- Get path to template.
              table <- DB.load Consts.templateTable Consts.templateTableHeaders
              case table of
                R.Error e -> return $ H.handleDBErr e
                R.Ok tbl -> do
                  let path = Template.get tbl template "path"
                  case path of
                    Nothing -> return $ H.noRecordErr template
                    Just path' -> do
                      result <- Artifact.details path'
                      case result of
                        R.Error e -> return $ H.handleArtifactErr e
                        R.Ok contents -> do
                          -- Parse and lift the template.  
                          let network = H.parseTemplate contents
                          let subnet = T.subnet network
                          let hosts = T.hosts network
                          -- Delete the hosts.
                          result <- deleteHosts provider hosts
                          case result of
                            R.Error e -> return $ H.handleProviderErr e
                            R.Ok _ -> do
                              -- Delete the network
                              result <- P.deleteNetwork name provider
                              case result of
                                R.Error e -> return $ H.handleProviderErr e
                                R.Ok _ -> return $ R.Ok "Ok"
