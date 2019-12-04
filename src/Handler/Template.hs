module Handler.Template
  ( create 
  , delete 
  , list
  , details
  ) where

{-| Handles requests for templates. -}

import qualified Lib.CommandLine.Args as Args
import qualified Lib.Utils.Result as R
import qualified Lib.IO.File as File

import qualified Conf.CLI as CLI
import qualified Conf.Constants as Consts

import qualified Handler.Utils as H

import qualified App.IO.Artifact as Artifact
import qualified App.IO.DB as DB

import qualified App.Data.Template as Template

{- | Create a new template. -}
create :: CLI.AppArgs -> H.Result
create args =
  -- Make sure the right arguments were provided.
  let opts = Args.options args
      file = CLI.file opts
      name = CLI.name opts
  in case H.require file H.missingFileArgMsg of 
    R.Error err -> return $ R.Error err
    R.Ok srcPath -> do
      case H.require name H.missingNameArgMsg of
        R.Error err -> return $ R.Error err
        R.Ok nm -> do
          -- Copy the src template to local artifact storage.
          result <- Artifact.create Artifact.Template srcPath nm
          case result of
            R.Error e -> return $ H.handleArtifactErr e
            R.Ok path -> do
              -- Load the templates table.
              table <- DB.load Consts.templateTable Consts.templateTableHeaders
              case table of
                R.Error e -> return $ H.handleDBErr e
                R.Ok tbl -> do
                  -- Add an entry for the new template.
                  let result = Template.create tbl nm path
                  case result of
                    R.Error e -> return $ H.handleTemplateErr e
                    R.Ok tbl' -> do
                      -- Save the table.
                      result <- DB.save Consts.templateTable tbl'
                      case result of
                        R.Error e -> return $ H.handleDBErr e 
                        R.Ok tbl'' -> return $ R.Ok "Ok"

{- | Delete a template. -}
delete :: CLI.AppArgs -> H.Result
delete args =
  let opts = Args.options args
      name = CLI.name opts
  in case H.require name H.missingNameArgMsg of
    R.Error err -> return $ R.Error err
    R.Ok nm -> do
      table <- DB.load Consts.templateTable Consts.templateTableHeaders
      case table of
        R.Error e -> return $ H.handleDBErr e
        R.Ok tbl -> do
          let path = Template.get tbl nm "path"
          let result = Template.delete tbl nm
          case result of
            R.Error e -> return $ H.handleTemplateErr e
            R.Ok tbl' -> do
              result <- DB.save Consts.templateTable tbl'
              case result of
                R.Error e -> return $ H.handleDBErr e
                R.Ok tbl'' -> do
                  case path of
                    Nothing -> return $ R.Ok "Ok"
                    Just path' -> do
                      result <- Artifact.delete Artifact.Template path'
                      case result of
                        R.Error e -> return $ H.handleArtifactErr e
                        R.Ok value -> return $ R.Ok "Ok"

{- | List all templates. -}
list :: CLI.AppArgs -> H.Result
list args = do
  table <- DB.load Consts.templateTable Consts.templateTableHeaders
  case table of
    R.Error e -> return $ H.handleDBErr e
    R.Ok tbl ->
      let output = H.extractColumn tbl "id"
      in return $ R.Ok (unlines output)

{- | Details of a template. -}
details :: CLI.AppArgs -> H.Result
details args =
  let opts = Args.options args
      name = CLI.name opts
  in case H.require name H.missingNameArgMsg of
    R.Error err -> return $ R.Error err
    R.Ok nm -> do
      table <- DB.load Consts.templateTable Consts.templateTableHeaders
      case table of
        R.Error e -> return $ H.handleDBErr e
        R.Ok tbl -> do
          let path = Template.get tbl nm "path"
          case path of
            Nothing -> return $ H.noRecordErr nm
            Just path' -> do
              result <- Artifact.details path'
              case result of
                R.Error e -> return $ H.handleArtifactErr e
                R.Ok contents -> return $ R.Ok contents 
