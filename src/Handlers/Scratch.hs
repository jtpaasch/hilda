module Handlers.Scratch
  ( create 
  , delete 
  ) where

{-| A 'Scratch' module, for experimenting. -}

import qualified Lib.CommandLine.Args as Args
import qualified Lib.Utils.Result as R
import qualified Lib.IO.File as File

import qualified Conf.CLI as CLI
import qualified Conf.Constants as Consts

import qualified Handlers.Utils as H

import qualified App.IO.Artifact as Artifact
import qualified App.IO.DB as DB
import qualified App.Data.Template as Template

-- TO DO: Remove when finished debugging
import qualified Lib.DB.CSV as CSV

create :: CLI.AppArgs -> H.Result
create args =
  -- Make sure the right arguments were provided.
  let opts = Args.options args
      file = CLI.file opts
      stack = CLI.stack opts
  in case H.require file H.missingFileArgMsg of 
    R.Error err -> return $ R.Error err
    R.Ok srcPath -> do
      case H.require stack H.missingStackArgMsg of
        R.Error err -> return $ R.Error err
        R.Ok stk -> do
          -- Copy the src template to local artifact storage.
          result <- Artifact.create Artifact.Template srcPath stk
          case result of
            R.Error e -> return $ H.handleArtifactErr e
            R.Ok path -> do
              -- Load the templates table.
              table <- DB.load Consts.templateTable Consts.templateTableHeaders
              case table of
                R.Error e -> return $ H.handleDBErr e
                R.Ok tbl -> do
                  -- Add an entry for the new template.
                  let result = Template.create tbl stk path
                  case result of
                    R.Error e -> return $ H.handleTemplateErr e
                    R.Ok tbl' -> do
                      -- Save the table.
                      result <- DB.save Consts.templateTable tbl'
                      case result of
                        R.Error e -> return $ H.handleDBErr e 
                        R.Ok tbl'' -> return $ R.Ok (show (CSV.rawTable tbl''))

delete :: CLI.AppArgs -> H.Result
delete args =
  let opts = Args.options args
      stack = CLI.stack opts
  in case H.require stack H.missingStackArgMsg of
    R.Error err -> return $ R.Error err
    R.Ok stk -> do
      table <- DB.load Consts.templateTable Consts.templateTableHeaders
      case table of
        R.Error e -> return $ H.handleDBErr e
        R.Ok tbl -> do
          let path = Template.get tbl stk "path"
          let result = Template.delete tbl stk
          case result of
            R.Error e -> return $ H.handleTemplateErr e
            R.Ok tbl' -> do
              result <- DB.save Consts.templateTable tbl'
              case result of
                R.Error e -> return $ H.handleDBErr e
                R.Ok tbl'' -> do
                  return $ R.Ok (show (CSV.rawTable tbl''))
                  case path of
                    Nothing -> return $ R.Ok "Ok"
                    Just path' -> do
                      result <- Artifact.delete Artifact.Template path'
                      case result of
                        R.Error e -> return $ H.handleArtifactErr e
                        R.Ok value -> return $ R.Ok "Ok"
