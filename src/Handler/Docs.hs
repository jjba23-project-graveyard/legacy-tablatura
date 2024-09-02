{-
  Copyright © 2022 Josep Bigorra

  This file is part of Tablatura.
  Tablatura is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation,
    either version 3 of the License, or (at your option) any later version.

  Tablatura is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  See the GNU General Public License for more details.
  You should have received a copy of the GNU General Public License along with Foobar.
  If not, see <https://www.gnu.org/licenses/>. 
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Docs where

import Import
import Text.Julius (juliusFile)

getPrivacyStatementR :: Handler Html
getPrivacyStatementR = do
  defaultLayout $ do
    setTitle $ fromString "Privacy statement | Tablatura, for all your music"
    let importJsFile = $(juliusFile "templates/js/import.julius")
    $(widgetFile "docs/privacy")

getLegalStatementR :: Handler Html
getLegalStatementR = do
  defaultLayout $ do
    setTitle $ fromString "Legal statement | Tablatura, for all your music"
    let importJsFile = $(juliusFile "templates/js/import.julius")
    $(widgetFile "docs/legal")

getLicenseStatementR :: Handler Html
getLicenseStatementR = do
  defaultLayout $ do
    setTitle $ fromString "License statement | Tablatura, for all your music"
    let importJsFile = $(juliusFile "templates/js/import.julius")
    $(widgetFile "docs/license")
