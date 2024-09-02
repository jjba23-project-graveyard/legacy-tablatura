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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Homepage" $ do
        it "loads the index and checks it looks right" $ do
          get HomeR
          statusIs 200
          htmlAnyContain "h1" "a modern framework for blazing fast websites"

          request $ do
              setMethod "POST"
              setUrl HomeR
              addToken
              fileByLabelExact "Choose a file" "test/Spec.hs" "text/plain" -- talk about self-reference
              byLabelExact "What's on the file?" "Some Content"

          statusIs 200
          -- more debugging printBody
          htmlAllContain ".upload-response" "text/plain"
          htmlAllContain ".upload-response" "Some Content"

        -- This is a simple example of using a database access in a test.  The
        -- test will succeed for a fresh scaffolded site with an empty database,
        -- but will fail on an existing database with a non-empty user table.
        it "leaves the user table empty" $ do
          get HomeR
          statusIs 200
          users <- runDB $ selectList ([] :: [Filter User]) []
          assertEq "user table empty" 0 $ length users
