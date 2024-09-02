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
module Handler.CommentSpec (spec) where

import TestImport
import Data.Aeson

spec :: Spec
spec = withApp $ do
    describe "valid request" $ do
        it "gives a 200" $ do
            get HomeR
            statusIs 200

            let message = "My message" :: Text
                body = object [ "message" .= message ]
                encoded = encode body

            request $ do
                setMethod "POST"
                setUrl CommentR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200

            (Entity _id comment:_) <- runDB $ selectList [CommentMessage ==. message] []
            assertEq "Should have " comment (Comment message Nothing)

    describe "invalid requests" $ do
        it "400s when the JSON body is invalid" $ do
            get HomeR

            let body = object [ "foo" .= ("My message" :: Value) ]

            request $ do
                setMethod "POST"
                setUrl CommentR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")

            statusIs 400

