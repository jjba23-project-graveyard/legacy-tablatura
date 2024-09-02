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
module Handler.ProfileSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Profile page" $ do
        it "asserts no access to my-account for anonymous users" $ do
            get ProfileR
            statusIs 403

        it "asserts access to my-account for authenticated users" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity

            get ProfileR
            statusIs 200

        it "asserts user's information is shown" $ do
            userEntity <- createUser "bar"
            authenticateAs userEntity

            get ProfileR
            let (Entity _ user) = userEntity
            htmlAnyContain ".username" . unpack $ userIdent user
