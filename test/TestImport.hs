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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import           Application                 (makeFoundation, makeLogWare)
import           ClassyPrelude               as X hiding (Handler, delete,
                                                   deleteBy)
import           Database.Persist            as X hiding (get)
import           Database.Persist.MongoDB    hiding (master)
import           Foundation                  as X
import           Model                       as X
import           Settings                    (appDatabaseConf)
import           Test.Hspec                  as X
import           Yesod.Auth                  as X
import           Yesod.Default.Config2       (loadYamlSettings, useEnv)
import           Yesod.Test                  as X
-- Wiping the test database
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Database.MongoDB.Admin      (dropCollection)
import           Database.MongoDB.Query      (allCollections)

import           Yesod.Core.Unsafe           (fakeHandlerGetLogger)

runDB :: Action IO a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> Action IO a -> IO a
runDBWithApp app query = do
    liftIO $ runMongoDBPool
        (mgAccessMode $ appDatabaseConf $ appSettings app)
        query
        (appConnPool app)

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will wipe your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = void $ runDBWithApp app dropAllCollections

dropAllCollections :: (MonadIO m, MonadBaseControl IO m) => Action m [Bool]
dropAllCollections = allCollections <&> filter (not . isSystemCollection)
      where
        isSystemCollection = isPrefixOf "system."

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.  The dummy email entry helps to confirm that foreign-key
-- checking is switched off in wipeDB for those database backends which need it.
createUser :: Text -> YesodExample App (Entity User)
createUser ident = runDB $ do
    user <- insertEntity User
        { userIdent = ident
        , userPassword = Nothing
        }
    _ <- insert Email
        { emailEmail = ident
        , emailUserId = Just $ entityKey user
        , emailVerkey = Nothing
        }
    return user
