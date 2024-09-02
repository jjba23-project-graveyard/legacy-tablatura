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

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation where

-- Used only when in "auth-dummy-login" setting is enabled.

import           Control.Monad.Logger               (LogSource)
import           Database.Persist.MongoDB           hiding (master)
import           Domain.Tab                         (tabArtistsURLToList)
import           Import.NoFoundation
import           Repositories.MongoArtistRepository
import           Repositories.MongoGenreRepository
import           Repositories.MongoTabRepository
import           Repositories.Repositories
import           Text.Hamlet                        (hamletFile)
import           Text.Jasmine                       (minifym)
import           Text.Julius
import           Yesod.Auth.OAuth2.Google
import           Yesod.Core.Types                   (Logger)
import qualified Yesod.Core.Unsafe                  as Unsafe
import           Yesod.Default.Util                 (addStaticContentExternal)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings    :: AppSettings,
    -- | Settings for static file serving.
    appStatic      :: Static,
    -- | Database connection pool.
    appConnPool    :: ConnectionPool,
    appHttpManager :: Manager,
    appLogger      :: Logger
  }

data MenuItem = MenuItem
  { menuItemLabel          :: Text,
    menuItemRoute          :: Route App,
    menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing   -> getApprootText guessApproot app req
      Just root -> root

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ =
    Just
      <$> defaultClientSessionBackend
        120 -- timeout in minutes
        "config/client_session_key.aes"

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware = defaultYesodMiddleware

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage

    muser <- maybeAuthPair
    mcurrentRoute <- getCurrentRoute

    -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
    (title, parents) <- breadcrumbs

    -- Define the menu items of the header.
    let menuItems =
          [ NavbarLeft $
              MenuItem
                { menuItemLabel = "Songs",
                  menuItemRoute = HomeR,
                  menuItemAccessCallback = True
                },
            -- NavbarLeft $
            --   MenuItem
            --     { menuItemLabel = "Artists",
            --       menuItemRoute = ArtistsR,
            --       menuItemAccessCallback = True
            --     },
            NavbarLeft $
              MenuItem
                { menuItemLabel = "Genres",
                  menuItemRoute = GenreListR,
                  menuItemAccessCallback = True
                },
            NavbarLeft $
              MenuItem
                { menuItemLabel = "Artists",
                  menuItemRoute = ArtistListR,
                  menuItemAccessCallback = True
                },
            NavbarLeft $
              MenuItem
                { menuItemLabel = "Profile",
                  menuItemRoute = ProfileR,
                  menuItemAccessCallback = isJust muser
                },
            NavbarRight $
              MenuItem
                { menuItemLabel = "Login",
                  menuItemRoute = AuthR LoginR,
                  menuItemAccessCallback = isNothing muser
                },
            -- NavbarRight $
            --   MenuItem
            --     { menuItemLabel = "Logout",
            --       menuItemRoute = AuthR LogoutR,
            --       menuItemAccessCallback = isJust muser
            --     },
            NavbarRight $
              MenuItem
                { menuItemLabel = "Contribute",
                  menuItemRoute = ContributeR,
                  menuItemAccessCallback = isJust muser
                }
          ]

    let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
    let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

    let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR css_tablatura_css
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute ::
    App ->
    Maybe (Route App)
  authRoute _ = Just $ AuthR LoginR

  isAuthorized ::
    -- \| The route the user is visiting.
    Route App ->
    -- \| Whether or not this is a "write" request.
    Bool ->
    Handler AuthResult
  -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized HomeR _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized (StaticR _) _ = return Authorized
  isAuthorized ArtistListR _ = return Authorized
  isAuthorized ArtistR {} _ = return Authorized
  isAuthorized GenreListR _ = return Authorized
  isAuthorized GenreR {} _ = return Authorized
  isAuthorized TabR {} _ = return Authorized
  isAuthorized TabSearchR {} _ = return Authorized
  isAuthorized PrivacyStatementR _ = return Authorized
  isAuthorized LegalStatementR _ = return Authorized
  isAuthorized LicenseStatementR _ = return Authorized
  isAuthorized UILabsR _ = return Authorized
  -- Routes requiring authentication
  isAuthorized ProfileR _ = isAuthenticated
  isAuthorized ContributeR _ = isAuthenticated
  isAuthorized TabMyContributionR {} _ = isAuthenticated
  isAuthorized GenreMyContributionR {} _ = isAuthenticated
  isAuthorized ArtistMyContributionR {} _ = isAuthenticated
  isAuthorized TabNewR _ = isAuthenticated
  isAuthorized ArtistNewR _ = isAuthenticated
  isAuthorized GenreNewR _ = isAuthenticated
  isAuthorized TabLikeR {} _ = isAuthenticated
  isAuthorized ArtistLikeR {} _ = isAuthenticated
  isAuthorized GenreLikeR {} _ = isAuthenticated
  isAuthorized TabDislikeR {} _ = isAuthenticated
  isAuthorized ArtistDislikeR {} _ = isAuthenticated
  isAuthorized GenreDislikeR {} _ = isAuthenticated
  -- Routes requiring the resource owner or super user permission
  isAuthorized (GenreEditR name) _ = isAuthResourceOwner $ GenreEditR name
  isAuthorized (GenreDeleteR name) _ = isAuthResourceOwner $ GenreDeleteR name
  isAuthorized (ArtistEditR name) _ = isAuthResourceOwner $ ArtistEditR name
  isAuthorized (ArtistDeleteR name) _ = isAuthResourceOwner $ ArtistDeleteR name
  isAuthorized (TabEditR art sng vrs) _ = isAuthResourceOwner $ TabEditR art sng vrs
  isAuthorized (TabDeleteR art sng vrs) _ = isAuthResourceOwner $ TabDeleteR art sng vrs
  -- Routes requiring super user status
  isAuthorized TabNonApprovedR _ = isAuthSuperUser
  isAuthorized ArtistNonApprovedR _ = isAuthSuperUser
  isAuthorized GenreNonApprovedR _ = isAuthSuperUser
  isAuthorized TabGrantApprovalR {} _ = isAuthSuperUser
  isAuthorized TabRevokeApprovalR {} _ = isAuthSuperUser
  isAuthorized GenreGrantApprovalR {} _ = isAuthSuperUser
  isAuthorized GenreRevokeApprovalR {} _ = isAuthSuperUser
  isAuthorized ArtistGrantApprovalR {} _ = isAuthSuperUser
  isAuthorized ArtistRevokeApprovalR {} _ = isAuthSuperUser

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ::
    -- \| The file extension
    Text ->
    -- \| The MIME content type
    Text ->
    -- \| The contents of the file
    LByteString ->
    Handler (Maybe (Either Text (Route App, [(Text, Text)])))
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return $
      appShouldLogAll (appSettings app)
        || level == LevelWarn
        || level == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  -- Takes the route that the user is currently on, and returns a tuple
  -- of the 'Text' that you want the label to display, and a previous
  -- breadcrumb route.
  breadcrumb ::
    -- \| The route the user is visiting currently.
    Route App ->
    Handler (Text, Maybe (Route App))
  breadcrumb HomeR     = return ("Home", Nothing)
  breadcrumb (AuthR _) = return ("Login", Just HomeR)
  breadcrumb ProfileR  = return ("Profile", Just HomeR)
  breadcrumb _         = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = MongoContext
  runDB :: ReaderT MongoContext Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runMongoDBPool
      (mgAccessMode $ appDatabaseConf $ appSettings master)
      action
      (appConnPool master)

instance YesodAuth App where
  type AuthId App = UserId

  -- Where to send a user after successful login
  loginDest :: App -> Route App
  loginDest _ = HomeR

  -- Where to send a user after logout
  logoutDest :: App -> Route App
  logoutDest _ = HomeR

  -- Override the above two destinations when a Referer: header is present
  redirectToReferer :: App -> Bool
  redirectToReferer _ = True

  authenticate ::
    (MonadHandler m, HandlerSite m ~ App) =>
    Creds App ->
    m (AuthenticationResult App)
  authenticate creds = liftHandler $
    runDB $ do
      x <- getBy $ UniqueUser $ credsIdent creds
      case x of
        Just (Entity uid _) -> return $ Authenticated uid
        Nothing ->
          Authenticated
            <$> insert
              User
                { userIdent = credsIdent creds,
                  userPassword = Nothing
                }

  -- You can add other plugins like Google Email, email or OAuth here
  authPlugins :: App -> [AuthPlugin App]
  authPlugins app =
    [ oauth2GoogleScoped ["email", "profile", "openid"] (appGoogleOAuthClientId $ appSettings app) (appGoogleOAuthClientSecret $ appSettings app)
    ]

  loginHandler = myLoginHandler

myLoginHandler :: AuthHandler master Html
myLoginHandler = do
  -- tp <- getRouteToParent
  liftHandler . defaultLayout $ do
    -- setTitle $ fromText ""
    -- master <- getYesod
    -- mapM_ (flip apLogin tp) (authPlugins master)
    let importJsFile = $(juliusFile "templates/js/import.julius")
    $(widgetFile "login/index")

resourceNotFoundHandler :: Handler Html
resourceNotFoundHandler = do
  defaultLayout $ do
    setTitle $ "Resource not found!"
    $(widgetFile "statuses/not-found")

emptyResultsHandler :: Handler Html
emptyResultsHandler = do
  defaultLayout $ do
    setTitle $ "Empty result set!"
    $(widgetFile "statuses/empty-results")

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
  userMaybe <- maybeAuth
  if isNothing userMaybe
    then pure $ Unauthorized "You must login to access this page"
    else pure Authorized

isAuthSuperUser :: Handler AuthResult
isAuthSuperUser = do
  (_, isUserSuperUser) <- fetchOwnerAuthStatus

  if isUserSuperUser
    then pure Authorized
    else pure $ Unauthorized "Only super users are allowed to perform this action!"

ifOwnerThenAuthorized :: Text -> Text -> AuthResult
ifOwnerThenAuthorized userId compareId =
  if userId == compareId then Authorized else Unauthorized "You are not allowed to edit resources that are not yours!"

ifOwnerThenAuthorizedMay :: Text -> Maybe Text -> AuthResult
ifOwnerThenAuthorizedMay userId compareId = do
  case compareId of
    Nothing -> Unauthorized "You are not allowed to edit resources that are not yours!"
    Just compareId' ->
      if userId == compareId'
        then Authorized
        else Unauthorized "You are not allowed to edit resources that are not yours!"

ifSuperUserOrOwnerThenAuthorized :: Text -> Maybe Text -> Bool -> AuthResult
ifSuperUserOrOwnerThenAuthorized userId compareId isUserSuperUser =
  if isUserSuperUser
    then Authorized
    else do
      ifOwnerThenAuthorizedMay userId compareId

fetchOwnerAuthStatus :: HandlerFor App (Text, Bool)
fetchOwnerAuthStatus = do
  user <- requireAuth
  app <- getYesod
  let isUserSuperUser = isInSuperUserList user (superUsers . appSettings $ app)
      userId = userIdent . entityVal $ user
  pure (userId, isUserSuperUser)

fetchOwnerAuthStatusMaybe :: HandlerFor App (Maybe Text, Bool)
fetchOwnerAuthStatusMaybe = do
  user <- maybeAuth
  app <- getYesod
  case user of
    Nothing -> pure (Nothing, False)
    Just x -> do
      let isUserSuperUser = isInSuperUserList x (superUsers . appSettings $ app)
      let userId = userIdent . entityVal $ x
      pure (Just userId, isUserSuperUser)

isInSuperUserList :: Entity User -> [Text] -> Bool
isInSuperUserList userEntity superUserList = do
  (userIdent . entityVal $ userEntity) `elem` superUserList

isAuthResourceOwner :: Route App -> Handler AuthResult
isAuthResourceOwner route = do
  (userId, isUserSuperUser) <- fetchOwnerAuthStatus
  case route of
    (TabEditR artists song version) -> do
      author <- runDB $ maybeGetCreatorOfTab mongoTabRepository (TabSearchKey (tabArtistsURLToList artists) song version)
      pure $ ifSuperUserOrOwnerThenAuthorized userId author isUserSuperUser
    (TabDeleteR artists song version) -> do
      author <- runDB $ maybeGetCreatorOfTab mongoTabRepository (TabSearchKey (tabArtistsURLToList artists) song version)
      pure $ ifSuperUserOrOwnerThenAuthorized userId author isUserSuperUser
    (GenreEditR name) -> do
      author <- runDB $ maybeGetCreatorOfGenre mongoGenreRepository name
      pure $ ifSuperUserOrOwnerThenAuthorized userId author isUserSuperUser
    (GenreDeleteR name) -> do
      author <- runDB $ maybeGetCreatorOfGenre mongoGenreRepository name
      pure $ ifSuperUserOrOwnerThenAuthorized userId author isUserSuperUser
    (ArtistEditR name) -> do
      author <- runDB $ maybeGetCreatorOfArtist mongoArtistRepository name
      pure $ ifSuperUserOrOwnerThenAuthorized userId author isUserSuperUser
    (ArtistDeleteR name) -> do
      author <- runDB $ maybeGetCreatorOfArtist mongoArtistRepository name
      pure $ ifSuperUserOrOwnerThenAuthorized userId author isUserSuperUser
    _ -> pure $ Unauthorized "Attempting to access restricted resource!"

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
