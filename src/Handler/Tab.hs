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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Tab where

import Application.TimeHandling
import Data.Text (strip)
import Domain.Sorting
import Domain.Tab
import Import
import Repositories.MongoGenreRepository
import Repositories.MongoTabRepository
import Repositories.Repositories
import Text.Julius (juliusFile)
import Views.TabViews

myGenreRepository :: GenreRepository
myGenreRepository = mongoGenreRepository

myTabRepository :: TabRepository
myTabRepository = mongoTabRepository

getTabR :: Text -> Text -> Int -> Handler Html
getTabR artistSlg tabSlg versionSlg = do
  maybeTab <- runDB $ maybeGetTab myTabRepository softId
  case maybeTab of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      let tab = entityVal entity

      _ <- runDB $ increaseViewCounterTab myTabRepository tab

      rawAllGenresOfTab <-
        maybe
          (pure [])
          (\genres -> runDB $ getGenreListBySlug myGenreRepository genres)
          (nonEmpty $ tabGenres tab)
      let allGenresOfTab = map entityVal rawAllGenresOfTab

      (userId, isUserSuperUser) <- fetchOwnerAuthStatusMaybe
      defaultLayout $ do
        setTitle $ toHtml $ buildTitleForTabPage tab
        $(widgetFile "tab/index")
  where
    importJsFile = $(juliusFile "templates/js/import.julius")
    buildTitleForTabPage tab =
      (tabArtistsListToHumanReadable . tabArtistNames $ tab)
        ++ " - "
        ++ tabSongName tab
        ++ " | Tablatura, for all your music"
    softId = TabSearchKey (tabArtistsURLToList artistSlg) tabSlg versionSlg

getTabEditR :: Text -> Text -> Int -> Handler Html
getTabEditR artistSlg tabSlg versionSlg = do
  maybeTab <- runDB $ maybeGetTab myTabRepository softId
  case maybeTab of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      let tab = entityVal entity

      (renderedTabEditForm, enctype) <- generateFormPost $ tabEditForm tab
      defaultLayout $ do
        setTitle $ toHtml $ buildTitleForEditPage tab
        $(widgetFile "tab/edit_page")
  where
    importJsFile = $(juliusFile "templates/js/import.julius")
    customJsFile = $(juliusFile "templates/tab/edit_page.julius")
    buildTitleForEditPage tab =
      "Editing "
        ++ (tabArtistsListToHumanReadable . tabArtistNames $ tab)
        ++ " - "
        ++ tabSongName tab
        ++ " | Tablatura, for all your music"
    softId = TabSearchKey (tabArtistsURLToList artistSlg) tabSlg versionSlg

postTabDeleteR :: Text -> Text -> Int -> Handler Html
postTabDeleteR artistSlg tabSlg versionSlg = do
  _ <- runDB $ deleteTab myTabRepository softId
  redirect HomeR
  where
    softId = TabSearchKey (tabArtistsURLToList artistSlg) tabSlg versionSlg

postTabEditR :: Text -> Text -> Int -> Handler Html
postTabEditR artistSlg tabSlg versionSlg = do
  maybeTab <- runDB $ maybeGetTab myTabRepository softId
  case maybeTab of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      let tab = entityVal entity

      ((result, _), _) <- runFormPost $ tabEditForm tab

      case result of
        FormSuccess updatedData -> do
          -- update tab
          _ <- runDB $ updateTab myTabRepository tab updatedData
          -- update genre with association to tab
          tabUpdates <-
            mapM
              (\x -> runDB $ updateTabsOfGenre myGenreRepository softId x)
              (maybe [] (map strip . lines . unTextarea) (tabEditGenre updatedData))
          redirect $ TabR artistSlg tabSlg versionSlg
        FormFailure _ -> redirectWith status422 HomeR
        FormMissing -> redirectWith status422 HomeR
  where
    softId = TabSearchKey (tabArtistsURLToList artistSlg) tabSlg versionSlg

postTabRevokeApprovalR :: Text -> Text -> Int -> Handler Html
postTabRevokeApprovalR artistSlg tabSlg versionSlg = do
  maybeTab <- runDB $ maybeGetTab myTabRepository softId
  case maybeTab of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      let tab = entityVal entity

      _ <- runDB $ revokeApprovalTab myTabRepository tab
      redirect HomeR
  where
    softId = TabSearchKey (tabArtistsURLToList artistSlg) tabSlg versionSlg

postTabGrantApprovalR :: Text -> Text -> Int -> Handler Html
postTabGrantApprovalR artistSlg tabSlg versionSlg = do
  user <- requireAuth
  maybeTab <- runDB $ maybeGetTab myTabRepository softId
  case maybeTab of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      let tab = entityVal entity

      _ <- runDB $ grantApprovalTab myTabRepository tab (userIdent . entityVal $ user)
      redirect HomeR
  where
    softId = TabSearchKey (tabArtistsURLToList artistSlg) tabSlg versionSlg

postTabLikeR :: Text -> Text -> Int -> Handler Html
postTabLikeR artistSlg tabSlg versionSlg = do
  user <- requireAuth
  maybeTab <- runDB $ maybeGetTab myTabRepository softId
  case maybeTab of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      let tab = entityVal entity

      _ <- runDB $ likeTab myTabRepository tab (userIdent . entityVal $ user)
      redirect $ TabR artistSlg tabSlg versionSlg
  where
    softId = TabSearchKey (tabArtistsURLToList artistSlg) tabSlg versionSlg

postTabDislikeR :: Text -> Text -> Int -> Handler Html
postTabDislikeR artistSlg tabSlg versionSlg = do
  user <- requireAuth
  maybeTab <- runDB $ maybeGetTab myTabRepository softId
  case maybeTab of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      let tab = entityVal entity

      _ <- runDB $ dislikeTab myTabRepository tab (userIdent . entityVal $ user)
      redirect $ TabR artistSlg tabSlg versionSlg
  where
    softId = TabSearchKey (tabArtistsURLToList artistSlg) tabSlg versionSlg

getTabNewR :: Handler Html
getTabNewR = do
  (renderedContributeForm, enctype) <-
    generateFormPost tabNewForm
  defaultLayout $ do
    setTitle "Tablatura"
    $(widgetFile "tab/new")
  where
    importJsFile = $(juliusFile "templates/js/import.julius")

postTabNewR :: Handler Html
postTabNewR = do
  user <- requireAuth
  ((result, _), _) <- runFormPost tabNewForm

  case result of
    FormFailure _ -> redirectWith status422 HomeR
    FormMissing -> redirectWith status422 HomeR
    FormSuccess contribution -> do
      -- insert tab
      insertResult <- runDB $ insertTab myTabRepository (userIdent . entityVal $ user) contribution
      case insertResult of
        Left a -> error $ "Attempting to insert record that already exists, please increase the version number: " ++ show a
        Right _ -> do
          -- update genre with association to tab
          _ <-
            mapM
              ( \x -> do
                  runDB $ updateTabsOfGenre myGenreRepository (extractSearchKeyFromTabContribution contribution) x
              )
              (maybe [] (map strip . lines . unTextarea) (tabInsertGenre contribution))
          redirect TabMyContributionR
  where
    extractSearchKeyFromTabContribution x =
      TabSearchKey
        (lines . slugify . tabInsertArtistNames $ x)
        (slugify . tabInsertSongName $ x)
        (tabInsertVersion x)

getHomeR :: Handler Html
getHomeR = do
  getParameters <- reqGetParams <$> getRequest
  let orientationMaybe = lookup (fromString $ show TabListOrientation) getParameters
      sortByMaybe = lookup (fromString $ show TabListSortBy) getParameters
      pageNumberMaybe = lookup (fromString $ show TabListPageNumber) getParameters

  resultSet <-
    runDB $
      getTabList
        myTabRepository
        (parseTabPageNumberMaybe pageNumberMaybe)
        (parseOrientationTypeMaybe orientationMaybe)
        (parseSortByTypeMaybe sortByMaybe)

  case nonEmpty resultSet of
    Nothing -> emptyResultsHandler
    Just nonEmptyResultSet ->
      renderTabList
        nonEmptyResultSet
        ( fromString $
            getTitleFromParams
              (parseOrientationTypeMaybe orientationMaybe)
              (parseSortByTypeMaybe sortByMaybe)
        )

renderTabList :: NonEmpty (Entity Tab) -> Text -> Handler Html
renderTabList rawTabs title = do
  defaultLayout $ do
    setTitle . toHtml $ title
    let allTabs = map entityVal rawTabs
        tabCardSectionList = $(widgetFile "tab/tab_card")

    $(widgetFile "homepage/index")
  where
    importJsFile = $(juliusFile "templates/js/import.julius")
    tabSortParams = $(widgetFile "tab/tab_sort_params")

getTabNonApprovedR :: Handler Html
getTabNonApprovedR = do
  getParameters <- reqGetParams <$> getRequest
  let orientationMaybe = lookup (fromString $ show TabListOrientation) getParameters
      sortByMaybe = lookup (fromString $ show TabListSortBy) getParameters
      pageNumberMaybe = lookup (fromString $ show TabListPageNumber) getParameters

  resultSet <-
    runDB $
      getNonApprovedTabList
        myTabRepository
        (parseTabPageNumberMaybe pageNumberMaybe)
        (parseOrientationTypeMaybe orientationMaybe)
        (parseSortByTypeMaybe sortByMaybe)

  case nonEmpty resultSet of
    Nothing -> emptyResultsHandler
    Just nonEmptyResultSet -> do
      renderTabList
        nonEmptyResultSet
        ( fromString $
            getTitleFromParams
              (parseOrientationTypeMaybe orientationMaybe)
              (parseSortByTypeMaybe sortByMaybe)
        )

getTabMyContributionR :: Handler Html
getTabMyContributionR = do
  user <- requireAuth

  getParameters <- reqGetParams <$> getRequest
  let orientationMaybe = lookup (fromString $ show TabListOrientation) getParameters
      sortByMaybe = lookup (fromString $ show TabListSortBy) getParameters
      pageNumberMaybe = lookup (fromString $ show TabListPageNumber) getParameters

  resultSet <-
    runDB $
      getMyContributionTabList
        myTabRepository
        (userIdent . entityVal $ user)
        (parseTabPageNumberMaybe pageNumberMaybe)
        (parseOrientationTypeMaybe orientationMaybe)
        (parseSortByTypeMaybe sortByMaybe)

  case nonEmpty resultSet of
    Nothing -> emptyResultsHandler
    Just nonEmptyResultSet -> do
      renderTabList
        nonEmptyResultSet
        ( fromString $
            getTitleFromParams
              (parseOrientationTypeMaybe orientationMaybe)
              (parseSortByTypeMaybe sortByMaybe)
        )

getTabSearchR :: Text -> Handler Html
getTabSearchR query = do
  getParameters <- reqGetParams <$> getRequest
  let orientationMaybe = lookup (fromString $ show TabListOrientation) getParameters
      sortByMaybe = lookup (fromString $ show TabListSortBy) getParameters
      pageNumberMaybe = lookup (fromString $ show TabListPageNumber) getParameters

  resultSet <-
    runDB $
      searchForTab
        myTabRepository
        (slugify query)
        (parseTabPageNumberMaybe pageNumberMaybe)
        (parseOrientationTypeMaybe orientationMaybe)
        (parseSortByTypeMaybe sortByMaybe)

  case nonEmpty resultSet of
    Nothing -> emptyResultsHandler
    Just nonEmptyResultSet ->
      renderTabList
        nonEmptyResultSet
        ( fromString $
            getTitleFromParams
              (parseOrientationTypeMaybe orientationMaybe)
              (parseSortByTypeMaybe sortByMaybe)
        )
