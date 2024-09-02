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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Artist where

import Application.TimeHandling
import Domain.Artist
import Domain.Sorting
import Domain.Tab
import Import
import Repositories.MongoArtistRepository
import Repositories.MongoGenreRepository
import Repositories.MongoTabRepository
import Repositories.Repositories
import Text.Julius (juliusFile)
import Views.ArtistViews

myArtistRepository :: ArtistRepository
myArtistRepository = mongoArtistRepository

renderArtistList :: NonEmpty (Entity Artist) -> Text -> Handler Html
renderArtistList rawAllArtists title = do
  let allArtists = map entityVal rawAllArtists

  defaultLayout $ do
    let artistCardSectionList = $(widgetFile "artist/artist_card")
    setTitle . toHtml $ title
    $(widgetFile "artist/index")
  where
    importJsFile = $(juliusFile "templates/js/import.julius")

getArtistListR :: Handler Html
getArtistListR = do
  getParameters <- reqGetParams <$> getRequest

  let orientationMaybe = lookup (fromString $ show ArtistListOrientation) getParameters
      sortByMaybe = lookup (fromString $ show ArtistListSortBy) getParameters

  resultSet <-
    runDB $
      getArtistList
        myArtistRepository
        (parseOrientationTypeMaybe orientationMaybe)
        (parseArtistSortByTypeMaybe sortByMaybe)

  case nonEmpty resultSet of
    Nothing -> emptyResultsHandler
    Just nonEmptyResultSet -> do
      renderArtistList
        nonEmptyResultSet
        "Artists | Tablatura, for all your music"

getArtistMyContributionR :: Handler Html
getArtistMyContributionR = do
  user <- requireAuth
  getParameters <- reqGetParams <$> getRequest

  let orientationMaybe = lookup (fromString $ show ArtistListOrientation) getParameters
      sortByMaybe = lookup (fromString $ show ArtistListSortBy) getParameters

  resultSet <-
    runDB $
      getMyContributionArtistList
        myArtistRepository
        (userIdent . entityVal $ user)
        (parseOrientationTypeMaybe orientationMaybe)
        (parseArtistSortByTypeMaybe sortByMaybe)

  case nonEmpty resultSet of
    Nothing -> emptyResultsHandler
    Just nonEmptyResultSet -> do
      renderArtistList
        nonEmptyResultSet
        "Artists | Tablatura, for all your music"

getArtistNonApprovedR :: Handler Html
getArtistNonApprovedR = do
  getParameters <- reqGetParams <$> getRequest

  let orientationMaybe = lookup (fromString $ show ArtistListOrientation) getParameters
      sortByMaybe = lookup (fromString $ show ArtistListSortBy) getParameters

  resultSet <-
    runDB $
      getNonApprovedArtistList
        myArtistRepository
        (parseOrientationTypeMaybe orientationMaybe)
        (parseArtistSortByTypeMaybe sortByMaybe)

  case nonEmpty resultSet of
    Nothing -> emptyResultsHandler
    Just nonEmptyResultSet -> do
      renderArtistList
        nonEmptyResultSet
        "Artists | Tablatura, for all your music"

getArtistR :: Text -> Handler Html
getArtistR slug = do
  maybeArtist <- runDB (maybeGetArtist myArtistRepository slug)
  case maybeArtist of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      getParameters <- reqGetParams <$> getRequest

      let orientationMaybe = lookup (fromString $ show TabListOrientation) getParameters
          sortByMaybe = lookup (fromString $ show TabListSortBy) getParameters
          pageNumberMaybe = lookup (fromString $ show TabListPageNumber) getParameters
          artist = entityVal entity

      _ <- runDB $ increaseViewCounterArtist myArtistRepository artist

      rawTabsForArtist <-
        runDB $
          getTabListBySearchKeys
            mongoTabRepository
            (artistTabs artist)
            (parseTabPageNumberMaybe pageNumberMaybe)
            (parseOrientationTypeMaybe orientationMaybe)
            (parseSortByTypeMaybe sortByMaybe)

      let allTabs = map entityVal rawTabsForArtist

      rawAllGenresOfArtist <-
        maybe
          (pure [])
          getGenreListForArtist
          (nonEmpty $ artistGenres artist)
      let allGenresOfArtist = map entityVal rawAllGenresOfArtist

      (userId, isUserSuperUser) <- fetchOwnerAuthStatusMaybe

      defaultLayout $ do
        let tabCardSectionList = $(widgetFile "tab/tab_card")
        setTitle $ toHtml $ buildTitleForArtistPage artist
        $(widgetFile "artist/detail")
  where
    importJsFile = $(juliusFile "templates/js/import.julius")
    tabSortParams = $(widgetFile "tab/tab_sort_params")
    buildTitleForArtistPage artist =
      artistName artist
        ++ " | Tablatura, for all your music"
    getGenreListForArtist nonE = runDB $ getGenreListBySlug mongoGenreRepository nonE

getArtistEditR :: Text -> Handler Html
getArtistEditR slug = do
  maybeArtist <- runDB (maybeGetArtist myArtistRepository slug)
  maybe resourceNotFoundHandler loadArtistEditForm maybeArtist
  where
    importJsFile = $(juliusFile "templates/js/import.julius")
    customJsFile = $(juliusFile "templates/artist/edit_page.julius")
    buildTitleForEditPage artist = artistName artist ++ " | Tablatura, for all your music"
    loadArtistEditForm entity = do
      let artist = entityVal entity

      (renderedArtistEditForm, enctype) <- generateFormPost $ artistEditForm artist

      defaultLayout $ do
        setTitle $ toHtml $ buildTitleForEditPage artist
        $(widgetFile "artist/edit_page")

postArtistDeleteR :: Text -> Handler Html
postArtistDeleteR slug = do
  _ <- runDB $ deleteArtist myArtistRepository slug
  redirect ArtistListR

postArtistEditR :: Text -> Handler Html
postArtistEditR slug = do
  maybeArtist <- runDB (maybeGetArtist myArtistRepository slug)
  maybe resourceNotFoundHandler performArtistEdit maybeArtist
  where
    performArtistEdit :: Entity Artist -> Handler Html
    performArtistEdit entity = do
      let artist = entityVal entity
      ((result, _), _) <- runFormPost $ artistEditForm artist

      case result of
        FormSuccess updatedData -> do
          _ <- runDB $ updateArtist myArtistRepository artist updatedData
          -- TODO: update genre with updated artist if present
          redirect $ ArtistR slug
        FormFailure _ -> redirectWith status422 ArtistListR
        FormMissing -> redirectWith status422 ArtistListR

getArtistNewR :: Handler Html
getArtistNewR = do
  (renderedContributeForm, enctype) <- generateFormPost artistNewForm
  defaultLayout $ do
    setTitle "Tablatura"
    $(widgetFile "artist/new")
  where
    importJsFile = $(juliusFile "templates/js/import.julius")
    customJsFile = $(juliusFile "templates/artist/new.julius")

postArtistNewR :: Handler Html
postArtistNewR = do
  user <- requireAuth

  ((result, _), _) <- runFormPost artistNewForm
  case result of
    FormSuccess contribution -> do
      _ <-
        runDB $
          insertArtist
            myArtistRepository
            (userIdent . entityVal $ user)
            contribution
      -- TODO: update genre with new artist if present
      redirect ArtistMyContributionR
    FormFailure _ -> redirectWith status422 ArtistListR
    FormMissing -> redirectWith status422 ArtistListR

postArtistGrantApprovalR :: Text -> Handler Html
postArtistGrantApprovalR slug = do
  maybeArtist <- runDB (maybeGetArtist myArtistRepository slug)
  user <- requireAuth
  maybe resourceNotFoundHandler (performApprovalGrant user) maybeArtist
  where
    performApprovalGrant :: Entity User -> Entity Artist -> Handler Html
    performApprovalGrant user entity = do
      let artist = entityVal entity
      _ <-
        runDB $
          grantApprovalArtist
            myArtistRepository
            artist
            (userIdent . entityVal $ user)
      redirect ArtistListR

postArtistRevokeApprovalR :: Text -> Handler Html
postArtistRevokeApprovalR slug = do
  maybeArtist <- runDB $ maybeGetArtist myArtistRepository slug
  maybe resourceNotFoundHandler performApprovalRevoke maybeArtist
  where
    performApprovalRevoke :: Entity Artist -> Handler Html
    performApprovalRevoke entity = do
      let artist = entityVal entity
      _ <- runDB $ revokeApprovalArtist myArtistRepository artist
      redirect ArtistListR

postArtistLikeR :: Text -> Handler Html
postArtistLikeR slug = do
  maybeArtist <- runDB (maybeGetArtist myArtistRepository slug)
  user <- requireAuth
  maybe resourceNotFoundHandler (performAction user) maybeArtist
  where
    performAction user entity = do
      let artist = entityVal entity
      _ <-
        runDB $
          likeArtist
            myArtistRepository
            artist
            (userIdent . entityVal $ user)
      redirect $ ArtistR slug

postArtistDislikeR :: Text -> Handler Html
postArtistDislikeR slug = do
  maybeArtist <- runDB (maybeGetArtist myArtistRepository slug)
  user <- requireAuth
  maybe resourceNotFoundHandler (performAction user) maybeArtist
  where
    performAction user entity = do
      let artist = entityVal entity
      _ <-
        runDB $
          dislikeArtist
            myArtistRepository
            artist
            (userIdent . entityVal $ user)
      redirect $ ArtistR slug
