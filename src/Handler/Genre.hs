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

module Handler.Genre where

import Application.TimeHandling
import Domain.Genre
import Domain.Sorting
import Domain.Tab
import Import
import Repositories.MongoArtistRepository (mongoArtistRepository)
import Repositories.MongoGenreRepository
import Repositories.MongoTabRepository (mongoTabRepository)
import Repositories.Repositories
import Text.Julius (juliusFile)
import Views.GenreViews

myGenreRepository :: GenreRepository
myGenreRepository = mongoGenreRepository

myTabRepository :: TabRepository
myTabRepository = mongoTabRepository

myArtistRepository :: ArtistRepository
myArtistRepository = mongoArtistRepository

renderGenreList :: NonEmpty (Entity Genre) -> Text -> Handler Html
renderGenreList rawAllGenres title = do
  let allGenres = map entityVal rawAllGenres

  defaultLayout $ do
    setTitle . toHtml $ title
    $(widgetFile "genre/index")
  where
    importJsFile = $(juliusFile "templates/js/import.julius")

getGenreListR :: Handler Html
getGenreListR = do
  getParameters <- reqGetParams <$> getRequest
  let orientationMaybe = lookup (fromString $ show GenreListOrientation) getParameters
      sortByMaybe = lookup (fromString $ show GenreListSortBy) getParameters

  resultSet <-
    runDB $
      getGenreList
        myGenreRepository
        (parseOrientationTypeMaybe orientationMaybe)
        (parseGenreSortByTypeMaybe sortByMaybe)

  case nonEmpty resultSet of
    Nothing -> emptyResultsHandler
    Just nonEmptyResultSet ->
      renderGenreList
        nonEmptyResultSet
        "Genres | Tablatura, for all your music"

getGenreMyContributionR :: Handler Html
getGenreMyContributionR = do
  user <- requireAuth

  getParameters <- reqGetParams <$> getRequest
  let orientationMaybe = lookup (fromString $ show GenreListOrientation) getParameters
      sortByMaybe = lookup (fromString $ show GenreListSortBy) getParameters

  resultSet <-
    runDB $
      getMyContributionGenreList
        myGenreRepository
        (userIdent . entityVal $ user)
        (parseOrientationTypeMaybe orientationMaybe)
        (parseGenreSortByTypeMaybe sortByMaybe)

  case nonEmpty resultSet of
    Nothing -> emptyResultsHandler
    Just nonEmptyResultSet ->
      renderGenreList
        nonEmptyResultSet
        "Genres | Tablatura, for all your music"

getGenreNonApprovedR :: Handler Html
getGenreNonApprovedR = do
  getParameters <- reqGetParams <$> getRequest
  let orientationMaybe = lookup (fromString $ show GenreListOrientation) getParameters
      sortByMaybe = lookup (fromString $ show GenreListSortBy) getParameters

  resultSet <-
    runDB $
      getNonApprovedGenreList
        myGenreRepository
        (parseOrientationTypeMaybe orientationMaybe)
        (parseGenreSortByTypeMaybe sortByMaybe)

  case nonEmpty resultSet of
    Nothing -> emptyResultsHandler
    Just nonEmptyResultSet ->
      renderGenreList
        nonEmptyResultSet
        "Genres | Tablatura, for all your music"

getGenreR :: Text -> Handler Html
getGenreR slug = do
  maybeGenre <- runDB (maybeGetGenre myGenreRepository slug)
  case maybeGenre of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      getParameters <- reqGetParams <$> getRequest
      let orientationMaybe = lookup (fromString $ show TabListOrientation) getParameters
          sortByMaybe = lookup (fromString $ show TabListSortBy) getParameters
          pageNumberMaybe = lookup (fromString $ show TabListPageNumber) getParameters
          genre = entityVal entity

      _ <- runDB $ increaseViewCounterGenre myGenreRepository genre

      rawTabsForGenre <-
        runDB $
          getTabListBySearchKeys
            myTabRepository
            (genreTabs genre)
            (parseTabPageNumberMaybe pageNumberMaybe)
            (parseOrientationTypeMaybe orientationMaybe)
            (parseSortByTypeMaybe sortByMaybe)

      let allTabs = map entityVal rawTabsForGenre

      rawArtistsOfGenre <- case nonEmpty $ genreArtists genre of
        Nothing -> pure []
        Just theGenreArtists -> runDB $ getArtistListBySlug myArtistRepository theGenreArtists

      let allArtists = map entityVal rawArtistsOfGenre

      (userId, isUserSuperUser) <- fetchOwnerAuthStatusMaybe

      defaultLayout $ do
        setTitle $ toHtml $ buildTitleForGenrePage genre
        let tabCardSectionList = $(widgetFile "tab/tab_card")
            artistCardSectionList = $(widgetFile "artist/artist_card")
        $(widgetFile "genre/detail")
      where
        importJsFile = $(juliusFile "templates/js/import.julius")
        tabSortParams = $(widgetFile "tab/tab_sort_params")
        buildTitleForGenrePage genre =
          genreName genre
            ++ " | Tablatura, for all your music"

getGenreEditR :: Text -> Handler Html
getGenreEditR slug = do
  maybeGenre <- runDB (maybeGetGenre myGenreRepository slug)
  case maybeGenre of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      let genre = entityVal entity

      (renderedGenreEditForm, enctype) <- generateFormPost $ genreEditForm genre

      defaultLayout $ do
        setTitle $ toHtml $ buildTitleForEditPage genre
        $(widgetFile "genre/edit_page")
  where
    importJsFile = $(juliusFile "templates/js/import.julius")
    customJsFile = $(juliusFile "templates/genre/edit_page.julius")
    buildTitleForEditPage genre =
      genreName genre ++ " | Tablatura, for all your music"

postGenreDeleteR :: Text -> Handler Html
postGenreDeleteR slug = do
  _ <- runDB $ deleteGenre myGenreRepository slug
  redirect GenreListR

postGenreEditR :: Text -> Handler Html
postGenreEditR slug = do
  maybeGenre <- runDB (maybeGetGenre myGenreRepository slug)
  case maybeGenre of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      let genre = entityVal entity
      ((result, _), _) <- runFormPost $ genreEditForm genre

      case result of
        FormSuccess updatedData -> do
          -- update genre
          _ <- runDB $ updateGenre myGenreRepository genre updatedData
          -- update tab with association to genre
          case genreEditTabs updatedData of
            Nothing -> redirect GenreListR
            Just tabsToUpdate -> do
              _ <-
                mapM
                  (\x -> runDB $ updateGenresOfTabAssociation myTabRepository x slug)
                  (myTabsDecoder . Just $ tabsToUpdate)
              redirect $ GenreR slug
        -- TODO update artists with association to genre
        FormFailure _ -> redirectWith status422 GenreListR
        FormMissing -> redirectWith status422 GenreListR

postGenreGrantApprovalR :: Text -> Handler Html
postGenreGrantApprovalR slug = do
  user <- requireAuth
  maybeGenre <- runDB (maybeGetGenre myGenreRepository slug)
  case maybeGenre of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      let genre = entityVal entity

      _ <- runDB $ grantApprovalGenre myGenreRepository genre (userIdent . entityVal $ user)
      redirect GenreListR

postGenreRevokeApprovalR :: Text -> Handler Html
postGenreRevokeApprovalR slug = do
  maybeGenre <- runDB (maybeGetGenre myGenreRepository slug)
  case maybeGenre of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      let genre = entityVal entity
      _ <- runDB $ revokeApprovalGenre myGenreRepository genre
      redirect GenreListR

postGenreLikeR :: Text -> Handler Html
postGenreLikeR slug = do
  user <- requireAuth
  maybeGenre <- runDB (maybeGetGenre myGenreRepository slug)
  case maybeGenre of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      let genre = entityVal entity
      _ <- runDB $ likeGenre myGenreRepository genre (userIdent . entityVal $ user)
      redirect $ GenreR slug


postGenreDislikeR :: Text -> Handler Html
postGenreDislikeR slug = do
  user <- requireAuth
  maybeGenre <- runDB (maybeGetGenre myGenreRepository slug)
  case maybeGenre of
    Nothing -> resourceNotFoundHandler
    Just entity -> do
      let genre = entityVal entity
      _ <- runDB $ dislikeGenre myGenreRepository genre (userIdent . entityVal $ user)
      redirect $ GenreR slug

getGenreNewR :: Handler Html
getGenreNewR = do
  (renderedContributeForm, enctype) <- generateFormPost genreNewForm
  defaultLayout $ do
    setTitle "Tablatura"
    $(widgetFile "genre/new")
  where
    importJsFile = $(juliusFile "templates/js/import.julius")
    customJsFile = $(juliusFile "templates/genre/new.julius")

postGenreNewR :: Handler Html
postGenreNewR = do
  user <- requireAuth

  ((result, _), _) <- runFormPost genreNewForm
  case result of
    FormSuccess contribution -> do
      -- insert genre
      _ <- runDB $ insertGenre myGenreRepository (userIdent . entityVal $ user) contribution
      -- update tab with association to genre
      _ <-
        mapM
          ( \x -> runDB $ updateGenresOfTabAssociation myTabRepository x (slugify . genreInsertName $ contribution)
          )
          (maybe [] (myTabsDecoder . Just) (genreInsertTabs contribution))
      -- TODO update artists with association to genre
      redirect GenreMyContributionR
    FormFailure _ -> redirectWith status422 GenreListR
    FormMissing -> redirectWith status422 GenreListR
