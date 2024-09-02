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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Repositories.Repositories (ArtistRepository (..), GenreRepository (..), TabRepository (..), RepositoryError (..)) where

import ClassyPrelude
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Domain.Artist
import Domain.Genre
import Domain.Sorting
import Domain.Tab
import Import.NoFoundation

data RepositoryError
  = RecordWithWantedSoftIdAlreadyExistsError String
  | RecordToUpdateDoesNotExistError String
  | RecordToDeleteDoesNotExistError String
  deriving (Read, Eq, Show)

data ArtistRepository = ArtistRepository
  { deleteArtist :: Text -> DB (Either RepositoryError ()),
    getArtistList :: Maybe OrientationType -> Maybe ArtistSortByType -> DB [Entity Artist],
    getArtistListBySlug :: NonEmpty Text -> DB [Entity Artist],
    getMyContributionArtistList :: Text -> Maybe OrientationType -> Maybe ArtistSortByType -> DB [Entity Artist],
    getNonApprovedArtistList :: Maybe OrientationType -> Maybe ArtistSortByType -> DB [Entity Artist],
    grantApprovalArtist :: Artist -> Text -> DB (Either RepositoryError (Key Artist)),
    insertArtist :: Text -> ArtistContributionFormValues -> DB (Either RepositoryError (Key Artist)),
    maybeGetArtist :: Text -> DB (Maybe (Entity Artist)),
    maybeGetCreatorOfArtist :: Text -> DB (Maybe Text),
    revokeApprovalArtist :: Artist -> DB (Either RepositoryError (Key Artist)),
    updateArtist :: Artist -> ArtistEditFormValues -> DB (Either RepositoryError (Key Artist)),
    updateTabsOfArtist :: TabSearchKey -> Text -> DB (Either RepositoryError (Key Artist)),
    maybeGetArtistObjectIdFromSoftId :: Text -> DB (Maybe (Key Artist)),
    increaseViewCounterArtist :: Artist -> DB (Either RepositoryError (Key Artist)),
    likeArtist :: Artist -> Text -> DB (Either RepositoryError (Key Artist)),
    dislikeArtist :: Artist -> Text -> DB (Either RepositoryError (Key Artist))
  }

data GenreRepository = GenreRepository
  { deleteGenre :: Text -> DB (Either RepositoryError ()),
    getGenreFromTextarea :: Textarea -> DB [Genre],
    getGenreListBySlug :: NonEmpty Text -> DB [Entity Genre],
    getGenreList :: Maybe OrientationType -> Maybe GenreSortByType -> DB [Entity Genre],
    getMyContributionGenreList :: Text -> Maybe OrientationType -> Maybe GenreSortByType -> DB [Entity Genre],
    getNonApprovedGenreList :: Maybe OrientationType -> Maybe GenreSortByType -> DB [Entity Genre],
    grantApprovalGenre :: Genre -> Text -> DB (Either RepositoryError (Key Genre)),
    insertGenre :: Text -> GenreContributionFormValues -> DB (Either RepositoryError (Key Genre)),
    maybeGetGenreObjectIdFromSoftId :: Text -> DB (Maybe (Key Genre)),
    maybeGetCreatorOfGenre :: Text -> DB (Maybe Text),
    maybeGetGenre :: Text -> DB (Maybe (Entity Genre)),
    revokeApprovalGenre :: Genre -> DB (Either RepositoryError (Key Genre)),
    updateGenre :: Genre -> GenreEditFormValues -> DB (Either RepositoryError (Key Genre)),
    updateTabsOfGenre :: TabSearchKey -> Text -> DB (Either RepositoryError (Key Genre)),
    increaseViewCounterGenre :: Genre -> DB (Either RepositoryError (Key Genre)),
    likeGenre :: Genre -> Text -> DB (Either RepositoryError (Key Genre)),
    dislikeGenre :: Genre -> Text ->DB (Either RepositoryError (Key Genre))
  }

data TabRepository = TabRepository
  { deleteTab :: TabSearchKey -> DB (Either RepositoryError ()),
    getNonApprovedTabList :: Int -> Maybe OrientationType -> Maybe TabSortByType -> DB [Entity Tab],
    getPagingFromParams :: Int -> [SelectOpt Type],
    maybeGetTab :: TabSearchKey -> DB (Maybe (Entity Tab)),
    maybeGetTabKeyFromEntity :: Tab -> DB (Maybe (Key Tab)),
    getTabList :: Int -> Maybe OrientationType -> Maybe TabSortByType -> DB [Entity Tab],
    getTabListBySearchKeys :: [TabSearchKey] -> Int -> Maybe OrientationType -> Maybe TabSortByType -> DB [Entity Tab],
    grantApprovalTab :: Tab -> Text -> DB (Either RepositoryError (Key Tab)),
    insertTab :: Text -> TabContributionFormValues -> DB (Either RepositoryError (Key Tab)),
    maybeGetCreatorOfTab :: TabSearchKey -> DB (Maybe Text),
    getMyContributionTabList :: Text -> Int -> Maybe OrientationType -> Maybe TabSortByType -> DB [Entity Tab],
    maybeGetTabObjectIdFromSoftId :: TabSearchKey -> DB (Maybe (Key Tab)),
    revokeApprovalTab :: Tab -> DB (Either RepositoryError (Key Tab)),
    searchForTab :: Text -> Int -> Maybe OrientationType -> Maybe TabSortByType -> DB [Entity Tab],
    updateGenresOfTabAssociation :: TabSearchKey -> Text -> DB (Either RepositoryError (Key Tab)),
    updateTab :: Tab -> TabEditFormValues -> DB (Either RepositoryError (Key Tab)),
    increaseViewCounterTab :: Tab -> DB (Either RepositoryError (Key Tab)),
    likeTab :: Tab -> Text -> DB (Either RepositoryError (Key Tab)),
    dislikeTab :: Tab -> Text -> DB (Either RepositoryError (Key Tab))
  }
