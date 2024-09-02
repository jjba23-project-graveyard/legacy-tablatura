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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Repositories.MongoGenreRepository
  ( mongoGenreRepository,
  )
where

import Application.FormHandling
import Application.TimeHandling
import ClassyPrelude hiding (NonEmpty, fromList, nonEmpty, toList)
import Data.List.NonEmpty (NonEmpty, fromList, nonEmpty, toList)
import Data.Text (strip)
import Domain.Genre
import Domain.Sorting
import Domain.Tab
import Import.NoFoundation hiding (NonEmpty, fromList, nonEmpty, toList)
import Repositories.Repositories
import qualified Data.Map

mongoGenreRepository :: GenreRepository
mongoGenreRepository =
  GenreRepository
    { deleteGenre = mongoDeleteGenre,
      getGenreFromTextarea = mongoGetGenreFromTextarea,
      getGenreListBySlug = mongoGetGenreListBySlug,
      getGenreList = mongoGetGenreList,
      getMyContributionGenreList = mongoGetMyContributionGenreList,
      getNonApprovedGenreList = mongoGetNonApprovedGenreList,
      grantApprovalGenre = mongoGrantApprovalGenre,
      insertGenre = mongoInsertGenre,
      maybeGetGenreObjectIdFromSoftId = mongoMaybeGetObjectIdFromSoftId,
      maybeGetCreatorOfGenre = mongoMaybeGetCreatorOfGenre,
      maybeGetGenre = mongoMaybeGetGenre,
      revokeApprovalGenre = mongoRevokeApprovalGenre,
      updateGenre = mongoUpdateGenre,
      updateTabsOfGenre = mongoUpdateTabsOfGenre,
      increaseViewCounterGenre = mongoIncreaseViewCounterGenre,
      likeGenre = mongoLikeGenre,
      dislikeGenre = mongoDislikeGenre
    }
  where
    mongoGetGenreList :: Maybe OrientationType -> Maybe GenreSortByType -> DB [Entity Genre]
    mongoGetGenreList orientationMaybe sortByMaybe =
      selectList [GenreApproved ==. True] (getGenreOrderingFromParams orientationMaybe sortByMaybe)

    mongoGetMyContributionGenreList :: Text -> Maybe OrientationType -> Maybe GenreSortByType -> DB [Entity Genre]
    mongoGetMyContributionGenreList userId orientationMaybe sortByMaybe =
      selectList [GenreAuthorId ==. userId] (getGenreOrderingFromParams orientationMaybe sortByMaybe)

    mongoGetNonApprovedGenreList :: Maybe OrientationType -> Maybe GenreSortByType -> DB [Entity Genre]
    mongoGetNonApprovedGenreList orientationMaybe sortByMaybe =
      selectList [GenreApproved ==. False] (getGenreOrderingFromParams orientationMaybe sortByMaybe)

    mongoGetGenreFromTextarea :: Textarea -> DB [Genre]
    mongoGetGenreFromTextarea x = do
      case nonEmpty (lines . unTextarea $ x) of
        Nothing -> pure []
        Just y -> map entityVal <$> mongoGetGenreListBySlug y

    mongoMaybeGetGenre :: Text -> DB (Maybe (Entity Genre))
    mongoMaybeGetGenre pk = headMay <$> selectList [GenreSlug ==. pk] [LimitTo 1]

    mongoMaybeGetObjectIdFromSoftId :: Text -> DB (Maybe (Key Genre))
    mongoMaybeGetObjectIdFromSoftId pk = headMay <$> selectKeysList [GenreSlug ==. pk] [LimitTo 1]

    mongoGrantApprovalGenre :: Genre -> Text -> DB (Either RepositoryError (Key Genre))
    mongoGrantApprovalGenre genre userId = do
      internalEntityUpdateWithParams genre $ fromList [GenreApproved =. True, GenreApprovedBy =. Just userId]

    mongoRevokeApprovalGenre :: Genre -> DB (Either RepositoryError (Key Genre))
    mongoRevokeApprovalGenre genre = do
      internalEntityUpdateWithParams genre $ fromList [GenreApproved =. False]

    mongoDeleteGenre :: Text -> DB (Either RepositoryError ())
    mongoDeleteGenre pk = do
      deleteWhere [GenreSlug ==. pk]
      pure $ Right ()

    mongoUpdateTabsOfGenre :: TabSearchKey -> Text -> DB (Either RepositoryError (Key Genre))
    mongoUpdateTabsOfGenre searchKey genreSlg = do
      now <- liftIO getEpoch
      old <- mongoMaybeGetGenre genreSlg
      maybeObjectId <- mongoMaybeGetObjectIdFromSoftId genreSlg

      case maybeObjectId of
        Nothing -> pure $ Left . RecordToUpdateDoesNotExistError $ "Attempting to update record that does not exist!"
        Just objectId -> do
          let oldAssoc = genreTabs . entityVal . headEx $ old
              newAssoc = addIfNotExistsItem searchKey oldAssoc
          update objectId [GenreTabs =. newAssoc, GenreLastEdited =. now]
          pure $ Right objectId

    mongoInsertGenre :: Text -> GenreContributionFormValues -> DB (Either RepositoryError (Key Genre))
    mongoInsertGenre authId contribution = do
      genreMatches <- mongoMaybeGetGenre pk
      now <- liftIO getEpoch
      parsedImage <- parseUploadIntoResizedBase64 (genreInsertImage contribution)
      parsedImageType <- parseImageType (genreInsertImage contribution)

      if not . null $ genreMatches
        then pure $ Left . RecordWithWantedSoftIdAlreadyExistsError $ "Attempting to insert genre that already exists!"
        else do
          softId <-
            insert $
              Genre
                (genreInsertName contribution)
                pk
                parsedImage
                parsedImageType
                (genreInsertLink contribution)
                (fmap unTextarea (genreInsertDescription contribution))
                (myTabsDecoder . genreInsertTabs $ contribution)
                (maybe [] (map strip . lines . unTextarea) (genreInsertArtists contribution))
                now
                now
                authId
                False
                Nothing
                Nothing
                Nothing
                Nothing
          pure $ Right softId
      where
        pk = slugify . genreInsertName $ contribution

    mongoUpdateGenre :: Genre -> GenreEditFormValues -> DB (Either RepositoryError (Key Genre))
    mongoUpdateGenre genre genreEdit = do
      case genreEditImage genreEdit of
        Nothing -> internalEntityUpdateWithParams genre $ fromList updateFields
        Just submittedImage -> do
          parsedImage <- parseUploadIntoResizedBase64 (Just submittedImage)
          parsedImageType <- parseImageType (genreEditImage genreEdit)
          internalEntityUpdateWithParams genre $
            fromList $
              [GenreImage =. parsedImage, GenreImageType =. parsedImageType] <> updateFields
      where
        updateFields =
          [ GenreDescription =. unTextarea <$> genreEditDescription genreEdit,
            GenreMoreInfoLink =. genreEditLink genreEdit,
            GenreTabs =. (myTabsDecoder . genreEditTabs $ genreEdit),
            GenreArtists =. maybe [] (map strip . lines . unTextarea) (genreEditArtists genreEdit)
          ]

    mongoGetGenreListBySlug :: NonEmpty Text -> DB [Entity Genre]
    mongoGetGenreListBySlug genreSlugs = do
      selectList (computeOrFilter genreSlugs) [Asc GenreName]
      where
        individualSlugFilter z = [GenreSlug ==. z]
        computeOrFilter y = foldl1Ex' (||.) (map individualSlugFilter y)

    mongoMaybeGetCreatorOfGenre :: Text -> DB (Maybe Text)
    mongoMaybeGetCreatorOfGenre pk = do
      rs <- mongoMaybeGetGenre pk
      pure $ fmap (genreAuthorId . entityVal) (headMay rs)

    mongoIncreaseViewCounterGenre :: Genre -> DB (Either RepositoryError (Key Genre))
    mongoIncreaseViewCounterGenre entity =
      internalEntityUpdateWithParams entity $
        fromList $
          [GenreViewCount =. newViews]
      where
        newViews = case genreViewCount entity of
          Nothing -> Just 1
          Just oldViews -> Just $ oldViews + 1

    mongoLikeGenre :: Genre -> Text -> DB (Either RepositoryError (Key Genre))
    mongoLikeGenre entity userSoftId =
      internalEntityUpdateWithParams entity $
        fromList $
          [GenreLikes =. newLikes, GenreDislikes =. newDislikes]
      where
        newLikes = case genreLikes entity of
          Nothing  -> Just $ Data.Map.fromList [(userSoftId, True)]
          Just likeMap -> Just $ Data.Map.insert userSoftId True likeMap
        newDislikes = case genreDislikes entity of
          Nothing -> Nothing
          Just dislikeMap -> Just $ Data.Map.delete userSoftId dislikeMap

    mongoDislikeGenre :: Genre -> Text -> DB (Either RepositoryError (Key Genre))
    mongoDislikeGenre entity userSoftId =
      internalEntityUpdateWithParams entity $
        fromList $
          [GenreLikes =. newLikes, GenreDislikes =. newDislikes]
      where
        newLikes = case genreLikes entity of
          Nothing -> Nothing
          Just likeMap -> Just $ Data.Map.delete userSoftId likeMap
        newDislikes = case genreDislikes entity of
          Nothing -> Just $ Data.Map.fromList [(userSoftId, True)]
          Just dislikeMap -> Just $ Data.Map.insert userSoftId True dislikeMap

    internalEntityUpdateWithParams :: Genre -> NonEmpty (Update Genre) -> DB (Either RepositoryError (Key Genre))
    internalEntityUpdateWithParams entity params = do
      maybeObjectId <- mongoMaybeGetObjectIdFromSoftId $ genreSlug entity
      now <- liftIO getEpoch

      case maybeObjectId of
        Nothing -> pure $ Left . RecordToUpdateDoesNotExistError $ show entity
        Just objectId -> do
          update objectId $ (GenreLastEdited =. now) : toList params
          pure $ Right objectId
