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

module Repositories.MongoArtistRepository
  ( mongoArtistRepository,
  )
where

import Application.FormHandling
import Application.TimeHandling
import Data.List.NonEmpty (NonEmpty, fromList, nonEmpty, toList)
import Data.Text (strip)
import Domain.Artist
import Domain.Sorting
import Domain.Tab (myTabsDecoder)
import Import.NoFoundation hiding (NonEmpty, fromList, toList)
import Repositories.Repositories
import qualified Data.Map

mongoArtistRepository :: ArtistRepository
mongoArtistRepository =
  ArtistRepository
    { maybeGetArtistObjectIdFromSoftId = mongoMaybeGetObjectIdFromSoftId,
      getArtistList = mongoGetArtistList,
      getMyContributionArtistList = mongoGetMyContributionArtists,
      getNonApprovedArtistList = mongoGetNonApprovedArtists,
      maybeGetArtist = mongoMaybeGetArtist,
      grantApprovalArtist = mongoGrantApprovalArtist,
      revokeApprovalArtist = mongoRevokeApprovalArtist,
      deleteArtist = mongoDeleteArtist,
      updateTabsOfArtist = mongoUpdateTabsOfArtist,
      insertArtist = mongoInsertArtist,
      updateArtist = mongoUpdateArtist,
      getArtistListBySlug = mongoGetArtistListBySlug,
      maybeGetCreatorOfArtist = mongoGetCreatorOfArtist,
      increaseViewCounterArtist = mongoIncreaseViewCounterArtist,
      likeArtist = mongoLikeArtist,
      dislikeArtist = mongoDislikeArtist
    }
  where
    mongoMaybeGetObjectIdFromSoftId :: Text -> DB (Maybe (Key Artist))
    mongoMaybeGetObjectIdFromSoftId softId = headMay <$> selectKeysList [ArtistSlug ==. softId] [LimitTo 1]

    mongoGetArtistList :: Maybe OrientationType -> Maybe ArtistSortByType -> DB [Entity Artist]
    mongoGetArtistList orientationType sortByType =
      selectList [ArtistApproved ==. True] (getArtistOrderingFromParams orientationType sortByType)

    mongoGetMyContributionArtists :: Text -> Maybe OrientationType -> Maybe ArtistSortByType -> DB [Entity Artist]
    mongoGetMyContributionArtists userId orientationType sortByType =
      selectList [ArtistAuthorId ==. userId] (getArtistOrderingFromParams orientationType sortByType)

    mongoGetNonApprovedArtists :: Maybe OrientationType -> Maybe ArtistSortByType -> DB [Entity Artist]
    mongoGetNonApprovedArtists orientationType sortByType =
      selectList [ArtistApproved ==. False] (getArtistOrderingFromParams orientationType sortByType)

    mongoMaybeGetArtist :: Text -> DB (Maybe (Entity Artist))
    mongoMaybeGetArtist softId = headMay <$> selectList [ArtistSlug ==. softId] [LimitTo 1]

    mongoGrantApprovalArtist :: Artist -> Text -> DB (Either RepositoryError (Key Artist))
    mongoGrantApprovalArtist artist userId = internalEntityUpdateWithParams artist $ fromList [ArtistApproved =. True, ArtistApprovedBy =. Just userId]

    mongoRevokeApprovalArtist :: Artist -> DB (Either RepositoryError (Key Artist))
    mongoRevokeApprovalArtist artist = internalEntityUpdateWithParams artist $ fromList [ArtistApproved =. False]

    mongoDeleteArtist :: Text -> DB (Either RepositoryError ())
    mongoDeleteArtist softId = do
      deleteWhere [ArtistSlug ==. softId]
      pure $ Right ()

    mongoUpdateTabsOfArtist :: TabSearchKey -> Text -> DB (Either RepositoryError (Key Artist))
    mongoUpdateTabsOfArtist searchKey artistSlg = do
      now <- liftIO getEpoch
      old <- mongoMaybeGetArtist artistSlg
      maybeObjectId <- mongoMaybeGetObjectIdFromSoftId artistSlg

      case maybeObjectId of
        Nothing -> pure $ Left . RecordToUpdateDoesNotExistError $ "Attempting to update record that does not exist!"
        Just objectId -> do
          let oldAssoc = artistTabs . entityVal . headEx $ old
              newAssoc = addIfNotExistsItem searchKey oldAssoc
          update objectId [ArtistTabs =. newAssoc, ArtistLastEdited =. now]
          pure $ Right objectId

    mongoInsertArtist :: Text -> ArtistContributionFormValues -> DB (Either RepositoryError (Key Artist))
    mongoInsertArtist authId contribution = do
      artistMatches <- mongoMaybeGetArtist softId
      now <- liftIO getEpoch
      maybeParsedImage <- parseUploadIntoResizedBase64 (artistInsertImage contribution)
      maybeParsedImageType <- parseImageType (artistInsertImage contribution)

      if not . null $ artistMatches
        then pure $ Left $ RecordWithWantedSoftIdAlreadyExistsError "Attempting to insert artist that already exists!"
        else do
          objectId <-
            insert $
              Artist
                (artistInsertName contribution)
                softId
                maybeParsedImage
                maybeParsedImageType
                (artistInsertLink contribution)
                (unTextarea <$> artistInsertDescription contribution)
                (myTabsDecoder . artistInsertTabs $ contribution)
                (maybe [] (map strip . lines . unTextarea) (artistInsertGenres contribution))
                now
                now
                authId
                False
                Nothing
                Nothing
                Nothing
                Nothing
          pure $ Right objectId
      where
        softId = slugify . artistInsertName $ contribution

    mongoUpdateArtist :: Artist -> ArtistEditFormValues -> DB (Either RepositoryError (Key Artist))
    mongoUpdateArtist artist artistEdit = do
      case artistEditImage artistEdit of
        Nothing -> internalEntityUpdateWithParams artist $ fromList params
        Just submittedImage -> do
          parsedImage <- parseUploadIntoResizedBase64 (Just submittedImage)
          parsedImageType <- parseImageType (artistEditImage artistEdit)
          internalEntityUpdateWithParams artist $
            fromList $
              [ArtistImage =. parsedImage, ArtistImageType =. parsedImageType] <> params
      where
        params =
          [ ArtistDescription =. unTextarea <$> artistEditDescription artistEdit,
            ArtistMoreInfoLink =. artistEditLink artistEdit,
            ArtistTabs =. (myTabsDecoder . artistEditTabs $ artistEdit),
            ArtistGenres =. maybe [] (map strip . lines . unTextarea) (artistEditGenres artistEdit)
          ]

    mongoGetArtistListBySlug :: NonEmpty Text -> DB [Entity Artist]
    mongoGetArtistListBySlug x = selectList (computeOrFilter x) [Asc ArtistName]
      where
        individualSlugFilter slug = [ArtistSlug ==. slug]
        computeOrFilter y = foldl1Ex' (||.) (map individualSlugFilter y)

    mongoGetCreatorOfArtist :: Text -> DB (Maybe Text)
    mongoGetCreatorOfArtist softId = do
      rs <- mongoMaybeGetArtist softId
      pure $ fmap (artistAuthorId . entityVal) (headMay rs)

    mongoLikeArtist :: Artist -> Text -> DB (Either RepositoryError (Key Artist))
    mongoLikeArtist entity userSoftId =
      internalEntityUpdateWithParams
        entity $
          fromList $
            [ArtistLikes =. newLikes, ArtistDislikes =. newDislikes]
      where
        newLikes = case artistLikes entity of
          Nothing -> Just $ Data.Map.fromList [(userSoftId, True)]
          Just likeMap -> Just $ Data.Map.insert userSoftId True likeMap
        newDislikes = case artistDislikes entity of
          Nothing -> Nothing
          Just dislikeMap -> Just $ Data.Map.delete userSoftId dislikeMap

    mongoDislikeArtist :: Artist -> Text -> DB (Either RepositoryError (Key Artist))
    mongoDislikeArtist entity userSoftId =
      internalEntityUpdateWithParams
        entity $
          fromList $
            [ArtistLikes =. newLikes, ArtistDislikes =. newDislikes]
      where
        newLikes = case artistLikes entity of
          Nothing -> Nothing
          Just likeMap -> Just $ Data.Map.delete userSoftId likeMap
        newDislikes = case artistDislikes entity of
          Nothing -> Just $ Data.Map.fromList [(userSoftId, True)]
          Just dislikeMap -> Just $ Data.Map.insert userSoftId True dislikeMap

    mongoIncreaseViewCounterArtist :: Artist -> DB (Either RepositoryError (Key Artist))
    mongoIncreaseViewCounterArtist entity =
      internalEntityUpdateWithParams
        entity $
          fromList $
            [ArtistViewCount =. newViews]
      where
        newViews = case artistViewCount entity of
          Nothing -> Just 1
          Just oldViews -> Just $ oldViews + 1

    internalEntityUpdateWithParams :: Artist -> NonEmpty (Update Artist) -> DB (Either RepositoryError (Key Artist))
    internalEntityUpdateWithParams entity params = do
      maybeObjectId <- mongoMaybeGetObjectIdFromSoftId $ artistSlug entity
      now <- liftIO getEpoch

      case maybeObjectId of
        Nothing -> pure $ Left . RecordToUpdateDoesNotExistError $ show entity
        Just objectId -> do
          update objectId $ (ArtistLastEdited =. now) : toList params
          pure $ Right objectId
