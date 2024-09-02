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

module Repositories.MongoTabRepository (mongoTabRepository) where

import Application.FormHandling
import Application.TimeHandling
import Data.List.NonEmpty (NonEmpty, fromList, toList)
import Data.Text (strip)
import Database.Persist.MongoDB
import Domain.Sorting
import Domain.Tab
import Import.NoFoundation hiding (fromList, singleton, toList)
import Repositories.Repositories
import qualified Data.Map

mongoTabRepository :: TabRepository
mongoTabRepository =
  TabRepository
    { deleteTab = mongoDeleteTab,
      getNonApprovedTabList = mongoGetNonApprovedTabList,
      getPagingFromParams = mongoGetPagingFromParams,
      maybeGetTab = mongoMaybeGetTab,
      maybeGetTabKeyFromEntity = mongoMaybeGetTabKeyFromEntity,
      getTabList = mongoGetTabList,
      getTabListBySearchKeys = mongoGetTabListBySearchKeys,
      grantApprovalTab = mongoGrantApprovalTab,
      insertTab = mongoInsertTab,
      maybeGetCreatorOfTab = mongoMaybeGetCreatorOfTab,
      getMyContributionTabList = mongoGetMyContributionTabList,
      maybeGetTabObjectIdFromSoftId = mongoMaybeGetObjectIdFromSoftId,
      revokeApprovalTab = mongoRevokeApprovalTab,
      searchForTab = mongoSearchForTab,
      updateGenresOfTabAssociation = mongoUpdateGenresOfTabAssociation,
      updateTab = mongoUpdateTab,
      increaseViewCounterTab = mongoIncreaseViewCounterTab,
      likeTab = mongoLikeTab,
      dislikeTab = mongoDislikeTab
    }
  where
    mongoResultsPerPage :: Int
    mongoResultsPerPage = 50

    mongoGetPagingFromParams :: Int -> [SelectOpt record]
    mongoGetPagingFromParams x =
      [ LimitTo mongoResultsPerPage,
        OffsetBy $ (x - 1) * mongoResultsPerPage
      ]

    mongoGetTabList :: Int -> Maybe OrientationType -> Maybe TabSortByType -> DB [Entity Tab]
    mongoGetTabList pageNumber orientationMaybe sortByMaybe = do
      selectList
        [TabApproved ==. True]
        ( mongoGetPagingFromParams pageNumber <> getTabOrderingFromParams orientationMaybe sortByMaybe
        )

    mongoGetNonApprovedTabList :: Int -> Maybe OrientationType -> Maybe TabSortByType -> DB [Entity Tab]
    mongoGetNonApprovedTabList pageNumber orientationMaybe sortByMaybe = do
      selectList
        [TabApproved ==. False]
        ( mongoGetPagingFromParams pageNumber <> getTabOrderingFromParams orientationMaybe sortByMaybe
        )

    mongoGetMyContributionTabList :: Text -> Int -> Maybe OrientationType -> Maybe TabSortByType -> DB [Entity Tab]
    mongoGetMyContributionTabList userId pageNumber orientationMaybe sortByMaybe = do
      selectList
        [TabAuthorId ==. userId]
        ( mongoGetPagingFromParams pageNumber <> getTabOrderingFromParams orientationMaybe sortByMaybe
        )

    mongoSearchForTab :: Text -> Int -> Maybe OrientationType -> Maybe TabSortByType -> DB [Entity Tab]
    mongoSearchForTab searchQuery pageNumber orientationMaybe sortByMaybe =
      selectList
        (searchParams <> [TabApproved ==. True])
        (mongoGetPagingFromParams pageNumber <> getTabOrderingFromParams orientationMaybe sortByMaybe)
      where
        searchParams =
          [TabPk &->. TabSearchKeyArtists `nestAnyEq` searchQuery]
            ||. [TabPk &->. TabSearchKeySong `nestEq` searchQuery]
            ||. [TabPk &->. TabSearchKeyArtists `nestIn` [searchQuery]]

    mongoGetTabListBySearchKeys :: [TabSearchKey] -> Int -> Maybe OrientationType -> Maybe TabSortByType -> DB [Entity Tab]
    mongoGetTabListBySearchKeys pks pageNumber orientationMaybe sortByMaybe =
      selectList
        [TabPk <-. pks, TabApproved ==. True]
        (mongoGetPagingFromParams pageNumber <> getTabOrderingFromParams orientationMaybe sortByMaybe)

    mongoMaybeGetTab :: TabSearchKey -> DB (Maybe (Entity Tab))
    mongoMaybeGetTab pk = headMay <$> selectList [TabPk ==. pk] [LimitTo 1]

    mongoMaybeGetObjectIdFromSoftId :: TabSearchKey -> DB (Maybe (Key Tab))
    mongoMaybeGetObjectIdFromSoftId pk = headMay <$> selectKeysList [TabPk ==. pk] [LimitTo 1]

    mongoDeleteTab :: TabSearchKey -> DB (Either RepositoryError ())
    mongoDeleteTab pk = do
      deleteWhere [TabPk ==. pk]
      pure $ Right ()

    mongoConstructTabExtraDataSources :: Maybe Text -> Maybe Text -> Maybe ExtraTabDataSources
    mongoConstructTabExtraDataSources youtubeURL spotifyURL
      | isNothing youtubeURL && isNothing spotifyURL = Nothing
      | otherwise =
          Just
            ExtraTabDataSources
              { extraTabDataSourcesYouTubeEmbedURL = youtubeURL,
                extraTabDataSourcesSpotifyEmbedURL = spotifyURL
              }

    mongoInsertTab :: Text -> TabContributionFormValues -> DB (Either RepositoryError (Key Tab))
    mongoInsertTab authId contribution = do
      tabMatches <- mongoMaybeGetTab pk
      now <- liftIO getEpoch
      parsedImage <- parseUploadIntoResizedBase64 (tabInsertImage contribution)

      if not . null $ tabMatches
        then pure . Left . RecordWithWantedSoftIdAlreadyExistsError $ show pk
        else do
          softId <-
            insert $
              Tab
                pk
                (tabArtistsURLToList . tabInsertArtistNames $ contribution)
                (tabInsertSongName contribution)
                (unTextarea . tabInsertContent $ contribution)
                (fmap unTextarea (tabInsertLegend contribution))
                (tabInsertMusicKey contribution)
                parsedImage
                parsedImageType
                (tabInsertTuning contribution)
                (mongoComputeGenreData . tabInsertGenre $ contribution)
                (mongoConstructTabExtraDataSources (tabInsertYouTubeEmbedURL contribution) (tabInsertSpotifyEmbedURL contribution))
                now
                now
                authId
                False
                Nothing
                (tabInsertCreationYear contribution)
                Nothing
                Nothing
                Nothing
                (tabInsertAlbumName contribution)
                (tabInsertAlbumLink contribution)
          pure $ Right softId
      where
        parsedImageType = Just "image/webp"
        pk =
          TabSearchKey
            (map slugify . tabArtistsURLToList . tabInsertArtistNames $ contribution)
            (slugify . tabInsertSongName $ contribution)
            (tabInsertVersion contribution)

    mongoMaybeGetTabKeyFromEntity :: Tab -> DB (Maybe (Key Tab))
    mongoMaybeGetTabKeyFromEntity x =
      mongoMaybeGetObjectIdFromSoftId $
        TabSearchKey
          (tabSearchKeyArtists . tabPk $ x)
          (tabSearchKeySong . tabPk $ x)
          (tabSearchKeyVersion . tabPk $ x)

    mongoGrantApprovalTab :: Tab -> Text -> DB (Either RepositoryError (Key Tab))
    mongoGrantApprovalTab tab userId = do
      internalEntityUpdateWithParams tab $ fromList [TabApproved =. True, TabApprovedBy =. Just userId]

    mongoRevokeApprovalTab :: Tab -> DB (Either RepositoryError (Key Tab))
    mongoRevokeApprovalTab tab = do
      internalEntityUpdateWithParams tab $ fromList [TabApproved =. False]

    mongoUpdateTab :: Tab -> TabEditFormValues -> DB (Either RepositoryError (Key Tab))
    mongoUpdateTab tab tabEdit = do
      case tabEditImage tabEdit of
        Nothing -> internalEntityUpdateWithParams tab $ fromList updateFields
        Just submittedImage -> do
          parsedImage <- parseUploadIntoResizedBase64 $ Just submittedImage
          internalEntityUpdateWithParams tab $ fromList $ [TabImage =. parsedImage, TabImageType =. parsedImageType] <> updateFields
      where
        parsedImageType = Just "image/webp"
        updateFields =
          [ TabContent =. (unTextarea . tabEditContent $ tabEdit),
            TabLegend =. fmap unTextarea (tabEditLegend tabEdit),
            TabMusicKey =. tabEditKey tabEdit,
            TabTuning =. tabEditTuning tabEdit,
            TabGenres =. (mongoComputeGenreData . tabEditGenre $ tabEdit),
            TabCreationYear =. tabEditCreationYear tabEdit,
            TabExtraDataSources =. mongoConstructTabExtraDataSources (tabEditYouTubeEmbedURL tabEdit) (tabEditSpotifyEmbedURL tabEdit),
            TabAlbumName =. tabEditAlbumName tabEdit,
            TabAlbumLink =. tabEditAlbumLink tabEdit
          ]

    mongoComputeGenreData :: Maybe Textarea -> [Text]
    mongoComputeGenreData x = map Data.Text.strip $ maybe [] (lines . unTextarea) x

    mongoUpdateGenresOfTabAssociation :: TabSearchKey -> Text -> DB (Either RepositoryError (Key Tab))
    mongoUpdateGenresOfTabAssociation searchKey genreSl = do
      now <- liftIO getEpoch
      old <- mongoMaybeGetTab searchKey
      maybeObjectId <- mongoMaybeGetObjectIdFromSoftId searchKey

      case maybeObjectId of
        Nothing -> pure $ Left . RecordToUpdateDoesNotExistError $ "Attempting to update record that does not exist!"
        Just objectId -> do
          let oldAssoc = tabGenres . entityVal . headEx $ old
              newAssoc = addIfNotExistsItem genreSl oldAssoc
          update objectId [TabGenres =. newAssoc, TabLastEdited =. now]
          pure $ Right objectId

    mongoMaybeGetCreatorOfTab :: TabSearchKey -> DB (Maybe Text)
    mongoMaybeGetCreatorOfTab pk = do
      rs <- mongoMaybeGetTab pk
      pure $ fmap (tabAuthorId . entityVal) (headMay rs)

    mongoIncreaseViewCounterTab :: Tab -> DB (Either RepositoryError (Key Tab))
    mongoIncreaseViewCounterTab entity =
      internalEntityUpdateWithParams
        entity $
          fromList $
            [TabViewCount =. newViews]
      where
        newViews = case tabViewCount entity of
          Nothing -> Just 1
          Just oldViews -> Just $ oldViews + 1

    mongoLikeTab :: Tab -> Text -> DB (Either RepositoryError (Key Tab))
    mongoLikeTab entity userSoftId =
      internalEntityUpdateWithParams
        entity $
          fromList $
            [TabLikes =. newLikes, TabDislikes =. newDislikes]
      where
        newLikes = case tabLikes entity of
          Nothing -> Just $ Data.Map.fromList [(userSoftId, True)]
          Just likeMap -> Just $ Data.Map.insert userSoftId True likeMap
        newDislikes = case tabDislikes entity of
          Nothing -> Nothing
          Just dislikeMap -> Just $ Data.Map.delete userSoftId dislikeMap

    mongoDislikeTab :: Tab -> Text -> DB (Either RepositoryError (Key Tab))
    mongoDislikeTab entity userSoftId =
      internalEntityUpdateWithParams
        entity $
          fromList $
            [TabLikes =. newLikes, TabDislikes =. newDislikes]
      where
        newLikes = case tabLikes entity of
          Nothing -> Nothing
          Just likeMap -> Just $ Data.Map.delete userSoftId likeMap
        newDislikes = case tabDislikes entity of
          Nothing -> Just $ Data.Map.fromList [(userSoftId, True)]
          Just dislikeMap -> Just $ Data.Map.insert userSoftId True dislikeMap

    internalEntityUpdateWithParams :: Tab -> NonEmpty (Update Tab) -> DB (Either RepositoryError (Key Tab))
    internalEntityUpdateWithParams entity params = do
      maybeObjectId <- mongoMaybeGetTabKeyFromEntity entity
      now <- liftIO getEpoch

      case maybeObjectId of
        Nothing -> pure $ Left . RecordToUpdateDoesNotExistError $ show entity
        Just objectId -> do
          update objectId $ (TabLastEdited =. now) : toList params
          pure $ Right objectId
