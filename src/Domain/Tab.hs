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

module Domain.Tab where

import Data.List (head, (!!))
import Data.Map (fromList)
import Data.Maybe
import Data.Text (split, splitOn, strip)
import Domain.Sorting
import Import.NoFoundation
import Text.Printf (printf)
import Text.Read (read, readMaybe)

data TabContributionFormValues = TabContributionFormValues
  { tabInsertArtistNames :: Text,
    tabInsertSongName :: Text,
    tabInsertVersion :: Int,
    tabInsertContent :: Textarea,
    tabInsertLegend :: Maybe Textarea,
    tabInsertMusicKey :: Maybe Text,
    tabInsertImage :: Maybe FileInfo,
    tabInsertTuning :: Maybe Text,
    tabInsertGenre :: Maybe Textarea,
    tabInsertYouTubeEmbedURL :: Maybe Text,
    tabInsertSpotifyEmbedURL :: Maybe Text,
    tabInsertCreationYear :: Maybe Text,
    tabInsertAlbumName :: Maybe Text,
    tabInsertAlbumLink :: Maybe Text
  }

data TabEditFormValues = TabEditFormValues
  { tabEditContent :: Textarea,
    tabEditLegend :: Maybe Textarea,
    tabEditKey :: Maybe Text,
    tabEditImage :: Maybe FileInfo,
    tabEditTuning :: Maybe Text,
    tabEditGenre :: Maybe Textarea,
    tabEditYouTubeEmbedURL :: Maybe Text,
    tabEditSpotifyEmbedURL :: Maybe Text,
    tabEditCreationYear :: Maybe Text,
    tabEditAlbumName :: Maybe Text,
    tabEditAlbumLink :: Maybe Text
  }

data TabSortByType
  = TabArtistAlphabetical
  | TabSongAlphabetical
  | TabChronological
  deriving (Eq, Show, Read, Ord)

data GetTabListParam
  = TabListOrientation
  | TabListSortBy
  | TabListPageNumber
  deriving (Eq, Show, Read, Ord)

type GetTabListParamMap a = Map TabSortByType (Map OrientationType a)

parseTabPageNumberMaybe :: Maybe Text -> Int
parseTabPageNumberMaybe = maybe 1 (read . unpack)

parseSortByTypeMaybe :: Maybe Text -> Maybe TabSortByType
parseSortByTypeMaybe = maybe Nothing (readMaybe . unpack)

decideValueForSorting :: OrientationType -> TabSortByType -> GetTabListParamMap a -> a -> a
decideValueForSorting orientationVal sortByVal valueMap fallback =
  fromMaybe fallback foundOrientationMaybe
  where
    foundOrientationMaybe = lookup orientationVal =<< lookup sortByVal valueMap

getTitleFromParams :: Maybe OrientationType -> Maybe TabSortByType -> String
getTitleFromParams orientationMaybe sortByValMaybe
  | isNothing orientationMaybe || isNothing sortByValMaybe = fallback
  | otherwise =
      decideValueForSorting
        (fromJust orientationMaybe)
        (fromJust sortByValMaybe)
        ( Data.Map.fromList
            [ ( TabSongAlphabetical,
                Data.Map.fromList
                  [ (Ascending, "Tablatura | A to Z songs all time"),
                    (Descending, "Tablatura | Z to A songs all time")
                  ]
              ),
              ( TabArtistAlphabetical,
                Data.Map.fromList
                  [ (Ascending, "Tablatura | A to Z artists all time"),
                    (Descending, "Tablatura | Z to A artist all time")
                  ]
              ),
              ( TabChronological,
                Data.Map.fromList
                  [ (Ascending, "Tablatura | Old to New tabs"),
                    (Descending, "Tablatura | Newest tabs")
                  ]
              )
            ]
        )
        fallback
  where
    fallback = "Tablatura | Newest tabs"

getTabOrderingFromParams :: Maybe OrientationType -> Maybe TabSortByType -> [SelectOpt Tab]
getTabOrderingFromParams orientationMaybe sortByValMaybe
  | isNothing orientationMaybe || isNothing sortByValMaybe = fallback
  | otherwise =
      decideValueForSorting
        (fromJust orientationMaybe)
        (fromJust sortByValMaybe)
        ( Data.Map.fromList
            [ ( TabSongAlphabetical,
                Data.Map.fromList
                  [ (Ascending, [Asc TabSongName, LimitTo 1996]),
                    (Descending, [Desc TabSongName, LimitTo 1996])
                  ]
              ),
              ( TabArtistAlphabetical,
                Data.Map.fromList
                  [ (Ascending, [Asc TabArtistNames, LimitTo 1996]),
                    (Descending, [Desc TabArtistNames, LimitTo 1996])
                  ]
              ),
              ( TabChronological,
                Data.Map.fromList
                  [ (Ascending, [Asc TabCreatedAt, LimitTo 1996]),
                    (Descending, [Desc TabCreatedAt, LimitTo 1996])
                  ]
              )
            ]
        )
        fallback
  where
    fallback = [Desc TabCreatedAt, LimitTo 1996]

extractSearchKeyFromTab :: Tab -> TabSearchKey
extractSearchKeyFromTab x =
  TabSearchKey
    (tabSearchKeyArtists . tabPk $ x)
    (tabSearchKeySong . tabPk $ x)
    (tabSearchKeyVersion . tabPk $ x)

tabArtistsListToURL :: [Text] -> Text
tabArtistsListToURL = intercalate "|"

tabArtistsListToHumanReadable :: [Text] -> Text
tabArtistsListToHumanReadable = intercalate " & "

tabArtistsURLToList :: Text -> [Text]
tabArtistsURLToList = splitOn "|"

myTabsEncoder :: [TabSearchKey] -> Text
myTabsEncoder searchKeys = unlines $ map createTabString searchKeys
  where
    createTabString g =
      fromString $
        printf
          "%s/%s/%d"
          (tabArtistsListToURL . tabSearchKeyArtists $ g)
          (tabSearchKeySong g)
          (tabSearchKeyVersion g)

-- | Receive a Maybe Textarea that contains an encoded tab identifier per line and decode it to a [TabSearchKey]
-- | e.g. dio/holy-diver/1 becomes a TabSearchKey [dio] holy-diver 1
myTabsDecoder :: Maybe Textarea -> [TabSearchKey]
myTabsDecoder x
  | isNothing x = []
  | otherwise = map parseTabFromValidLine validTabLines
  where
    givenRawTabText = map strip (maybe [] (lines . unTextarea) x)
    validTabLines = filter (\line -> length line == 3) $ map (split (== '/')) givenRawTabText
    parseTabFromValidLine line = TabSearchKey (tabArtistsURLToList (Data.List.head line)) (line !! 1) (read (unpack $ line !! 2) :: Int)
