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
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Domain.Artist where

import           Data.Map            (fromList)
import           Data.Maybe
import           Domain.Sorting
import           Import.NoFoundation
import           Text.Read           (readMaybe)

data ArtistContributionFormValues = ArtistContributionFormValues
  { artistInsertName        :: Text,
    artistInsertDescription :: Maybe Textarea,
    artistInsertImage       :: Maybe FileInfo,
    artistInsertLink        :: Maybe Text,
    artistInsertTabs        :: Maybe Textarea,
    artistInsertGenres      :: Maybe Textarea
  }

data ArtistEditFormValues = ArtistEditFormValues
  { artistEditDescription :: Maybe Textarea,
    artistEditImage       :: Maybe FileInfo,
    artistEditLink        :: Maybe Text,
    artistEditTabs        :: Maybe Textarea,
    artistEditGenres      :: Maybe Textarea
  }

data ArtistSortByType
  = ArtistAlphabetical
  | ArtistChronological
  deriving (Eq, Show, Read, Ord)

data GetArtistListParam
  = ArtistListOrientation
  | ArtistListSortBy
  deriving (Eq, Show, Read, Ord)

type GetTabListParamMap a = Map ArtistSortByType (Map OrientationType a)

parseArtistSortByTypeMaybe :: Maybe Text -> Maybe ArtistSortByType
parseArtistSortByTypeMaybe = maybe Nothing (readMaybe . unpack)

decideValueForSorting :: OrientationType -> ArtistSortByType -> GetTabListParamMap a -> a -> a
decideValueForSorting orientationVal sortByVal valueMap fallback =
  fromMaybe fallback foundOrientationMaybe
  where
    foundOrientationMaybe = lookup orientationVal =<< lookup sortByVal valueMap

-- TODO: pagination
getArtistOrderingFromParams :: Maybe OrientationType -> Maybe ArtistSortByType -> [SelectOpt Artist]
getArtistOrderingFromParams orientationMaybe sortByValMaybe
  | isNothing orientationMaybe || isNothing sortByValMaybe = fallback
  | otherwise =
      decideValueForSorting
        (fromJust orientationMaybe)
        (fromJust sortByValMaybe)
        ( Data.Map.fromList
            [ ( ArtistAlphabetical,
                Data.Map.fromList
                  [ (Ascending, [Asc ArtistName, LimitTo 1996]),
                    (Descending, [Desc ArtistName, LimitTo 1996])
                  ]
              ),
              ( ArtistChronological,
                Data.Map.fromList
                  [ (Ascending, [Asc ArtistCreatedAt, LimitTo 1996]),
                    (Descending, [Desc ArtistCreatedAt, LimitTo 1996])
                  ]
              )
            ]
        )
        fallback
  where
    fallback = [Asc ArtistName, LimitTo 1996]
