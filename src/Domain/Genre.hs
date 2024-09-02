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

module Domain.Genre where

import Data.Map (fromList)
import Data.Maybe
import Domain.Sorting
import Import.NoFoundation
import Text.Read (readMaybe)

data GenreContributionFormValues = GenreContributionFormValues
  { genreInsertName :: Text,
    genreInsertDescription :: Maybe Textarea,
    genreInsertImage :: Maybe FileInfo,
    genreInsertLink :: Maybe Text,
    genreInsertTabs :: Maybe Textarea,
    genreInsertArtists :: Maybe Textarea
  }

data GenreEditFormValues = GenreEditFormValues
  { genreEditDescription :: Maybe Textarea,
    genreEditImage :: Maybe FileInfo,
    genreEditLink :: Maybe Text,
    genreEditTabs :: Maybe Textarea,
    genreEditArtists :: Maybe Textarea
  }

data GenreSortByType
  = GenreAlphabetical
  | GenreChronological
  deriving (Eq, Show, Read, Ord)

data GetGenreListParam
  = GenreListOrientation
  | GenreListSortBy
  deriving (Eq, Show, Read, Ord)

type GetTabListParamMap a = Map GenreSortByType (Map OrientationType a)

parseGenreSortByTypeMaybe :: Maybe Text -> Maybe GenreSortByType
parseGenreSortByTypeMaybe = maybe Nothing (readMaybe . unpack)

decideValueForSorting :: OrientationType -> GenreSortByType -> GetTabListParamMap a -> a -> a
decideValueForSorting orientationVal sortByVal valueMap fallback =
  fromMaybe fallback foundOrientationMaybe
  where
    foundOrientationMaybe = lookup orientationVal =<< lookup sortByVal valueMap

-- TODO: pagination
getGenreOrderingFromParams :: Maybe OrientationType -> Maybe GenreSortByType -> [SelectOpt Genre]
getGenreOrderingFromParams orientationMaybe sortByValMaybe
  | isNothing orientationMaybe || isNothing sortByValMaybe = fallback
  | otherwise =
      decideValueForSorting
        (fromJust orientationMaybe)
        (fromJust sortByValMaybe)
        ( Data.Map.fromList
            [ ( GenreAlphabetical,
                Data.Map.fromList
                  [ (Ascending, [Asc GenreName, LimitTo 1996]),
                    (Descending, [Desc GenreName, LimitTo 1996])
                  ]
              ),
              ( GenreChronological,
                Data.Map.fromList
                  [ (Ascending, [Asc GenreCreatedAt, LimitTo 1996]),
                    (Descending, [Desc GenreCreatedAt, LimitTo 1996])
                  ]
              )
            ]
        )
        fallback
  where
    fallback = [Asc GenreName, LimitTo 1996]
