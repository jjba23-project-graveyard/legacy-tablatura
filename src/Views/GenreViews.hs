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

module Views.GenreViews where

import Data.Maybe
import Domain.Genre
import Domain.Tab
import Import
import Views.CommonViews

genreEditForm :: Genre -> Html -> MForm Handler (FormResult GenreEditFormValues, Widget)
genreEditForm genre extra = do
  (genreLinkVal, genreLinkView) <- optionalCodeInputField . Just . genreMoreInfoLink $ genre
  (genreImageVal, genreImageView) <- emptyOptionalImageFileField
  (genreDescriptionVal, genreDescriptionView) <- optionalTextareaField (fmap (Just . Textarea) (genreDescription genre))
  (genreTabsVal, genreTabsView) <- optionalCodeTextareaField . Just . Just . Textarea . myTabsEncoder . genreTabs $ genre
  (genreArtistsVal, genreArtistsView) <- optionalCodeTextareaField . Just . Just . Textarea . unlines . genreArtists $ genre

  let genreEditFormValuesVal =
        GenreEditFormValues
          <$> genreDescriptionVal
          <*> genreImageVal
          <*> genreLinkVal
          <*> genreTabsVal
          <*> genreArtistsVal
      widget = $(widgetFile "genre/edit_form")

  return (genreEditFormValuesVal, widget)

genreNewForm :: Html -> MForm Handler (FormResult GenreContributionFormValues, Widget)
genreNewForm extra = do
  (genreNameVal, genreNameView) <- emptyRequiredInputField
  (genreLinkVal, genreLinkView) <- emptyOptionalCodeInputField
  (genreImageVal, genreImageView) <- emptyOptionalImageFileField
  (genreDescriptionVal, genreDescriptionView) <- emptyOptionalTextareaField
  (genreTabsVal, genreTabsView) <- emptyOptionalCodeTextareaField
  (genreArtistsVal, genreArtistsView) <- emptyOptionalCodeTextareaField

  let genreEditFormValuesVal =
        GenreContributionFormValues
          <$> genreNameVal
          <*> genreDescriptionVal
          <*> genreImageVal
          <*> genreLinkVal
          <*> genreTabsVal
          <*> genreArtistsVal
      widget = $(widgetFile "genre/new_form")

  return (genreEditFormValuesVal, widget)
