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

module Views.ArtistViews where

import Views.CommonViews
import Data.Maybe
import Domain.Artist
import Domain.Tab (myTabsEncoder)
import Import

artistEditForm :: Artist -> Html -> MForm Handler (FormResult ArtistEditFormValues, Widget)
artistEditForm artist extra = do
  (artistLinkVal, artistLinkView) <- optionalCodeInputField . Just . artistMoreInfoLink $ artist
  (artistImageVal, artistImageView) <- emptyOptionalImageFileField
  (artistDescriptionVal, artistDescriptionView) <- optionalTextareaField (fmap (Just . Textarea) (artistDescription artist))
  (artistTabsVal, artistTabsView) <- optionalCodeTextareaField . Just . Just . Textarea . myTabsEncoder . artistTabs $ artist
  (artistGenresVal, artistGenresView) <- optionalCodeTextareaField . Just . Just . Textarea . unlines . artistGenres $ artist

  let artistEditFormValuesVal =
        ArtistEditFormValues
          <$> artistDescriptionVal
          <*> artistImageVal
          <*> artistLinkVal
          <*> artistTabsVal
          <*> artistGenresVal
      widget = $(widgetFile "artist/edit_form")

  return (artistEditFormValuesVal, widget)

artistNewForm :: Html -> MForm Handler (FormResult ArtistContributionFormValues, Widget)
artistNewForm extra = do
  (artistNameVal, artistNameView) <- emptyRequiredInputField
  (artistLinkVal, artistLinkView) <- emptyOptionalCodeInputField
  (artistImageVal, artistImageView) <- emptyOptionalImageFileField
  (artistDescriptionVal, artistDescriptionView) <- emptyOptionalTextareaField
  (artistTabsVal, artistTabsView) <- emptyOptionalCodeTextareaField
  (artistGenresVal, artistGenresView) <- emptyOptionalCodeTextareaField

  let artistEditFormValuesVal =
        ArtistContributionFormValues
          <$> artistNameVal
          <*> artistDescriptionVal
          <*> artistImageVal
          <*> artistLinkVal
          <*> artistTabsVal
          <*> artistGenresVal
      widget = $(widgetFile "artist/new_form")

  return (artistEditFormValuesVal, widget)
