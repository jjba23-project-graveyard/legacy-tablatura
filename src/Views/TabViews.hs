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

module Views.TabViews where

import Data.Maybe
import Domain.Tab
import Import
import Views.CommonViews

tabEditForm :: Tab -> Html -> MForm Handler (FormResult TabEditFormValues, Widget)
tabEditForm tab extra = do
  (legendVal, legendView) <- optionalCodeTextareaField (fmap (Just . Textarea) (tabLegend tab))
  (tabKeyVal, tabKeyView) <- optionalInputField . Just . tabMusicKey $ tab
  (tabImageVal, tabImageView) <- emptyOptionalImageFileField
  (tabVal, tabView) <- requiredCodeTextareaField . Just . Textarea . tabContent $ tab
  (tabTuningVal, tabTuningView) <- optionalInputField . Just . tabTuning $ tab
  (tabGenreVal, tabGenreView) <- optionalTextareaField . Just . Just . Textarea . unlines . tabGenres $ tab
  (tabYouTubeEmbedURLVal, tabYouTubeEmbedURLView) <- optionalCodeInputField youtubeEmbedURL
  (tabSpotifyEmbedURLVal, tabSpotifyEmbedURLView) <- optionalCodeInputField spotifyEmbedURL
  (tabCreationYearVal, tabCreationYearView) <- optionalInputField . Just  . tabCreationYear  $ tab
  (tabAlbumNameVal, tabAlbumNameView) <- optionalInputField . Just . tabAlbumName $ tab
  (tabAlbumLinkVal, tabAlbumLinkView) <- optionalCodeInputField . Just . tabAlbumLink $ tab

  let tabEditFormValuesVal =
        TabEditFormValues
          <$> tabVal
          <*> legendVal
          <*> tabKeyVal
          <*> tabImageVal
          <*> tabTuningVal
          <*> tabGenreVal
          <*> tabYouTubeEmbedURLVal
          <*> tabSpotifyEmbedURLVal
          <*> tabCreationYearVal
          <*> tabAlbumNameVal
          <*> tabAlbumLinkVal
      widget = $(widgetFile "tab/edit_form")

  return (tabEditFormValuesVal, widget)
  where
    youtubeEmbedURL = fmap extraTabDataSourcesYouTubeEmbedURL (tabExtraDataSources tab)
    spotifyEmbedURL = fmap extraTabDataSourcesSpotifyEmbedURL (tabExtraDataSources tab)

tabNewForm :: Html -> MForm Handler (FormResult TabContributionFormValues, Widget)
tabNewForm extra = do
  (artistNameVal, artistNameView) <- emptyRequiredInputField
  (songNameVal, songNameView) <- emptyRequiredInputField
  (versionVal, versionView) <- emptyRequiredPositiveIntField
  (legendVal, legendView) <- emptyOptionalCodeTextareaField
  (tabKeyVal, tabKeyView) <- emptyOptionalInputField
  (tabImageVal, tabImageView) <- emptyOptionalImageFileField
  (tabVal, tabView) <- emptyRequiredCodeTextareaField
  (tabTuningVal, tabTuningView) <- emptyOptionalInputField
  (tabGenreVal, tabGenreView) <- emptyOptionalCodeTextareaField
  (tabYouTubeEmbedURLVal, tabYouTubeEmbedURLView) <- emptyOptionalCodeInputField
  (tabSpotifyEmbedURLVal, tabSpotifyEmbedURLView) <- emptyOptionalCodeInputField
  (tabCreationYearVal, tabCreationYearView) <- emptyOptionalInputField
  (tabAlbumNameVal, tabAlbumNameView) <- emptyOptionalCodeInputField
  (tabAlbumLinkVal, tabAlbumLinkView) <- emptyOptionalCodeInputField

  let contributeVal =
        TabContributionFormValues
          <$> artistNameVal
          <*> songNameVal
          <*> versionVal
          <*> tabVal
          <*> legendVal
          <*> tabKeyVal
          <*> tabImageVal
          <*> tabTuningVal
          <*> tabGenreVal
          <*> tabYouTubeEmbedURLVal
          <*> tabSpotifyEmbedURLVal
          <*> tabCreationYearVal
          <*> tabAlbumNameVal
          <*> tabAlbumLinkVal

      widget = $(widgetFile "tab/new_form")

  return (contributeVal, widget)
