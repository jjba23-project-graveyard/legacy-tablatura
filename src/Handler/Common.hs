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

-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $
    TypedContent "image/x-icon" $
      toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR =
  return $
    TypedContent typePlain $
      toContent $(embedFile "config/robots.txt")
