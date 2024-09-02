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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Application.FormHandling where

import Application.CommandRunner (runSystemCommand)
import ClassyPrelude
import qualified Data.ByteString.Base64
import Data.UUID.V4
import Import.NoFoundation
import System.Directory (getCurrentDirectory)
import Text.Printf (printf)

parseUploadIntoResizedBase64 :: Maybe FileInfo -> DB (Maybe Text)
parseUploadIntoResizedBase64 Nothing = pure Nothing
parseUploadIntoResizedBase64 (Just x) = do
  fileByteString <- fileSourceByteString x

  newUUID <- liftIO nextRandom
  pwd <- liftIO getCurrentDirectory

  let unoptimizedDestination = printf "%s/%s/%s.webp" pwd tempWebDataDirectory (show newUUID)
      optimizedDestination = printf "%s/%s/%s-opt.webp" pwd tempWebDataDirectory (show newUUID)

  writeFile unoptimizedDestination fileByteString

  liftIO $ runSystemCommand "convert" [unoptimizedDestination, "-resize", "400x400!", optimizedDestination]

  newF <- readFile optimizedDestination

  liftIO $ runSystemCommand "rm" ["-rf", unoptimizedDestination, optimizedDestination]

  let byteS = decodeUtf8 (Data.ByteString.Base64.encode newF)
  pure $ Just byteS

parseImageType :: Maybe FileInfo -> DB (Maybe Text)
parseImageType x = pure (fmap fileContentType x)
