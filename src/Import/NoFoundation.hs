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

{-# LANGUAGE CPP            #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Import.NoFoundation
  ( module Import,
    DB,
    applyIfNonEmptyOrNotFound,
  )
where

import           ClassyPrelude         as Import hiding (Handler, delete,
                                                  deleteBy, fromList)
import           ClassyPrelude.Yesod   as Import hiding(fromList)
import           Data.Kind             (Type)
import           Data.Maybe            as Import hiding (catMaybes)
import           Database.MongoDB      (MongoContext)
import           Import.Utils          as Import
import           Model                 as Import
import           Settings              as Import
import           Settings.StaticFiles  as Import
import           Text.Slugify          as Import
import           Yesod.Auth            as Import
import           Yesod.Core.Types      as Import (loggerSet)
import           Yesod.Default.Config2 as Import
import Data.List.NonEmpty as Import (NonEmpty, nonEmpty, fromList)

-- | A convenient synonym for database access functions.
type DB a =
  forall (m :: Type -> Type).
  (MonadIO m, MonadResource m) =>
  ReaderT MongoContext m a

applyIfNonEmptyOrNotFound :: MonadHandler m => (NonEmpty a1 -> m a2) -> [a1] -> m a2
applyIfNonEmptyOrNotFound f rs =
  maybe
  notFound f (nonEmpty rs)
