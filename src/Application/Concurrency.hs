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

module Application.Concurrency where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception as E
import Control.Monad
import Data.Maybe
import Data.Text (split, splitOn, strip)
import Domain.Sorting
import Import.NoFoundation hiding (atomically, newTVarIO)
import Text.Printf (printf)

-- Work is the IO action to be sent to a background thread.
type Work = IO ()

-- SendWork's type is a function that takes a bit of work and sends it to a background thread.
type SendWork = Work -> STM ()

-- Spawn workers creates i background threads as a pool to execute the work that is sent via the returned function.
spawnWorkers :: Int -> IO (SendWork, IO ())
spawnWorkers i
  | i <= 0 = error "Need positive number of workers"
  | otherwise = do
      -- Create the channel through which the jobs are sent, and a counter of running threads.
      workChan <- atomically newTChan
      runCount <- newTVarIO i
      -- Define and start the threads.  These choose to stop gracefully if the
      -- job throws an exception.
      let stop = atomically (writeTVar runCount . pred =<< readTVar runCount)
          die e = do
            id <- myThreadId
            print ("Thread " ++ show id ++ " died with exception " ++ show (e :: ErrorCall))
            stop
          work = do
            mJob <- atomically (readTChan workChan)
            case mJob of
              Nothing -> stop
              Just job -> E.catch job die >> work
      replicateM_ i (forkIO work)
      -- Create a convenience command to stop the threads (which blocks)
      let stopCommand = do
            atomically (replicateM_ i (writeTChan workChan Nothing))
            atomically
              ( do
                  running <- readTVar runCount
                  when (running > 0) retry
              )
      -- Send a closure to submit jobs; this hides the channel so you can't read from it.
      return (writeTChan workChan . Just, stopCommand)

-- A toy command

-- printJob i = do threadDelay (i*1000)
--                 id <- myThreadId
--                 print ("printJob took "++show i++" ms in thread "++show id)
-- demo = do
--  (submit,stop) <- spawnWorkers 4
--  mapM_ (atomically . submit . printJob) (take 40 (cycle [100,200,300,400]))
--  atomically $ submit (error "Boom")
--  stop
