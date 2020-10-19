module ContTUsage
  (
    --transformedRaceWithSum
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently, race)
import Lib

returnValueAfter :: Int -> a -> IO a
returnValueAfter delay value = do
  threadDelay delay
  return value

--asContT :: a -> ContT b m a
--asContT value = pure value

--sumDelayedValuesInSequence :: Int -> Int -> IO Int
--sumDelayedValuesInSequence xDelay yDelay = do
--  let
--    xContT :: ContT Int IO ()
--    xContT = asContT 100 >>= (returnValueAfter xDelay)
--    yContT :: ContT Int IO ()
--    yContT = asContT 2 >>= (returnValueAfter yDelay)
--    zContT = do
--      x <- xContT
--      y <- yContT
--      return $ x + y
--  runContT zContT ()


--lsdkfnvd = do
--  x_dfdvfdf = makeCont 100 returnValueAfter xDelay
--  y_dfdvfdf = makeCont 100 returnValueAfter yDelay
--  z_dfvddfv = do
--    x <- x_dfdvfdf
--    y <- y_dfdvfdf
--    return $ x + y
