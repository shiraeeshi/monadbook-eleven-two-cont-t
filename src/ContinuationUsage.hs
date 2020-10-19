module ContinuationUsage
  (
    raceWithSum
    , afterAsyncDelay1
    , afterAsyncDelay2
    , afterAsyncDelay3
  ) where

import Control.Exception (SomeException(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently, race, async, poll)
import Lib

returnValueAfter :: Int -> a -> IO (Cont a a)
returnValueAfter delay value = do
  threadDelay delay
  return $ return value

sumDelayedValues :: Int -> Int -> IO Int
sumDelayedValues xDelay yDelay = do
  let
    xAction = returnValueAfter xDelay 100
    yAction = returnValueAfter yDelay 2
  (xCont, yCont) <- concurrently xAction yAction
  let
    zCont = do
      x <- xCont
      y <- yCont
      return $ x + y
  return $ runCont zCont id

raceWithSum :: Int -> Int -> Int -> IO (Either String Int)
raceWithSum xDelay yDelay cDelay = do
  let
    cAction = do
      cCont <- returnValueAfter cDelay "didn't have enough time to sum"
      return $ runCont cCont id
  race cAction (sumDelayedValues xDelay yDelay)

----------------------------------

--returnValueAfter :: Int -> a -> Cont r a
--returnValueAfter delay value =
--  let
--    action = do
--      delayThread delay
--      return value

--download :: String -> Cont () String

--do
--  page1 <- download(url1)
--  return $ length page1


--sounds like mutable, imperative
--contThatPrintsMessage5TimesAndThenSucceeds :: String -> r -> Cont r a
--contThatPrintsMessage5TimesAndThenSucceeds msg value = 

afterAsyncDelay1 :: Int -> a -> IO (Cont (IO ()) a)
afterAsyncDelay1 delay value = do
  async1 <- async $ do
    threadDelay delay
    return value
  return $ Cont $ \c -> do
    res <- poll async1
    case res of
      Nothing -> putStrLn "still running"
      Just (Left e) -> putStrLn "error"
      Just (Right x) -> c x

afterAsyncDelay2 :: Int -> a -> IO (Cont (IO (Maybe (Either String r))) a)
afterAsyncDelay2 delay value = do
  async1 <- async $ do
    threadDelay delay
    return value
  return $ Cont $ \c -> do
    res <- poll async1
    case res of
      --Nothing -> putStrLn "still running"
      --Just (Left e) -> putStrLn "error"
      Nothing -> return Nothing
      Just (Left e) -> return $ Just (Left "error")
      Just (Right x) -> c x

afterAsyncDelay3 :: Int -> a -> IO (Cont (IO r) (Maybe (Either SomeException a)))
afterAsyncDelay3 delay value = do
  async1 <- async $ do
    threadDelay delay
    return value
  return $ Cont $ \c -> do
    res <- poll async1
    c res

