module Main where

import Control.Exception (SomeException(..))

import Lib
import ContinuationUsage
  ( raceWithSum
  , afterAsyncDelay1
  , afterAsyncDelay2
  , afterAsyncDelay3
  )

oldmain :: IO ()
oldmain = do
  let
    xDelay = 500*1000
    yDelay = 1000*1000
    cDelay = 1011*1000
  result <- raceWithSum xDelay yDelay cDelay
  putStrLn $ "result: " ++ (show result)

main1 :: IO ()
main1 = do
  let
    delay = 5*1000*1000
  cont <- afterAsyncDelay1 delay "hello world!"
  --waitLineToRunCont cont
  --let cont2 = cont >>= \i -> putStrLn $ i ++ " some additional text"
  --waitLineToRunCont cont2
  let cont2 = fmap (\i -> i ++ " some additional text") cont
  waitLineToRunCont1 cont2

waitLineToRunCont1 :: Show a => Cont (IO ()) a -> IO ()
waitLineToRunCont1 cont = do
  l <- getLine
  if l == "q"
    then return ()
    else do
      runCont cont $ \v ->
        putStrLn $ "result: " ++ (show v)
      waitLineToRunCont1 cont

-- main2 :: IO ()
-- main2 = do
--   let
--     delay = 5*1000*1000
--   cont <- afterAsyncDelay2 delay "hello world!"
--   --waitLineToRunCont cont
--   --let cont2 = cont >>= \i -> putStrLn $ i ++ " some additional text"
--   --waitLineToRunCont cont2
--   let cont2 = fmap (\i -> i ++ " some additional text") cont
--   waitLineToRunCont2 cont2
-- 
-- waitLineToRunCont2 :: Show a => Cont (IO (Maybe (Either String r))) a -> IO ()
-- waitLineToRunCont2 cont = do
--   l <- getLine
--   if l == "q"
--     then return ()
--     else do
--       runCont cont $ \v -> do
--         case v of
--           Nothing -> putStrLn ">>>still running"
--           Just (Left e) -> putStrLn ">>>error"
--           Just (Right x) -> putStrLn $ ">>>result: " ++ (show x)
--         return v
--       waitLineToRunCont2 cont

main :: IO ()
main = do
  let
    delay = 5*1000*1000
  cont <- afterAsyncDelay3 delay "hello world!"
  --waitLineToRunCont cont
  --let cont2 = cont >>= \i -> putStrLn $ i ++ " some additional text"
  --waitLineToRunCont cont2
  --let cont2 = fmap (\i -> i ++ " some additional text") cont
  waitLineToRunCont3 cont

waitLineToRunCont3 :: Show a => Cont (IO ()) (Maybe (Either SomeException a)) -> IO ()
waitLineToRunCont3 cont = do
  l <- getLine
  if l == "q"
    then return ()
    else do
      runCont cont $ \v -> do
        case v of
          Nothing -> putStrLn ">>>still running"
          Just (Left e) -> putStrLn ">>>error"
          Just (Right x) -> putStrLn $ ">>>result: " ++ (show x)
      waitLineToRunCont3 cont
