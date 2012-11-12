{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Monad
import System.Random ( random, newStdGen, StdGen )
import Data.Maybe
import Network.FastCGI
import Data.Time (UTCTime, getCurrentTime)


somethingRandom :: IO String
somethingRandom = do
  gen <- newStdGen
  let (a, _)  = random gen :: (Integer, StdGen)
  return $ show a


dispatchAction :: Maybe String -> [String] -> IO String
dispatchAction (Just "random") _ = somethingRandom
dispatchAction (Just "time") _ = do
    time <- getCurrentTime
    return $ show time

dispatchAction (Just "flipme") items = return . concat $ map reverse items
dispatchAction _ _ = return "Enter something!"

run = do
  action <- getInput "action"
  items <- prepareItems
  a <- liftIO $ dispatchAction action items
  output a
  where 
    prepareItems = do 
      names <- getInputNames
      let f = liftM fromJust
      mapM ( f . getInput ) names

main = runFastCGI . handleErrors $ run