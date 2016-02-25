module Main where

import Control.Monad.Tardis

ex1 :: Tardis Int Double String
ex1 = do
  a <- getFuture
  sendFuture 0.5
  b <- getPast
  sendPast 10
  return $ show a ++ " " ++ show b

main :: IO ()
main = do
  putStrLn "hello world"
  print $ runTardis ex1 (1,2.0)

