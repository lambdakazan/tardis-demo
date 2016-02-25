module Main where

import Control.Monad.Tardis

ex1 :: Tardis Int Double String
ex1 = do
  a <- getFuture
  sendFuture 0.5
  b <- getPast
  sendPast 10
  return $ show a ++ " " ++ show b

ex2 :: Tardis [Int] () [Int]
ex2 = do
  fibs <- getFuture
  modifyBackwards $ scanl (+) 0
  sendPast $ 1:fibs
  return fibs

main :: IO ()
main = do
  putStrLn "hello world"
  print $ runTardis ex1 (1,2.0)
  print $ take 10 $ evalTardis ex2 ([],())

