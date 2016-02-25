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

ex3 :: [Double] -> Tardis Double (Double,Int) [Double]
ex3 [] = return []
ex3 (x:xs) = do
  avg <- getFuture
  modifyForwards $ \(s, n) -> (s+x, n+1)
  rest <- ex3 xs
  (s,n) <- getPast
  sendPast $ s/fromIntegral n
  return $ x/avg : rest

-- instance MonadFix m => Monad (TardisT bw fw m) where
--   return x = tardis $ \s -> (x, s)
--   m >>= f  = TardisT $ \ ~(bw, fw) -> do
--     rec (x,  ~(bw'', fw' )) <- runTardisT m (bw', fw)
--         (x', ~(bw' , fw'')) <- runTardisT (f x) (bw, fw')
--     return (x', (bw'', fw''))

-- -- | Modify the backwards-traveling state
-- -- as it passes through from future to past.
-- modifyBackwards :: MonadFix m => (bw -> bw) -> TardisT bw fw m ()
-- modifyBackwards f = do
--   rec
--     sendPast (f x)
--     x <- getFuture
--   return ()

main :: IO ()
main = do
  putStrLn "hello world"
  print $ runTardis ex1 (1,2.0)
  print $ take 10 $ evalTardis ex2 ([],())
  print $ evalTardis (ex3 [1..4]) (0,(0,0))

