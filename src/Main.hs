module Main where

import Control.Monad.Tardis
import Data.Functor.Identity

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

data Term = Add Term Term
          | Mul Term Term
          | Const Int
          deriving (Eq,Show)
type Value = Int

interp0 :: Term -> Identity Value
interp0 (Add f g) = do
  x <- interp0 f
  y <- interp0 g
  return $ x+y
interp0 (Mul f g) = do
  x <- interp0 f
  y <- interp0 g
  return $ x*y
interp0 (Const x) = return x

interp1 :: Term -> Tardis () Int Value
interp1 (Add f g) = do
  x <- interp1 f
  y <- interp1 g
  modifyForwards (+1)
  return $ x+y
interp1 (Mul f g) = do
  x <- interp1 f
  y <- interp1 g
  modifyForwards (+1)
  return $ x*y
interp1 (Const x) = return x

main :: IO ()
main = do
  putStrLn "hello world"
  print $ runTardis ex1 (1,2.0)
  print $ take 10 $ evalTardis ex2 ([],())
  print $ evalTardis (ex3 [1..4]) (0,(0,0))
  let term = Mul (Add (Const 3) (Const 2)) (Const 3)
  print $ runIdentity $ interp0 term
  print $ runTardis (interp1 term) ((),0)

