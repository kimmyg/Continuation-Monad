module Main where
import Control.Monad
import Data.Map

data Cont k v r t = Cont (((t -> r) -> r), [(Map k v)])

instance Monad (Cont k v r) where
  return x = Cont ((\k -> k x), [empty])
  Cont (c, ms) >>= f = Cont ((\k -> c (\x -> let Cont (g, ms') = f x in g k)), (empty:ms))

runContinuation :: (t -> r) -> Cont k v r t -> r
runContinuation k (Cont (c, ms)) = c k

stackLookup :: Ord k => k -> [Map k v] -> [v]
stackLookup k []     = []
stackLookup k (m:ms) = case Data.Map.lookup k m of
  Just v  -> v:(stackLookup k ms)
  Nothing -> stackLookup k ms

wcm :: Ord k => k -> v -> Cont k v r t -> Cont k v r t
wcm k v (Cont (c, (m:ms))) = Cont (c, (insert k v m):ms)

ccm :: Ord k => k -> Cont k v r t -> [v]
ccm k (Cont (c, ms)) = stackLookup k ms

fact :: Int -> Cont String String r Int
fact 0 = return 1
fact n = wcm "fact" (show n) (do
  acc <- fact (n - 1);
  return (n * acc))

fact_tr :: Int -> Int -> Cont String String r Int
fact_tr 0 acc = return acc
fact_tr n acc = wcm "fact" (show n) (do
  fact_tr (n - 1) (n * acc))

main = print $ ccm "fact" (fact_tr 5 1)

{-
main = runContinuation print $ do
  x <- return 5;
  y <- return 6;
  return (x + y)
-}
{-
return 5 >>= \x ->
  return 6 >>= \y ->
    return (x + y)

hypotenuse( x, y ) = sqrt( sum( square x, square y ) )

hypotenuse( x, y, k ) = \a -> \b -> sum( a, b, \x -> sqrt( x, k ) )

hypotenuse x y k = square x \x_sq ->
  square y \y_sq ->
    sum x_sq y_sq \x_sq_plus_y_sq ->
      sqrt x_sq_plus_y_sq k


plus( a, b, k ) = k (a + b)
square( z, k ) = k (z * z)
sqrt( x_sq_plus_y_sq, k ) = k( _sqrt( x_sq_plus_y_sq ) )
-}
