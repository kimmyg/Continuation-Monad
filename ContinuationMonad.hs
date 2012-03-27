module Main where
import Control.Monad
import Data.Map

data Cont k v r t = Cont (((t -> r) -> r), [(Map k v)])

instance Monad (Cont k v r) where
  return x = Cont ((\k -> k x), [empty])
  Cont (c, ms) >>= f = Cont ((\k -> c (\x -> let Cont (g, ms') = f x in g k)), ms)

runContinuation :: (t -> r) -> Cont k v r t -> r
runContinuation k (Cont (c, ms)) = c k

--wcm :: Ord k => k -> v -> Cont k v r t -> Cont k v r t
--wcm k v (Cont (c, ms)) = Cont (c, insert k v ms)

--ccm :: Ord k -> k -> Cont k v r t -> Cont k v [v] t
--ccm k Cont (c, ms) = 

fact :: Int -> Cont k v r Int
fact 0 = return 1
fact n = do
  acc <- fact (n - 1);
  return (n * acc)

main = runContinuation print $ fact 5

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
