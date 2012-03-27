module Main where
import Control.Monad
import Data.Map

data Cont r t = Cont ((t -> r) -> r)

instance Monad (Cont r) where
  return x = Cont (\k -> k x)
  Cont c >>= f = Cont (\k -> c (\x -> let Cont g = f x in g k))

runContinuation :: (t -> r) -> Cont r t -> r
runContinuation k (Cont f) = f k

--wcm :: Ord k -> k -> v -> Cont r -> Cont r


main = runContinuation print $ do
  x <- return 5;
  y <- return 6;
  return (x + y)

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
