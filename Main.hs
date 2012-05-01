module Main where
import ContinuationMonad

main = runContinuation print $ do
  x <- return 5
  y <- return 6
  return (x+y)
