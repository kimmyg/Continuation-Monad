module ContinuationMonad where
import Control.Monad
import Data.Map

data Cont r t = Cont ((t -> r) -> r)

instance Monad (Cont r) where
  return x = Cont (\k -> k x)
  Cont c >>= f = Cont (\k -> c (\x -> let (Cont g) = f x in g k))

--callCC :: ((t->r)->r) Cont r t -> r?

runCont :: (t -> r) -> Cont r t -> r
runCont k (Cont c) = c k
