module Lib
    ( Cont(..)
      , ContT(..)
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

----------------------------------
newtype Cont r a = Cont { runCont :: (a -> r) -> r }
----------------------------------

instance Functor (Cont r) where
  --fmap f (Cont g) = Cont $ \h -> g (\a1 -> h (f a1))
  fmap a2b (Cont g) = Cont $ \b2r ->
    let a2r = \a1 -> b2r (a2b a1)
    in g a2r

instance Applicative (Cont r) where
  pure x = Cont $ \f -> f x
  (Cont g) <*> (Cont h) = Cont $ \fx2r ->
    g $ \f ->
    h $ \x ->
    fx2r $ f x

instance Monad (Cont r) where
  (Cont g) >>= f = Cont $ \k ->
    g $ \a ->
      let (Cont h) = f a
      in h k

----------------------------------

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

instance Functor (ContT r m) where
  fmap a2b (ContT g) = ContT $ \b2mr ->
    let a2mr = \a1 -> b2mr (a2b a1)
    in g a2mr

instance Applicative (ContT r m) where
  pure x = ContT $ \f -> f x
  (ContT g) <*> (ContT h) = ContT $ \fx2mr ->
    g $ \f ->
    h $ \x ->
    fx2mr $ f x

instance Monad (ContT r m) where
  (ContT g) >>= f = ContT $ \k ->
    g $ \a ->
      let (ContT h) = f a
      in h k
