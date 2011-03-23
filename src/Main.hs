-- This module presents that:
-- If a Functor which is both Traversable and Monoid is given,
-- the Functor can be monad.
module Main where
import Control.Applicative

-- definition for Traversable
class Applicative t => Traversable t where
        traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
        dist :: Applicative f => t (f a) -> f (t a)
        dist = traverse id

-- list is traversable
instance Traversable [] where
        traverse f [] = pure []
        traverse f (x:xs) = (:) <$> f x <*> (traverse f xs)

-- definition for Monoid
class (Functor m) => Monoid m where
        m_zero :: m a
        m_plus :: m a -> m a -> m a

-- list has monoid structure.
instance Monoid [] where
        m_zero = []
        xs `m_plus` ys = xs ++ ys

-- phantom type for monoid ( x is a phantom type variable)
newtype (Monoid m) => Accy m a x = Acc {acc :: m a}
accy :: (Monoid m) => m a -> Accy m a x
accy ma = Acc ma

-- phantom type for monoid is Functor and Applicative.
instance (Monoid m) => Functor (Accy m x) where
        -- fmap does nothing because Accy is phantom
        fmap _ (Acc ma) = (Acc ma)
instance (Monoid m) => Applicative (Accy m a) where
        pure _ = Acc m_zero
        Acc ma <*> Acc mb = Acc (ma `m_plus` mb)

-- accumulation for Traversable using monoid operation. 
accumulate :: (Traversable t, Monoid t) => (t a -> t b) -> t (t a) -> t b
accumulate f x = acc (traverse (accy.f) x)

-- reduce using accumulate
reduce :: (Traversable t, Monoid t) => t (t a) -> t a
reduce = accumulate id

-- Given a Functor t such that t is both Traversable and Monoid,
-- t can be a Monad.
class (Traversable t, Monoid t) => DerivedMonad t where
        _return :: a -> t a
        _return x = pure x
        (->>=) :: t a -> (a -> t b) -> t b
        x ->>= f = reduce (f <$> x)
instance DerivedMonad []

law1 x f= ((_return x) ->>= f) == (f x)
law2 x = (x ->>= _return == x)
law3 x f g= ((x ->>= f) ->>= g) == (x ->>= (\y -> f y ->>= g))

main::IO()
main = do print "Law1: "
          print $ law1 [1,1] (\x -> [x])
          print "Law2: "
          print $ law2 [2,2]
          print "Law3: "
          print $ law3 [1,1] (\x->[x]) (\x->[x+1])
