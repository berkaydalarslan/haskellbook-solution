import Control.Applicative
import Control.Monad

data Sum a b = First a | Second b deriving (Eq,Show)

instance (Semigroup a , Semigroup b) => Semigroup (Sum a b) where 

  First a <> _ = First a
  _ <> First a = First a
  Second b <> Second b' = Second (b <> b')

instance (Monoid a , Monoid b) => Monoid  (Sum a b) where 
    mempty = First mempty
    mappend = (<>)

instance Monoid a => Functor (Sum a) where 
    fmap  _  (First a) = (First  a)
    fmap f (Second b) = Second (f b)

instance Monoid a => Applicative (Sum a) where 
    pure = Second
    First a <*> _ = First  a
    Second f <*> bs = fmap f bs

instance Monoid a => Monad (Sum a) where 
    return = pure
    Second b >>= k = (k b)
    First a >>= _ = First a
  
  