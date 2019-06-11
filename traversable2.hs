import Data.Traversable

data Identity a = Identity  a   deriving (Eq,Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)


instance Applicative Identity where 
    pure = Identity
    Identity f <*> xs = fmap f xs 


instance Foldable Identity where 
    foldMap f (Identity a) = f a
    foldr f t (Identity a) = f a t

instance Traversable Identity where 
    traverse f (Identity a) = Identity <$> f a