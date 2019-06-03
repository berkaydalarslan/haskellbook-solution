import Control.Applicative
import Control.Monad

newtype Identity a = Identity a deriving (Eq,Show)

instance Semigroup a => Semigroup (Identity a ) where 
    Identity a <> Identity a' = Identity (a <> a')

instance Monoid a => Monoid (Identity a) where 
    mempty = Identity mempty
    mappend = (<>)

instance Functor Identity where 
    fmap f (Identity a) = Identity ( f a)

instance Applicative Identity where 
    pure = Identity
    (Identity f) <*> as = fmap f as

instance Monad Identity where 
    return = pure 
    (Identity a) >>= k = k a


j :: Monad m => m (m a) -> m a
j x = join $ x


l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = fmap f x

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = f <$> x <*> y

a :: Monad m => m a -> m (a -> b) -> m b
a x f =  f <*> x

