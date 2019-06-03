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

