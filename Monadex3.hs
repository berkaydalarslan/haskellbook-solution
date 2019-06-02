import Control.Applicative
import Control.Monad

data PhhhbbtttEither b a = Lefty a | Righty b deriving (Eq, Show)

instance (Semigroup a ,Semigroup b) => Semigroup (PhhhbbtttEither b a) where 
    Righty b <> _ = Righty b
    _ <> Righty b = Righty b
    Lefty a <> Lefty a'  = Lefty (a <> a')


instance (Monoid a , Monoid b) => Monoid (PhhhbbtttEither b a) where 
    mempty = Righty memty
    mappend = (<>)

instance Functor (PhhhbbtttEither b ) where 
    pure = Lefty
    fmap _ (Righty b) = (Righty b)
    fmap f (Lefty a) = Lefty (f a)

instance Applicative (PhhhbbtttEither b) where 
    Righty b <*> _ = Righty b
    Lefty f  <*>  as = fmap f as

instance Monad (PhhhbbtttEither a) where
    return = pure 
    Lefty a >>= k = (k a)
    Righty b >>= _ = Righty b