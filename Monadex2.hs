import Control.Applicative
import Control.Monad

data Nope a = NopeDatJpg deriving (Eq,Show)

instance Semigroup a => Semigroup (Nope a) where 
    NopeDatJpg <> _ = NopeDatJpg
   
instance Monoid a => Monoid (Nope a) where 
    mempty = NopeDatJpg
    mappend = (<>)

instance Functor Nope where 
    fmap _ _ = (NopeDatJpg)

instance  Applicative Nope where 
    pure = pure NopeDatJpg
    _ <*> _ = NopeDatJpg

instance  Monad Nope where 
    return = pure 
    NopeDatJpg >>= _ = NopeDatJpg