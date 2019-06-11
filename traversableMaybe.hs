import Data.Traversable
data Optional a = Nada | Yep a deriving ( Eq,Show)

instance Functor Optional where 
    fmap _ Nada = Nada 
    fmap f (Yep a) = Yep (f a)

instance Applicative Optional where 
    pure = Yep
    Nada <*> _ = Nada 
    (Yep f) <*> xs = fmap f xs

instance Foldable Optional where 
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a
    foldr f t Nada = t
    foldr f t (Yep a) = f a t

instance Traversable Optional where 
    traverse _ Nada =  pure Nada
    traverse  f (Yep a) = Yep <$> f  a