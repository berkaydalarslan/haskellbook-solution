import Control.Applicative
--import Test.QuickCheck
--import Test.QuickCheck.Checkers
--import Test.QuickCheck.Classes

data Validation e a = Failure e | Success a deriving (Eq, Show)

instance (Semigroup e ,Semigroup a) => Semigroup (Validation e a) where 
    Failure e <> _ = Failure e
    _ <> Failure e = Failure e
    Success a <> Success a' = Success (a <> a')

instance (Monoid e , Monoid a) => Monoid (Validation e a) where 
    mempty = Success mempty
    mappend = (<>)

instance Functor (Validation e) where 
     
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

instance Applicative (Validation e) where
    pure = Success 
    Failure e <*> _ =  Failure e
    (Success  f ) <*> xs =  fmap f xs

data Berkay = Berkay deriving (Eq,Show)