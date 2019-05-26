import Test.QuickCheck
import Test.QuickCheck.Function

data Three' a b = Three' a b b deriving (Eq,Show)
data Sum a b = First a | Second b deriving(Eq,Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance (Num a,Num b) => Num (Sum a b) where
    First a + First b = First (a+b)
    

data Berkay a b = Berkay a b deriving (Eq,Show)
data Ky a b = Ky a b deriving (Eq,Show)

instance Functor (Three' a) where  
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a , Arbitrary b) => Arbitrary (Three' a b ) where 
    arbitrary = do 
    a <-arbitrary
    b <- arbitrary 
    return $ Three' a b b


functorIdentity :: (Eq(f a), Functor f) => f a -> Bool
functorIdentity x = fmap id x == x

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g.f)x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int 
type IdTh = Three' Int Int 
type FunctorIdentity = IdTh -> Bool
type FunctorCompose  = IdTh -> IntToInt -> IntToInt -> Bool

main:: IO ()
main = do 
    quickCheck (functorCompose :: FunctorCompose)
    quickCheck (functorIdentity :: FunctorIdentity)
