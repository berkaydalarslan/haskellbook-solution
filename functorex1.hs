import Test.QuickCheck
import Test.QuickCheck.Function

newtype Identity a = Identity a deriving (Show,Eq) 

instance Functor Identity where 
    fmap  f (Identity a) = Identity (f a)  

{-
functorIdentity :: (Functor f , Eq (f a)) => (a->b) -> f a -> Bool
functorIdentity x = fmap id x == x 
-}

functorIdentity :: (Eq (f a), Functor f) => f a -> Bool
functorIdentity f = fmap id f == f


instance Arbitrary a => Arbitrary (Identity a) where 
    arbitrary = do 
    a <- arbitrary 
    return $ Identity a

functorCompose' :: (Eq (f c), Functor f) => f a-> Fun a b -> Fun b c-> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

functorCompose :: (Eq (f c), Functor f) =>(a -> b)-> (b -> c)-> f a-> Bool
functorCompose f g x = fmap g (fmap f x) == (fmap (g . f) x)

type IdInt = Identity Int
type IntToInt = Fun Int Int 
type IntFC = IdInt -> IntToInt -> IntToInt ->Bool
type FunctorIdentity = IdInt -> Bool

main :: IO () 
main = do 
      quickCheck (functorCompose' :: IntFC)
      quickCheck (functorIdentity:: FunctorIdentity)
