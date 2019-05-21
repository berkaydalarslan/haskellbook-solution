import Test.QuickCheck
import Test.QuickCheck.Function

data Two a b = Two a b deriving (Eq,Show)

instance Functor (Two a) where 
    fmap f (Two a b) = Two a (f b) 

instance (Arbitrary a , Arbitrary b) => Arbitrary (Two a b) where 
    arbitrary = do 
    a <- arbitrary
    b <- arbitrary 
    return $ Two a b 

functorIdentity :: (Eq (f a) ,Functor f) => f a -> Bool 
functorIdentity  x = fmap id x == x 

functorCompose :: (Eq (f c),Functor f) => f a -> Fun b c -> Fun a b -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (f . g) x ) == (fmap f . fmap g $ x)
--functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IdTwo = Two Int Int 
type IntToInt = Fun Int Int 
type FunctorIdentity = IdTwo -> Bool 
type FunctorCompose = IdTwo ->IntToInt -> IntToInt -> Bool 

main :: IO ()
main = do 
       quickCheck (functorIdentity :: FunctorIdentity)
       quickCheck (functorCompose :: FunctorCompose)