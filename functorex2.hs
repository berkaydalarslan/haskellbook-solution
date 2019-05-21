import Test.QuickCheck
import Test.QuickCheck.Function

data Pair a = Pair a a deriving (Eq,Show)


instance Functor Pair where 
  fmap f (Pair a  a')= Pair (f a) (f a')


instance Arbitrary a => Arbitrary (Pair a) where 
    arbitrary = do 
    a <- arbitrary
    return $ Pair a a

functorCompose :: (Eq (f c), Functor f) =>  f a -> Fun a b -> Fun b c -> Bool 
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

functorIdentity :: (Eq (f a) , Functor f) => f a -> Bool 
functorIdentity x = fmap id x == x


type IntToInt = Fun Int Int 
type IdInt = Pair Int  
type FunctorCompose = IdInt -> IntToInt -> IntToInt-> Bool
type FunctorIdentity = IdInt -> Bool 

main :: IO ()
main = do 
       quickCheck (functorCompose :: FunctorCompose )
       quickCheck (functorIdentity ::FunctorIdentity)