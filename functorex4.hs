import Test.QuickCheck
import Test.QuickCheck.Function 

data Three a b c = Three a b c deriving (Eq,Show)

instance Functor (Three a b) where 
    fmap f (Three a b c) = Three a b (f c)

data Berkay a = Berkay a deriving (Eq,Show)
instance (Arbitrary c, Arbitrary b, Arbitrary a ) => Arbitrary (Three a b c) where
  arbitrary = do 
  a <- arbitrary
  b <-arbitrary
  c <- arbitrary 
  return $ Three a b c
  
functorIdentity :: (Eq (f a) ,Functor f) => f a -> Bool 
functorIdentity x = fmap id x == x

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g.f) x ) == (fmap g .fmap f $ x)

type IdTh = Three Int Int Int
type IntToInt = Fun Int Int 
type FunctorCompose = IdTh -> IntToInt -> IntToInt -> Bool
type FunctorIdentity = IdTh -> Bool

main :: IO ()
main = do 
     quickCheck (functorCompose ::FunctorCompose)
     quickCheck (functorIdentity :: FunctorIdentity)


