import Data.Monoid
import Data.Semigroup
import Test.QuickCheck

newtype Identity a = Identity a deriving (Eq,Show)

instance Semigroup a => Semigroup (Identity a) where 
    Identity a <> Identity a' = Identity (a <> a')  

instance Monoid a => Monoid (Identity a) where 
    mempty = Identity mempty 
    mappend = (<>) 

instance Arbitrary a => Arbitrary (Identity a) where 
    arbitrary = do 
    a <- arbitrary 
    return $ Identity a 

rightIdentity :: (Eq m  , Monoid m) =>  m-> Bool 
rightIdentity x = x <> mempty == x

leftIdentity :: (Eq m ,Monoid m) => m -> Bool 
leftIdentity x = mempty <> x == x 

controlAssoIdentity :: (Eq m ,Semigroup m) => m -> m -> m -> Bool
controlAssoIdentity x y z = x <> (y <> z) == (x <> y) <> z

type IdentityAsso = Identity String 

type ControlCheck = IdentityAsso -> IdentityAsso -> IdentityAsso -> Bool

main:: IO ()
main = do
    quickCheck (controlAssoIdentity :: ControlCheck)
    quickCheck (rightIdentity :: IdentityAsso -> Bool)
    quickCheck (leftIdentity :: IdentityAsso -> Bool)
