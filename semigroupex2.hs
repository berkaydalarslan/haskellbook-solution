import Test.QuickCheck
import Data.Semigroup

newtype Identity a = Identity a deriving(Eq,Show)


instance Semigroup a => Semigroup (Identity a)  where 
    (Identity a) <> (Identity a') = Identity (a <> a')

instance Arbitrary a => Arbitrary (Identity a)  where 
    arbitrary = do
        a <- arbitrary
        return $ Identity a 

semiGroupAssoc :: (Eq m , Semigroup m) => m-> m-> m -> Bool
semiGroupAssoc x y z = x <> (y <> z) == (x <> y) <> z

type IdentityString = Identity String


type IdentityAssoc = IdentityString -> IdentityString  -> IdentityString -> Bool


main :: IO ()
main = quickCheck (semiGroupAssoc :: IdentityAssoc)