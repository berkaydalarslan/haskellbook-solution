import Test.QuickCheck
import Data.Semigroup

data Two a b = Two a b deriving (Eq,Show)

instance (Semigroup a ,Semigroup b) => Semigroup (Two a b ) where 
   Two a b <> Two a' b' = Two ( a <> a')  (b <> b')


instance (Arbitrary a , Arbitrary b) => Arbitrary (Two a b) where 
    arbitrary = do 
    a <-  arbitrary 
    b <- arbitrary 
    return $ Two a b

controlTwo :: (Eq m, Semigroup m) => m-> m -> m -> Bool
controlTwo x y z = x <> (y <> z) == (x <> y) <> z

type IdentityString = Two String String

type IdentityAssoc = IdentityString -> IdentityString -> IdentityString -> Bool

main = quickCheck (controlTwo :: IdentityAssoc)