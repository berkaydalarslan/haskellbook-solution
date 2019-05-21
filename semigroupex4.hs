import Test.QuickCheck
import Data.Semigroup

data Three a b c = Three a b c deriving (Eq,Show)

instance (Semigroup a ,Semigroup b , Semigroup c) => Semigroup (Three a b c)  where 
    Three a b c  <> Three a' b' c' = Three (a <> a')  (b <> b') (c <> c')


instance (Arbitrary a , Arbitrary b , Arbitrary c) => Arbitrary (Three a b c) where 
    arbitrary = do 
    a <- arbitrary 
    b <- arbitrary 
    c <- arbitrary 
    return $ Three a b c

controlThree :: (Eq m , Semigroup m) => m -> m -> m -> Bool
controlThree x y z = x <> (y <> z) == (x <> y) <> z

type IdentityString = Three String String String 
type IdentityAssoc = IdentityString -> IdentityString -> IdentityString-> Bool

main :: IO ()
main = quickCheck (controlThree :: IdentityAssoc)