import Data.Monoid
import Data.Semigroup
import Test.QuickCheck


data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial
instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where 
    arbitrary=return $ Trivial


semigroupAssoc :: (Eq m, Semigroup m) => m->m->m->Bool
semigroupAssoc x y z = x <> (y <> z) == (x <> y) <> z

monoidLeftIdentity :: (Eq m , Monoid m ) => m-> Bool
monoidLeftIdentity x = mempty <> x == x

monoidRightIdentity :: (Eq m ,Monoid  m) => m-> Bool 
monoidRightIdentity x = x <> mempty == x


    
    
type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)