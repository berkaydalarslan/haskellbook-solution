import Data.Monoid 
import Data.Semigroup 
import Test.QuickCheck

newtype BoolConj = BoolConj Bool deriving (Eq,Show)

instance Semigroup BoolConj where 
    BoolConj True <> _ = BoolConj True 
    BoolConj False <> BoolConj False = BoolConj False 
    BoolConj _ <> BoolConj True = BoolConj True 

instance Monoid BoolConj where 
    mempty = BoolConj False 
    mappend = (<>)

instance Arbitrary BoolConj where 
    arbitrary = oneof [return $ BoolConj True , return $ BoolConj False]


rightIdentity :: (Eq m , Monoid m) => m -> Bool 
rightIdentity x = x <> mempty == x

leftIdentity :: (Eq m , Monoid  m) => m -> Bool
leftIdentity x = mempty <> x == x


controlAssocBoolConj :: (Eq m , Semigroup m) => m -> m-> m -> Bool
controlAssocBoolConj x y z = x <> ( y <> z) == (x <> y) <> z 

type IdentityAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
main :: IO ()
main = do 
      quickCheck (controlAssocBoolConj :: IdentityAssoc)
      quickCheck (rightIdentity :: BoolConj -> Bool)
      quickCheck (leftIdentity :: BoolConj ->Bool)