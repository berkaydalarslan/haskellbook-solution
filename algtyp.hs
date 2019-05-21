module Jammin where
import Data.Char


data Person =
    Person {  name ::String
             , age :: Int}
             deriving (Eq,Show)


data Fruit =
    Peach
    | Plum
    | Apple
    | Blackberry
    deriving (Eq, Show)
{-
data JamJars =
    Jam Fruit Int
         deriving (Eq, Show)
-}


data JamJars = 
    JamJars  { fruitname:: Fruit
                ,  number :: Int}
                deriving (Eq,Show)
 
row1 =  JamJars Peach 10
row2 =  JamJars Apple 20
row3 =  JamJars Blackberry 30
row4 =  JamJars Apple 100
row5 =  JamJars Blackberry 15
row6 =  JamJars Peach 50
allJam = [row1, row2, row3, row4, row5, row6]

numberJar :: JamJars -> Int
numberJar (JamJars x y)= y



totalJar :: [JamJars] -> [Int]
totalJar [] = []
totalJar (xs) = map numberJar xs
{-
class TotalJar1  where
      totalJar1 :: [a]->[Int]

instance TotalJar1 JamJars where 
    TotalJar1 (JamJars x ys) = ys
 -}
{- BARANA SOR-----------
instance (Ord) JamJars where

mostRow:: [JamJars]->JamJars
mostRow xs = foldr (\x y-> if (x>y) then x else y) 0 xs
-}
---------------------BARANA SOR
data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show


data FlowerType = Gardenia
  | Daisy
  | Rose
  | Lilac
   deriving Show

type Gardener = String
{-
data Garden =
    Garden Gardener FlowerType
     deriving Show

-}
{-
data Garden = 
     Gardenia Gardener
    | Daisy   Gardener
    | Rose    Gardener
    | Lilac   Gardener
         deriving Show
-}



data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
    , lang :: ProgrammingLanguage }
    deriving (Eq, Show)



allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
 [ GnuPlusLinux
 , OpenBSDPlusNevermindJustBSDStill
 , Mac
 , Windows
 ]
allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]
 
allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = x , lang = y} | x <- allOperatingSystems , y <- allLanguages]

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
, lang = Haskell }



data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)



mapTree ::(a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =  Node (mapTree f left)  (f a)  (mapTree f right)  


testTree' :: BinaryTree Integer
testTree' =
   Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected =
   Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)


-- acceptance test for mapTree
mapOkay =
   if mapTree (+1) testTree' == mapExpected
     then print "yup okay!"
        else error "test failed!"




preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a]++ (preorder left) ++ (preorder right)


inorder :: BinaryTree a -> [a]
inorder Leaf =[]
inorder (Node left a right) =     (inorder left) ++ [a] ++ (inorder right)




testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)


testPreorder :: IO ()
testPreorder =
   if preorder testTree == [2, 1, 3]
   then putStrLn "Preorder fine!"
   else putStrLn "Bad news bears."
testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
   then putStrLn "Inorder fine!"
   else putStrLn "Bad news bears."




capitalizeWord :: String -> String
capitalizeWord []=[]
capitalizeWord (x:xs)= (toUpper x)  : xs


capitalizeParagraph :: String -> String
capitalizeParagraph [] =[]
capitalizeParagraph (xs) =concat $ map capitalizeWord $ split xs 


split :: String -> [String]
split []=[]
split xs = words xs



