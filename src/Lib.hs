module Lib where

import qualified Data.DummyList.Examples
import qualified Data.MyString.Examples

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPES!
data MaritalStatus = Single | Married | Widowed
                   deriving (Show, Read, Eq)

data Gender = Male | Female
            deriving (Show, Read, Eq)

data AcademicTitle = DiS | Bc | Mgr | Ing | PhDr | MUDr | PhD | Doc | Prof
                   deriving (Show, Read, Ord, Eq, Bounded, Enum)

data Person = Person { pFirstname     :: String
                     , pLastname      :: String
                     , pGender        :: Gender
                     , pMaritalStatus :: MaritalStatus
                     , pAge           :: Int
                     , pATitles       :: [AcademicTitle]
                     }

-- | Full czech salutation (in nominative - i.e. první pád)
-- |
-- | "pan doktor Pavel Novák", "paní inženýrka Karolína Šťastná"
-- | "slečna Petra Králová", "Jan" (kid)
-- | if younger than 15 -> kid -> just firstname
-- | if female younger than 25 without academic title and single -> "slečna"
-- | otherwise "pan/paní" depending on the gender
-- | if academic titles, pick the most important (nothing for DiS and Bc)
-- |
-- | https://www.muni.cz/o-univerzite/uredni-deska/oslovovani-akademickych-pracovniku
-- | http://www.etiketavse.estranky.cz/clanky/etiketa/4.-oslovovani-a-spolecenska-vyznamnost.html
-- | http://www.studenta.cz/vysokoskolske-tituly-jak-oslovovat-na-akademicke-pude/magazin/article/587
czechSalutation :: Person -> String
czechSalutation p = genderSalutation p ++ titleSalutation p ++ nameSalutation p

genderSalutation :: Person -> String
genderSalutation (Person _ _ gender status age titles)
                 | age < 15 = ""
                 | age < 25 && gender == Female && status == Single && titles == [] = "slečna "
                 | gender == Male = "pan "
                 | gender == Female = "paní "

titleSalutation :: Person -> String
titleSalutation (Person firstname lastname gender status age titles)
                 | titles == [] = ""
                 | title == Prof && gender == Male = "profesor "
                 | title == Prof && gender == Female = "profesorka "
                 | title == Doc && gender == Male = "docent "
                 | title == Doc && gender == Female = "docentka "
                 | elem title [PhD, MUDr, PhDr] && gender == Male = "doktor "
                 | elem title [PhD, MUDr, PhDr] && gender == Female = "doktorka "
                 | title == Mgr && gender == Male = "magistr "
                 | title == Mgr && gender == Female = "magistra "
                 | otherwise = ""
                 where title = last titles


nameSalutation :: Person -> String
nameSalutation (Person firstname lastname _ _ age _)
                 | age < 15 = firstname
                 | otherwise = (firstname ++ " " ++ lastname)

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPE!
-- https://en.wikipedia.org/wiki/Allen%27s_interval_algebra
-- Notice that even DATA CONSTRUCTOR can be written in infix by using ` `
-- - it is normal, because data constructor is actually function!
--
--                                 X                Y
data AllensIAlgebraRelation a = (a, a) `Equals`   (a, a) -- X = Y
                              | (a, a) `Before`   (a, a) -- X < Y
                              | (a, a) `Meets`    (a, a) -- X m Y
                              | (a, a) `Overlaps` (a, a) -- X o Y
                              | (a, a) `Starts`   (a, a) -- X s Y
                              | (a, a) `During`   (a, a) -- X d Y
                              | (a, a) `Finishes` (a, a) -- X f Y
                             deriving (Show, Read, Eq)

-- | Compare two intervals given as tuples and return appropriate
-- | Allen's Interval Algebra relation between them
-- | It assumes that for (x, y) is always x <= y
-- TODO: implement Allen's algebra relation detection of intervals
allensComparison :: Ord a => (a, a) -> (a, a) -> AllensIAlgebraRelation a
allensComparison = undefined

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPE!
data Shape2D = Circle { ciRadius :: Double }
             | Square { sqSide :: Double }
             | Rectangle { reWidth :: Double, reHeight :: Double }
             | Triangle { triSideA :: Double, triSideB :: Double, triSideC :: Double }
             deriving (Show, Read, Eq)

shapeCircumference :: Shape2D -> Double
shapeCircumference (Circle ciRadius) = 2 * pi * ciRadius
shapeCircumference (Square sqSide) = 4 * sqSide
shapeCircumference (Rectangle reWidth reHeight) = 2 * (reWidth + reHeight)
shapeCircumference (Triangle triSideA triSideB triSideC) = triSideA + triSideB + triSideC

shapeArea :: Shape2D -> Double
shapeArea (Circle ciRadius) = pi * ciRadius * ciRadius
shapeArea (Square sqSide) = sqSide * sqSide
shapeArea (Rectangle reWidth reHeight) = reWidth * reHeight
shapeArea (Triangle triSideA triSideB triSideC) = sqrt (s*(s - triSideA) * (s - triSideB) * (s - triSideC))
                                                         where s = (triSideA + triSideB + triSideC) / 2
-------------------------------------------------------------------------------
-- | Geometric sequence as infinite list
-- | https://en.wikipedia.org/wiki/Geometric_progression
geometricSequence :: Num b => b -> b -> [b]
geometricSequence a r = subseq a r 1
                      where subseq a r n = a * n : subseq a r (r*n)

primes :: [Integer]
primes = filterPrimes (2:[3,5 ..])
  where
    filterPrimes (p:rest) = p : filterPrimes [x|x <- rest, mod x p > 0]

factorization :: Integer -> [Integer]
factorization n = extendPrimes n (uniqueFactorization n)

uniqueFactorization :: Integer -> [Integer]
uniqueFactorization n = filter (lessAndDiv) (take (fromInteger n) primes)
                      where lessAndDiv a = a <= n && mod n a == 0

extendPrimes :: Integer -> [Integer] -> [Integer]
extendPrimes n [] = []
extendPrimes n (p:xs)
              | mod nNext p == 0 = p : extendPrimes nNext (p:xs)
              | otherwise = p : extendPrimes nNext (xs)
              where nNext = div n p

-- | Euler's totient function
-- | https://en.wikipedia.org/wiki/Euler%27s_totient_function
phi :: Integer -> Integer
phi 0 = 0
phi 1 = 1
phi n = round (product ((map eulerFraction primeslist)) * fromInteger (abs n))
       where
             primeslist = (uniqueFactorization (abs n))
             eulerFraction :: Integer -> Double
             eulerFraction p = 1 - ( 1 / fromInteger p)
-------------------------------------------------------------------------------
-- !!! DO NOT COPY, JUST IMPORT (avoid conflicts, pick the best option for you)
-- iii visit the content of modules
dummyListExample1 = Data.DummyList.Examples.example1
stringExample2 = Data.MyString.Examples.example2
stringExample3 = Data.MyString.Examples.example3
