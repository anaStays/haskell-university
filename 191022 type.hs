str = "Hello" :: String
lchar = ['H','e','l','l','o']

-- Они равны, так как типы являются синонимами

type FunIntToInt = Int -> Int -> Int

funSum :: FunIntToInt
funSum = (+)

funAval :: FunIntToInt -> Int -> Int -> Int
funAval f 0 0 = f 1 1
funAval f x y = f x y

type FirstName = String
type LastName = String

showFullName :: FirstName -> LastName -> String
showFullName fn ln = fn ++ " " ++ ln

-- Синонимы не создают новые типы.

fn = "Ivan" :: String
ln = "Petrov" :: String

data Bit = Zero | One
	deriving (Show)
-- конструктор типа данных и конструкторы самих данных(исп-тся при написании кода)

type Bits = [Bit]

zero = Zero

fromIntToBits :: Int -> Bits
fromIntToBits 0 = [Zero]
fromIntToBits 1 = [One]
fromIntToBits x = reverse . fromIntToBitsR $ x
	where
		fromIntToBitsR 0 = [Zero]
		fromIntToBitsR 1 = [One]
		fromIntToBitsR x =
			if even x 
				then Zero : fromIntToBits (x `div` 2)
				else One : fromIntToBits (x `div` 2)

showBit :: Bit -> String
showBit Zero = "0"
showBit One = "1"


data Name = Name FirstName LastName
	deriving (Show)

name = Name "Ivan" "Vasin"

showName :: Name -> String
showName (Name fn ln) = fn ++ " " ++ ln

type MiddleName = String
data FullName = FullName Name | FullNameWithMiddle Name MiddleName | ShortName Char Char LastName
	deriving (Show)

fullName = FullName . Name fn $ ln
fullNameWithMiddle = FullNameWithMiddle (Name fn ln) "Ivanovich"
shortName = ShortName 'I' 'I' ln
 

showFullName' :: FullName -> String
showFullName' (FullName (Name fn ln)) = fn ++ " " ++ ln
showFullName' (FullNameWithMiddle (Name fn ln) mn) = fn ++ " " ++ mn ++ " " ++ ln
showFullName' (ShortName n m ln) = concat [ln, " ", [n], ".", [m], "."]


data Roots = TR Double Double | OR Double | None | Inf
	deriving(Show)

solve :: Double -> Double -> Double -> Roots
solve 0 0 0 = Inf
solve 0 0 _ = None
solve 0 b c = OR ((-c)/b)
solve a b c = if d < 0 then None else if d == 0 then OR (negate b / (2*a)) else TR x1 x2 
	where
		d = b^2 - 4*a*c
		sqrtD = sqrt d
		x1 = (negate b + sqrtD)/(2*a)
		x2 = (negate b + sqrtD)/(2*a)

showRoots :: Roots -> String
showRoots None = "No solutions"
showRoots Inf = "Infinity count of solutions"
showRoots (OR x) = "One solution: " ++ show x
showRoots (TR x1 x2) = "Two solution: " ++ show x1 ++ " " ++ show x2
