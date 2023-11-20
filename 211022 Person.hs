data Name = Name String String

data Person = Person {name :: String, age :: Int, sex :: Bool, hight :: Int}
	deriving(Show)

person = Person {name = "Anna", age = 24, sex = True, hight = 165}
-- Можно вводить в любом порядке, используя метки


getName :: Person -> String
getName (Person name _ _ _) = name

getAge :: Person -> Int
getAge (Person _ age _ _) = age


getSex :: Person -> Bool
getSex (Person _ _ sex _) = sex

getHight :: Person -> Int
getHight (Person _ _ _ hight) = hight

-- person2 = Person {name = "Anna"}

person3 = Person (name person) (age person + 1) (sex person) (hight person)


person4 = person {age = 25}


lfun = [(+) 0,(-) 0, div 1, mod 1]
l n lf = [x n | x <- lfun]


fun3 x y z = x - y + z

-- ( (take 3 .) . (. map (^2)) ) (map (+1)) [1..10]

data Point a = Point a a
	deriving(Show)

pointInt = Point 5 5 :: Point Int
pointDouble = Point 5.1 5.2 :: Point Double
pointChar = Point "f" "c"

funSum :: (Num a) => Point a -> a
funSum (Point x y) = x + y