{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}



class Median m where
	median:: m -> m
	toM:: m -> [Int]
	numberMedian:: m -> Int
	numberMedian m = div (length . toM $ m) 2 + 1

instance Median Int where
	median m = div m 2
	toM m = helper m []
		where
			helper 0 s = s
			helper m s = helper (div m 10) ((mod m 10):s)

newtype MyString = MyString String
	deriving(Show)

instance Median MyString where
	toM m = helper m [] 0
		where
			helper (MyString "") s  _ = reverse s
			helper (MyString (x:xs)) s n = helper (MyString xs) (n:s) (n+1) 
	median m = helper m 0 ((numberMedian m)-1)
		where
			helper (MyString (x:xs)) tpos posneed
				|tpos == posneed = (MyString (x:[]))
				|otherwise = helper (MyString xs) (tpos+1) posneed 

----------
--дерево в список
--высота дерева
--Реализовать классы типов: Semigroup, Monoid, таким 
--образом чтобывыполнялось условие: 
--toList (mappend tree1 tree2) == mappend (toList
--tree1) (toList tree2).

data Tree a = Empty | Node a (Tree a) (Tree a)
	deriving(Show, Eq)

insertToTree:: (Ord a) => a -> (Tree a) -> (Tree a)
insertToTree a Empty = Node a Empty Empty
insertToTree a (Node i tl tr)
	|a>i = Node i (insertToTree a tl) tr 
	|a<i = Node i tl (insertToTree a tr) 
	|otherwise = Node i tl tr
toList:: (Tree a) -> [a]
toList Empty = []
toList (Node t left right) = t:(toList (left))++(toList (right))

findHeightTree:: (Tree a) -> Int
findHeightTree Empty = 0
findHeightTree (Node _ left right) = (max (findHeightTree left) (findHeightTree right)) + 1

instance Num a => Semigroup (Tree a) where
	(<>) Empty (Node e f g) = Node e f g
	(<>) (Node b c d) Empty = Node b c d
	(<>) Empty Empty = Empty
	(<>) (Node b c d) e = Node b c ((<>) d e)

instance Num a => Monoid (Tree a) where
	mempty = Empty

tree1 = Node 1 (Node 5 (Node 9 Empty Empty) Empty) (Node 7 Empty Empty) 
tree2 = Node 4 (Node 3 Empty Empty) (Empty) 
tree3 = Node 1 (Node 2 (Node 8 Empty Empty) Empty) Empty
res = (toList (mappend tree1 tree2)) == (mappend (toList tree1) (toList tree2))
res2 = (toList . mconcat $ [tree1, tree2, tree3]) == concat [toList(tree1), toList(tree2), toList(tree3)]
-----------

-- +, -, *, /, ^, cos, sin 

isDelim:: Char -> Bool
isDelim c
	|' ' == c = True
	|otherwise = False

isOp:: Char -> Bool
isOp c
	|c=='+' || c=='-' || c=='*' || c=='/' || c=='^' = True
	|otherwise = False

isUnOp:: String->Bool
isUnOp s
	|s=="cos" || s=="sin" = True
	|otherwise = False



isDigit:: Char -> Bool
isDigit c
	|c >= '0' && c<= '9' = True
	|otherwise = False


data Tokens = BracketOp | BracketCl | Plus | Minus | Mult | Div | Degree | Cos | Sin | Digit Float
	deriving(Show)


priority:: Tokens -> Int
priority Cos = 4
priority Sin = 4
priority Degree = 3
priority Mult = 2
priority Div = 2
priority Plus = 1
priority Minus = 1 
priority _ = 0

--правая
ass:: Tokens->Bool
ass Degree = True
ass _ = False


toFloat:: String->Float
toFloat = read

strEq:: String->String->Bool
strEq (x:y:z) (a:b:c) = a==x && y==b && z==c
strEq _ _ = False


helper2 (x:y:xs) res flag = reverse (y:x:res)
helper2 _ _ _ = error "error1"


getTokens:: String -> [Tokens]
getTokens str = helper str []
	where
		helper "" res = reverse res
		helper (x:xs) res
			|x == '(' = helper xs (BracketOp:res)
			|x == ')' = helper xs (BracketCl:res)
			|x == '+' = helper xs (Plus:res)
			|x == '-' = helper xs (Minus:res)
			|x == '*' = helper xs (Mult:res)
			|x == '/' = helper xs (Div:res)
			|x == '^' = helper xs (Degree:res)
			|x == 'c' = if (strEq ((helper2 xs [x] 0)) ("cos")) then helper (helper3 xs) (Cos:res) else error "errorCos"
			|x == 's' = if (strEq ((helper2 xs [x] 0)) ("sin")) then helper (helper3 xs) (Sin:res) else error "errorSin"
			|x == ' ' = helper xs res
			|x >= '0' && x<='9' = helper (findTail xs) ((Digit (toFloat . reverse $ (findDigit xs [x] 0))):res)
			|otherwise = error "error"
				where
					findDigit [] res flag = res
					findDigit (x:xs) res flag
						|x >= '0' && x<='9' = findDigit xs (x:res) flag
						|x == '.' && flag/=1 = findDigit xs (x:res) 1
						|x == '.' && flag==1 = error "error"
						|otherwise = res
					findTail [] = []
					findTail (x:xs)
						|x >= '0' && x<='9' = findTail xs 
						|x == '.' = findTail xs
						|otherwise = (x:xs)
					helper3 (x:y:xs) = xs

getPol:: [Tokens]->[Tokens]
getPol [] = []
getPol (x:xs) = helper (x:xs) [] []
	where
		helper ((Digit a):xs) res st = helper xs ((Digit a):res) st
		helper (Cos:xs) res st = helper xs res (Cos:st)
		helper (Sin:xs) res st = helper xs res (Sin:st)
		helper (BracketOp:xs) res st = helper xs res (BracketOp:st)
		helper (BracketCl:xs) res (BracketOp:st) = helper xs res st
		helper (BracketCl:xs) res [] = error "errorBracket"
		helper (BracketCl:xs) res (x:st) = helper (BracketCl:xs) (x:res) st
		helper (a:xs) res [] = helper xs res [a]
		helper (a:xs) res (b:st) = if (priority a < priority b || priority a == priority b && not (ass b)) then helper (a:xs) (b:res) st else helper xs res (a:b:st)
		helper [] res (a:st) = helper [] (a:res) st
		helper [] res [] = reverse res
		helper a b c = error "errorPol"

toInt:: Float->Int
toInt = floor

isInt:: Float -> Bool
isInt a = floor a == ceiling a

getAnswer:: [Tokens] -> Float
getAnswer ts = helper ts 0 []
	where
		helper ((Digit a):xs) res st = helper xs res (a:st)
		helper (Cos:xs) res (a:st) = helper xs res ((cos a):st)
		helper (Sin:xs) res (a:st) = helper xs res ((sin a):st)
		helper (Plus:xs) res (a:b:st) = helper xs res (((+) b a):st)
		helper (Degree:xs) res (a:b:st) = if isInt a then helper xs res (((^) b (toInt a)):st) else error "errorDegree"
		helper (Div:xs) res (a:b:st) = helper xs res (((/) b a):st)
		helper (Mult:xs) res (a:b:st) = helper xs res (((*) b a):st)
		helper (Minus:xs) res (a:b:st) = helper xs res (((-) b a):st)
		helper [] res st = head st
		helper _ _ _ = error "errorAns"

main:: IO ()
main = do
    putStrLn "Enter expression: "
    x <-getLine
    putStrLn ("Result: " ++ (show . getAnswer . getPol . getTokens $ x))
    putStrLn "Continue?(y/n)"
    ans <- getLine
    if ans == "y" then main else putStrLn "Happy End!!!"