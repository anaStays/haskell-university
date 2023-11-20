data List a = Empty | Cons a (List a)
	deriving(Show)

list1 = Cons 5 (Cons 3 (Cons 4 Empty))
list2 = Cons 1 . Cons 2 . Cons 3

myHead :: List a -> a
myHead Empty = error "List is empty"
myHead (Cons x xs) = x


showLis :: Show a => List a -> String
showLis Empty = ""
showLis (Cons x xs) = (show x) ++ " " ++ (showLis xs)
reverseLis (Cons x xs) = x

instance Show a => Show (List a) where
	show (Cons x xs) = show x