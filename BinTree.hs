data Tree = Empty | Node Int Tree Tree
	deriving(Show, Eq)

insertToTree:: Int->Tree->Tree
insertToTree a Empty = Node a Empty Empty
insertToTree a (Node i tl tr) =
	|a>i = Node a (insertToTree a tl) tr 
	|a<i = Node a tl (insertToTree a tr) 
	|otherwise = Node i tl tr
--Сделать поиск (реализован в другом файле)