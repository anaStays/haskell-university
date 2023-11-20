class CloneAndAppend a where
	cloneAndAppend :: a->a
	cloneAndAppendList :: [a]->[a]
	cloneAndAppendList = map cloneAndAppend

instance CloneAndAppend Int where
	cloneAndAppend = (*2)

instance CloneAndAppend [a] where
	cloneAndAppend l = l ++ l

class MyReverse a where
	myReverse :: a -> a
	
instance MyReverse [a] where
	myReverse l = reverse l 
	
--instance MyReverse Int where
--	myReverse i = (-i)

instance MyReverse Int where
	myReverse = read . reverse . show

--kind сигнатура конструктора типа данных

data Ex1 a b = C1 a | C2 b

type Cont a = a Int

funEx :: Cont [] -> Cont []
funEx = id