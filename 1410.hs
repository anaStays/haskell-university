bad:: [Int] -> Int -> Int
bad [] c = c
bad (_ : others) c = bad others $ c + 1

-- $! - заставляет вычилить сразу

good:: [Int] -> Int -> Int
good [] c = c
good (_ : others) c = good others $! c + 1
 
-- undefined выражение, которые имеет любой тип 
-- если допать ~ перед аргументом на вход, то всегда будет удовлетворять

fun :: Num a1 => [a2]->[a1]
fun (x:xs) = [1,2]
fun [] = []

fun1 n = if even n then n else undefined

fun2 [] = []
fun2 l@(x:xs) = x : l

x = [1..10] !! 5


list = zip l1 l2
	where 
		l1=[1..]
		l2 = map(^2) l1

funsum :: Int->Int
funsum n = sum . map snd . take n $ list

