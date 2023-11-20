--Можно использовать только библиотечные функции библиотеки prelude
--можно использовать функцию error"любой текст"
--import Data.Char пример импортирования модуля
import Data.Char
--Решение квадратного уравнения с вещественными корнями. a, b, c мб = 0
--
findRoots :: Double -> Double -> Double -> [Double]
findRoots 0 0 0 = error "infinity"
findRoots 0 0 _ = error "solution doesn't exist"
findRoots 0 b c = (-c)/b : []
findRoots a b c 
	|d < 0 = error "real roots don't exist"
	|d == 0 = (-b)/a2 : []
	|otherwise = ((- b) - sqrtd)/a2 : ((-b) + sqrtd)/a2 : []
	where
		d = b*b - 2*a2*c
		sqrtd = sqrt d
		a2 = 2*a

----------
--Реализовать функцию sum 'n' count :: Integer -> (Integer, Integer),
--которая подсчитывает сумму цифр заданного числа и их количество

sumCount :: Integer -> (Integer, Integer)
sumCount 0 = (0, 1)
sumCount x = helper (abs x) 0 0
	where
		helper 0 n s = (n, s)
		helper x n s = helper (div x 10) (n+1) (s+(mod x 10))

		--Не работает
--------
--Из заданной строки удаляет все слова, которые содержат хотя бы 1
--заглавную букву. Должна принимать и возвращать строку. Должна
--состоять из композиций встроенных функций.
--

deleteUpWordFromString :: String -> String
--deleteUpWordFromString str = unwords. filter (isLower . head) . words $ str
deleteUpWordFromString str = unwords. filter (all isLower) . words $ str
str :: [Char]
str = "My mum is washing dIshes"
--------

--Найти частичную сумму первых n элементов =1/n^2
--Функция должна быть реализована с использованием функций zip,
--map или zipWith без явного использования рекурсии

--list = map(\x-> 1/(x^2)) [1..]
funsum :: Int->Double
funsum n = sum . take n $ (map(\x-> 1/(x^2)) [1..])

--------
--Удалить из списка каждый третий элемент. Функция 
--должна принимать в качестве параметра список и 
--возвращать список.

deleteEveryThird :: [a] -> [a]
deleteEveryThird [] = []
deleteEveryThird ll = reverse (helper ll 0 [])
	where
		helper [] _ nl = nl
		helper (x:xs) 0 nl = helper xs 1 (x:nl)
		helper (x:xs) 1 nl = helper xs 2 (x:nl)
		helper (x:xs) 2 nl = helper xs 0 nl

-----
--Список из целых чисел, выделить все неубывающие 
--подпоследовательности максимальной длины. 
--Принимает и возвращает список

findSubSeq :: [Int] -> [[Int]]
findSubSeq (x:xs) = helper x (x:xs) [] []
	where
		helper _ [] ll tl = reverse((reverse(tl):ll))
		helper prevx (x:xs) ll tl = if (x >= prevx) then helper x xs ll (x:tl) else helper x xs (reverse(tl):ll) [x]

findMaxLenSeq :: [[Int]] -> Int
findMaxLenSeq ll = helper ll 0 
	where
		helper [] maxlen = maxlen
		helper (l:ls) maxlen = if (length l) > maxlen then helper ls (length l) else helper ls maxlen

findMaxSubSeq :: [Int] -> [[Int]]
findMaxSubSeq ll = filter (\x -> length x == maxlen) lseq
	where
		lseq = findSubSeq ll
		maxlen = findMaxLenSeq lseq


ll = [1,2,3,1,2,3,4,5,4,4,3,1,5,6,9,0,0,0,0,1]::[Int]

--Список из уникальных элементов, найти все перестановки заданного
--списка. Принимает список, возвращает список списков. 

swap [a,b] = [b,a]

findPerm :: (Eq a) => [a]->[[a]]
findPerm [] =[[]]
findPerm l = [a:x | a<-l, x <- (findPerm $ filter (\x -> x /= a) l)]






toM:: String -> [Int]
toM m = helper m [] 0
		where
			helper "" s  _ = reverse s
			helper (x:xs) s n = helper xs (n:s) (n+1) 

numberMedian:: String -> Int
numberMedian m = div (length . toM $ m) 2 + 1

median:: String -> String
median m = helper m "" 0 ((numberMedian m)-1)
		where
			helper (x:xs) res tpos posneed
				|tpos == posneed = x:[]
				|otherwise = helper xs res (tpos+1) posneed 