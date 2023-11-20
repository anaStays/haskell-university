fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = helper 0 1 n
	where
		helper pp p n
			| n == 0 = pp
			| otherwise = helper p (p + pp) (n-1)

fst'(x, _, _) = x
snd'(_, x, _) = x
thd'(_, _, x) = x

a = [1, 2, 3]
b = [1..10]
c = [0, 2..10]
d = [10, 9..1]
e = [(-10)..(-1)]
f = [0.1, 0.2 .. 1] :: [Float]
i = [1..]

m = [ x^y | x <- [1..10], y <- [2,3], even x ]
mm = [ x^y | x <- [1..10], y <- [1..10], even x, y < 4, y > 1 ]

--Сгенерировать список в котором будем -5 до -1 от 1 до 5 из каждой пары взять максимум между квадратом отрицательного и обычного числа
maxm x y = if x > y then x else y

mmm = [ maxm (x^2) y | x <- [(-5) .. (-1)], y <- [1..5], x<(-2), y<3]

--length 

head' (x:_) = x
tail' (_:xs) = xs


--length' (x:xs) = helper 0 len (x:xs)
--	where 
--		helper st len
--			| tail' (x:xs) ==

length' [] = 0
length' (x:xs) = 1 + length' xs

maximum' [x] = x
maximum' (x:xs) = if x > maximum' xs then x else maximum' xs

--take 5 [2..]
--[2,3,4,5,6]
--[1,2]++[3,4]
--[1,2,3,4]

--Доделать take			if n > 1 then x:(take' n-1 xs) else if n > 0 []


--take' n (x:xs) = if n > 1 then x:(take' (n-1) xs) else if n > 0 then x:[] else []

--head' (x:_) = x
--tail' (_:xs) = xs

take' _ [] = []
take' n (x:xs) = if [x]==[] 
	then [] 
	else if [x]==xs
		then take' 1 xs
		else if n > 1 then x:(take' (n-1) xs) else if n > 0 then [x] else []
