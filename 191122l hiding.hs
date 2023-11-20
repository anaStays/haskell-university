import Prelude hiding (id)
--Безымянные функции используются в композиционном стиле программирования
--Отфильтровать список чисел, выбрав числа >2, <10 и четные
f:: [Int]->[Int]
f = filter p where
	p x = x > 2 && x<10 && even x
f1:: [Int]->[Int]
f1 = filter (\x -> x>2 && x<10 && even x)

--f:: (a->Bool)-> [a]-> [a], x:: (Int->Bool)
--(f x):: [Int]->[Int]

--add a b = (+) a b
--add = (+)

filter1:: (a->Bool)->[a]->[a]
filter1 = \p -> \xs -> case xs of
	[] -> []
	(x:xs)-> let rest = filter1 p xs 
		in if p x then x:rest else rest

square a b c = (\p -> sqrt (p*(p-a)*(p-b)*(p-c))) ((a+b+c)/2)

--Функции высших порядков - функции, которые могут принимать в качестве аргументов функции или возвращать функции

id:: a->a
id x = x
const:: a->b->a
const a _ = a

(&&)::Bool->Bool->Bool
(&&) a = if a then id else (const False)
(||) a = if a then (const True) else id
(.):: (b->c)->(a->b)->a->c
(.) f g = \x->f(g x)
