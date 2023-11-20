reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse'(xs) ++ [x]

elem' :: (Eq a) => a->[a]->Bool
elem' el [] = False
elem' el (x:xs) 
	| el == x = True
	| otherwise = elem' el xs

--можно задавать список функций

myfoldl :: (b->a->b) -> b -> [a] -> b
myfoldl f ini [] = ini
myfoldl f ini (x:xs) = myfoldl f (f ini x) xs 


myfoldr :: (a->b->b) -> b -> [a] -> b
myfoldr f ini [] = ini
myfoldr f ini (x:xs) = f x (myfoldr f ini xs) 

revfoldl :: [a]->[a]
revfoldl = foldl (\l x -> x:l) [] 
--revfoldl = foldl (flip (:)) [] 

--Композиция функций f(g(x))
--succ
-- выполнится сначала все, что правее $, а только затем выполнится применение функции

f=(\x-> if even x then div x 2 else x ) . succ. sum . reverse . map (^2)

--zip объединяет 2 списка в список кортежей 
--zipwith (+)
--curry - сворачивает 2 параметра в 1 параметр
--uncurry - делает обратную работу




