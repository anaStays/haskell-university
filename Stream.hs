import qualified Prelude as P
import Prelude

data Stream a = a :& Stream a 
-- Поток - бесконечный список без конструктора пустого списка

--nats:: Nat -> Stream Nat
--nats a = a :& nats (Succ a)



head:: Stream a->a
head (x :& xs) = x
tail:: Stream a->Stream a
tail (x :& xs) = xs
(!!):: Stream a-> P.Int->a
(!!) (x :& xs) n 
	|n<0 = error "n<0 what do you want"
	|n==0 = x
	|otherwise = (Main.!!) xs (n - 1)
take::P.Int->Stream a-> [a]
take n (x :& xs) = helper (x :& xs) n []
	where
		helper (x :& xs) n res
			|n==0 = res
			|n<0 = error "n<0 what do you want"
			|otherwise = helper xs (n - 1) (x:res)



iterate :: (a->a)-> a -> Stream a
iterate f a = a :& (Main.iterate) f (f a)

constStream :: a -> Stream a
constStream a = (Main.iterate) (\x->x) a
--constStream a = a :& constStream a
--nats = iterate Succ Zero

instance Show a => Show (Stream a)
	where
		show xs = showInfinity(show((Main.take) 5 xs))
			where
				showInfinity x = P.init x P.++ "..."

st = Main.iterate (+1) 1

map :: (a->b)->Stream a -> Stream b
map f (x :& xs) = (f x) :& (Main.map f xs)

filter:: (a->Bool) -> Stream a -> Stream a
filter f (x :& xs)
	|f x = x :& (Main.filter f xs)
	|otherwise = Main.filter f xs

zip:: Stream a -> Stream b -> Stream (a,b)
zip (x :& xs) (y :& ys) = (x,y) :& (Main.zip xs ys)

zipWith :: (a->b->c) -> Stream a -> Stream b -> Stream c
zipWith f (x :& xs) (y :& ys) = (f x y) :& Main.zipWith f xs ys 