module Kleisli where
import Prelude hiding (id, (>>), (*>), (+>), pred, ($), (*$), (+$),sequence)
class Category cat where
	id :: cat a a
	(>>) :: cat a b -> cat b c -> cat a c

class Kleisli m where
	idk :: a -> m a
	(*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

(+>) :: Kleisli m => (a -> m b) -> (b -> c) -> (a -> m c)

f +> g = f *> (g >> idk) 

instance Category (->) where
	id = \x -> x
	f >> g = \x -> g (f x)

data Nat = Zero | Succ Nat
	deriving(Show)

instance Num Nat where
	(+) a Zero = a
	(+) a (Succ b) = Succ (a+b)
	(*) a Zero = Zero
	(*) a (Succ Zero) = a
	(*) a (Succ b) = a+(a*b)
	fromInteger 0 = Zero
	fromInteger n = Succ(fromInteger(n-1))
	abs x =x
	signum Zero = Zero
	signum _ = Succ Zero
	negate = error "negative is undefined"

pred :: Nat -> Maybe Nat
pred Zero = Nothing
pred (Succ a) = Just a

{-
instance Kleisli Maybe where 
	idk = Just
	f *> g = \a -> case f a of
		Nothing -> Nothing 
		Just b -> g b
-}

maybe1:: b->(a->b)-> Maybe a -> b
maybe1 n f Nothing = n
maybe1 n f (Just x) = f x


instance Kleisli Maybe where
	idk = Just
	f *> g = f >> maybe1 Nothing g

--Алгоритм Линдермайера (L-системы)
--аксиомы:
--a->ab
--b->a
--
--a ab aba abaab

next:: Char ->String
next 'a' = "ab"
next 'b' = "a"


instance Kleisli [] where
	idk = \a -> [a]
	f *> g = f >> map g >> concat

generate:: Int -> (a->[a])->(a->[a])
generate 0 f = idk
generate n f = f *> generate (n-1) f

gen n = generate n next 'a'

($):: (a->b)->a->b
f $ a = (const a >> f) ()

(*$):: Kleisli m => (a-> m b)-> m a -> m b
f *$ a = (const a *> f) ()
(+$):: Kleisli m =>  (a->b)-> m a-> m b
f +$ a = (const a +> f) ()

infixr 0 +$, *$, $

three = Succ(Succ(Succ Zero))
pr1 = pred *$ pred *$ idk three

pr2 = pred *$ pred *$ idk Zero
pr3 = next *$ next *$ idk 'a'
pr4 = next *$ tail $ next *$ reverse $ next *$ idk 'a'


--lift1 :: (a'->b')->m' a'->m' b'
--f:: (a->b->c)
--a:: m a
--lift1 f a :: m (b->c)

--lift1 $
--liftN N-арность

($$) :: Kleisli m => m (b->c) -> m b -> m c
mf $$ ma = (+$ ma) *$ mf 

lift2 :: Kleisli m => (a->b->c)-> m a -> m b -> m c
lift2 f a b = f' $$ b
	where
		f' = f +$ a

sequence :: Kleisli m => [m a] -> m [a]
sequence = foldr (lift2 (:)) (idk [])

sq1 = sequence [Just 1, Just 2, Just 3]
sq2 = sequence [Just 1, Nothing]
sq3 = sequence [[1,2,3],[11,22]]

mapk :: Kleisli m => (a -> m b) -> [a] -> m [b]
mapk f = sequence . map f
