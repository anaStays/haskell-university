module Nat where 
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
	
data Maybe1 a = Nothing1 | Just1 a 

-- �������� ������������ �������

--a -> Maybe1 b

pred :: Nat -> Maybe1 Nat
pred Zero = Nothing1
pred (Succ a) = Just1 a



fix:: (a->a)->a
fix f = let x = f x
	in x

data Stream a = a :& Stream a

constStream:: a -> Stream a
constStream a = a :& constStream a

foldNat:: a->(a->a)->Nat->a
foldNat z s Zero = z
foldNat z s (Succ n) = s(foldNat z s n)

--x:: Nat -> a
--x Zero = z
--x (Succ n) = s (x n)

--x::Nat -> a
--x = \nat -> case nat of 
--	Zero -> z
--	Succ n -> s (x n)

--x::Nat->a
--x=(\t -> \nat -> case nat of
--	Zero -> z
--	Succ n -> s (t n)) x

--x::Nat-> a
--x = f x
--	where f = \t -> \nat -> case nat of
--		Zero -> z
--		Succ n -> s (t n)

foldNat1:: a->(a->a)->Nat -> a
foldNat1 z s = fix f 
	where f t = \nat->case nat of
		Zero -> z
		Succ n -> s (t n)
{-
(.)::(b->c)->(a->b)->(a->c)
f.g= \x -> f (g x)
(>>)::(a->b)->(b->c)->(a->c)
-}

{-
class Category  cat where
	id:: cat a a
	(>>):: cat a b -> cat b c -> cat a c
	f>>g = \x -> g (f x)

--f>>id==f
--id >> f == f
--(f >> gg) >> h == f >> (g >> h)


--a -> m b 
class Kleisli m where
	idk :: a -> m a
	(*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
	(+>) :: Kleisli m -> (a -> m b) -> (b -> c) -> (a -> m c)
	f +> g = f *> (g >> idk)
{-
idk *> == f
f *> idk == f
(f *> g) *> h == f *> (g *> h)
-}
-}
-- ����� >> ����� == �����
-- ���� +> ����� == ����
-- ���� *> ���� == ����

