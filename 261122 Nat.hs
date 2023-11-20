import qualified Prelude as P
--module Nat where 
data Nat = Zero | Succ Nat
--	deriving(Show)

instance P.Num Nat where
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
	

meanInt ::  P.Int-> P.Int->P.Double
meanInt a b = fromIntegral (a+b)/2
--Ordering

natToInteger :: Nat -> P.Int
natToInteger a = helper 0 a
		where
			helper i Zero = i
			helper i (Succ a) = helper (i+1) a

instance P.Eq Nat where
	(==) a b = if (natToInteger(a) == natToInteger(b)) then True else False 

addNat:: Nat->Nat->Nat
addNat a Zero = a
addNat a (Succ b) = Succ (addNat a b)

foldNat:: a -> (a->a)->Nat->a
foldNat zero succ Zero =zero
foldNat zero succ (Succ b) = succ (foldNat zero succ b)

add1:: Nat->Nat->Nat
add1 = foldNat id (Succ .)

two = Succ(Succ Zero)
three = Succ(Succ(Succ Zero))

data Stream a = a :& Stream a 
-- Поток - бесконечный список без конструктора пустого списка

nats:: Nat -> Stream Nat
nats a = a :& nats (Succ a)

constStream :: a -> Stream a
constStream a = a :& constStream a

head:: Stream a->a
head (x:xs) = x
--tail:: Stream a->Stream
--(!!):: Stream a-> Int->a
--take::P.Int->Stream->[a]

--instance Show a => Show (Stream a)
--	where
--		show xs = showInfinity(show(take 5 xs))
--			where
--				showInfinity x = P.init x P.++ "..."
