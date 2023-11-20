type Swap a b = (b,a)

instance Semigroup Int where
	(<>) = (+) 


newtype Sum = Sum Int
	deriving(Show)
newtype Prod = Prod Int
	deriving(Show)

instance Semigroup Sum where
	(<>) (Sum x) (Sum y) = Sum $ (+) x y

instance Semigroup Prod where
	(Prod x) <> (Prod y) = Prod (x*y)

instance Monoid Sum where
	mempty = Sum 0

instance Monoid Prod where
	mempty = Prod 1

class Semigroup a => Monoid1 a where
	mempty1 :: a
	mappend1 :: a->a->a
	mappend1 = (<>)
	mconcat1 :: [a]->a
	mconcat1 = foldl mappend1 mempty1