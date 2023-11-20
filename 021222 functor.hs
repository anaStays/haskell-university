import Data.Functor ((<$>))

--type Functor:: (*->*)->Constraint
class Functor1 f where
	fmap1:: (a->b)-> f a-> f b
--(<$):: a-> f b -> f a


instance Functor1 [] where
	fmap1 f = map f

instance Functor1 Maybe where
	fmap1 f Nothing = Nothing
	fmap1 f (Just a) = Just (f a)

instance Functor1 ((->) e) where
	fmap1 = (.)

fun1 = fmap1 length tail

instance Functor1 ((,) l) where
	fmap1 f (l, a) =  (l, f a)

instance Functor1 (Either a) where
	fmap1 f (Left a) = Left a
	fmap1 f (Right b) = Right (f b)

--(a->b->c) -> f a -> f b -> f c

res = fmap (++) ["1","2","3"] 

res2 = (++) <$> ["1","2","3"] <*> ["qwe","rty"]


