import Control.Applicative
-- Ввод - вывод и мы опять играем 

-- Создание exe: ghc --make 171222l.hs -o rev4

main:: IO ()
-- main = print "aaa"
--main = print =<< f <$> getChar <*> getChar <*> getChar
--main = putStrLn =<< f <$> getChar <*> getChar <*> getChar
main = msg1 >> getLine >>= read >>= append
	where 
		read file = readFile file >>= putStrLn >> return file
		append file = msg2 >> getLine >>= appendFile file
		msg1 = putStr "Input file: " 
		msg2 = putStr "Input text: " 


f:: Char -> Char -> Char -> String
f a b c = reverse $ [a,b,c]


{-
class RandomGen g where
	next :: g -> (Int, g)
	split :: g -> (g,g)
	getRange :: g -> (Int, Int)
-}