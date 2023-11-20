data List a = Empty | Cons a (List a)
	deriving(Show)

data Test = Test Int [Int]
fun :: Test -> Int
--fun (Test a (x:xs)) = x
fun (Test a (x:y:xs)) = y
data List2 = Empty2 | Cons2 Test List2

fun2 (Cons2 (Test a (x:xs)) b) = x
