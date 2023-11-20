--main = putStrLn"Hello world my darling!"
--name x y = x + y
maxm x y = if x > y then x else y

--discVal = (price - minprice)*disc/100

--discEval price minprice disc = (price - minprice)*disc/100

--discount price minprice disc =
--if price < minprice
--then price
--else price-discEval price minprice disc
--where
--discVal = (price - minprice)*disc/100
--discEval price minprice disc = (price - minprice)*disc/100

func ch = if ch=='a' then "10" else if ch=='b' then "11" else "error"

funcIf a b c = if a then b else c

translate ch =
	case ch of
		'A' -> "10"
		'B' -> "11"
		'C' -> "12"
		otherwise -> "error"

translate' 'A'="10"
translate' 'B'="11"
translate' 'C'="12"
translate' _="error"

translate'' val
	|val == 'A' ="10"
	|val == 'B' ="11"
	|otherwise = "error"


translate1 val
	|val == a =x
	|val == 'B' =y
	|otherwise = "error"
	where x = "10"
	      y = "11"
	      a = 'A'

