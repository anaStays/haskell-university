
quadraticEx a b c =
	let
		x1 = (-b - d)/a2
		x2 = (-b +d)/a2
		d = sqrt $ b^2 - 2*a*c
		a2 = a*2
	in (x1, x2)

findAn 0 = 1
findAn 1 = 2
findAn 2 = 3
findAn n = helper 1 2 3 n

helper f0 f1 f2 0 = f0
helper f0 f1 f2 1 = f1
helper f0 f1 f2 2 = f2
helper f0 f1 f2 n = helper (f1) (f2) (f1 + f2 -2*f0) (n-1) 
--findAn n = findAn (n-2) + findAn (n-1) - 2*findAn (n-3)

findIntegral :: (Double->Double)->Double->Double->Double->Double
findIntegral f a b n = h * ((f a + f b)/2 + res a (n-1) 0)
	where
		h = (b-a) / n
		res _ 0 sum = sum
		res a n sum = res (a+h) (n-1) (f (a) + sum)
	