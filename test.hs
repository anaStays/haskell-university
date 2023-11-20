test :: Double -> String
test = show 

findRoots :: Double -> Double -> Double -> [Double]
findRoots 0 0 0 = error "infinity"
findRoots 0 0 _ = error "solution doesn't exist"
findRoots a b c = [11]