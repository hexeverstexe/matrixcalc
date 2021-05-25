import Data.List

normalize :: [[Float]] -> [[Float]]
normalize xss = f (filter (/= 0.0) (map getNonZero (gauss xss))) (gauss xss)
  where
    f [] matrix       = matrix
    f (x:xs) (ys:yss) = map (/x) ys : f xs yss

isNormalized :: [[Float]] -> Bool
isNormalized xss = (echelonForm xss) && (foldr (==) True (map (\x-> x == 1.0 || x == 0.0) (map getNonZero xss)))

getNonZero :: [Float] -> Float
getNonZero []     = 0
getNonZero (x:xs) = if x == 0 then getNonZero xs else x

echelonForm :: [[Float]] -> Bool
echelonForm []  = True
echelonForm xss = foldr (&&) True (zipWith (<) z (tail z))
  where z = map detZeros xss

gauss :: [[Float]] -> [[Float]]
gauss xss = f 0 xss
  where
    f _ [] = []
    f n matrix
     | n >= length (head matrix) = matrix
     | (head matrix) !! n == 0   = f (n+1) matrix
     | otherwise                 = (head (gaussCol (n+1) matrix)) : f (n+1) (tail (gaussCol (n+1) matrix))


--receives n and a matrix and computes the matrix that has only zeros in the nth column except for the first row (done by applying only rowtransformations)
gaussCol :: Int -> [[Float]] -> [[Float]]
gaussCol _ []  = []
gaussCol n xss = f 1 (detGaussFactors (nthCol n xss)) xss
  where f _ [] matrix     = matrix
        f a (x:xs) matrix = f (a+1) xs (alpha a 1 x matrix)


nthCol :: Int -> [[Float]] -> [Float]
nthCol _ [] = []
nthCol n xs = if n == 1 then map head xs else nthCol (n-1) (map tail xs)

--returns a List of factors so if x is multiplied by that factor the corresponding value of the original list would be the additive inverse
detGaussFactors :: [Float] -> [Float]
detGaussFactors []     = []
detGaussFactors (x:xs) = 0 : map (/(-x)) xs



-- amount of leading Zeros
detZeros :: [Float] -> Int
detZeros []     = 0
detZeros (x:xs) = if x == 0 then detZeros xs + 1 else 0

--sorts by amount of leading zeros in descending order
sortLeadingZeros :: [[Float]] -> [[Float]]
sortLeadingZeros = sortBy (\x y -> compare (detZeros x) (detZeros y))



--Fills a list with zeros to reach a certain length
fillWithZeros :: (Fractional a) => Int -> [a] -> [a]
fillWithZeros x ys | x > (length ys) = fillWithZeros (x-1) (ys ++ [0])
                   | otherwise       = ys

--determines the maximum Length of a List in a List of Lists
detMaxL :: [[a]] -> Int
detMaxL []       = 0
detMaxL (xs:xss) = max (length xs) (detMaxL xss)

--Takes an index, an Element and a List and returns the inputlist with the given element at the determined index
replaceElement :: Int -> a -> [a] -> [a]
replaceElement _ _ []     = []
replaceElement 1 y (z:zs) = y:zs
replaceElement x y (z:zs) = z:(replaceElement (x-1) y zs)


-- elementary line transformations

--takes i j k and a List Matrix and adds the k-multiple of row j onto row i
alpha :: (Fractional a) => Int -> Int -> a -> [[a]] -> [[a]]
alpha _ _ _ []  = []
alpha x y z xss = replaceElement x (zipWith (+) (xss !! (x-1)) (map (*z) (xss !! (y-1)))) xss

-- changes the rows of the given indices
tau ::  Int -> Int -> [a] -> [a]
tau x y xs = replaceElement x (xs !! (y-1)) (replaceElement y (xs !! (x-1)) xs)


mu :: (Fractional a) => Int -> a -> [[a]] -> [[a]]
mu x y ys = replaceElement x (map (*y) (ys !! (x-1))) ys

--creates a matrix out of a list of Lists (fills up shorter rows with zeros)
cmatrix :: (Fractional a) => [[a]] -> [[a]]
cmatrix xss = map (fillWithZeros (detMaxL xss)) xss

-- takes m n and a list then partitions list into m lists of length n
listToM :: Int -> Int -> [Float] -> [[Float]]
listToM 0 _ _  = []
listToM _ _ [] = []
listToM m n xs = (take n xs) : listToM (m-1) n (drop n xs)

--test matrices
m1 :: [[Float]]
m1 = cmatrix [[1..4],[2..4],[1,2,5,3]]
