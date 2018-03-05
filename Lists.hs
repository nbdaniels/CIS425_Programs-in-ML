ones :: [Integer]
ones = 1 : ones

intList :: Integer -> [Integer]
intList n = n : intList (n + 1)

takeN :: Integer -> [Integer] -> [Integer]
takeN 1 (x:xs) = x : []
takeN n (x:xs) = x : takeN (n-1) xs

evens :: [Integer]
evens = [2, 4 ..]

odds :: [Integer]
odds = [1, 3 ..]

merge :: [Integer] -> [Integer] -> [Integer]
merge x [] = x
merge [] list = list
merge (x:list1) (y:list2) = x : y : merge list1 list2

ci :: [Integer]
ci = [n^3 | n <- [1..]]
cii :: [Integer]
cii = [3^n | n <- [1..]]
ciii :: [Integer]
ciii = merge [0,1..] [n^2 | n <- [0,1..]]
civ :: [Integer]
civ = [-1,-2..]

sift :: Integer -> [Integer] -> [Integer]
sift p [] = []
sift p (n : ns) =
	if (mod n p == 0)
	then sift p ns
	else n : sift p ns

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p : ns) = p : sieve(sift p ns)

primes :: [Integer]
primes = sieve [2,3..]