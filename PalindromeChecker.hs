import Data.Char

palindrome = do
	print "Please enter a phrase:"
	str <- getLine
	let s = fixString str
	if (isPalindrome s) == True
		then print "yes"
		else print "no"

isPalindrome str = str == reverse str

fixString (x:xs) = if (isSpace x || isPunctuation x) then fixString xs else toLower x:fixString(xs)
fixString [] = []

forever :: IO() -> IO()
forever a = do
	a
	forever a
	
repeatN :: Int -> IO() -> IO()
repeatN 1 a = do a
repeatN n a = do
	a
	repeatN (n-1) a
	
sequen :: [IO a] -> IO[a]
sequen [] = return[]
sequen (a:as) = do
	x <- a
	xs <- sequen(as)
	return (x:xs)