import Control.Monad.Reader
import System.Environment

act :: String -> Integer -> Integer
act str = do 
	let (key,valTemp) = span (/= '=') str
	let val = (\x -> read x :: Integer) . tail $ valTemp
	make key val
		where
			make "summand" v = (+v)
			make "multiplier" v = (*v)
			make "divisor" v = (`div` v)

main = do
	[pathConf, pathNumb] <- getArgs
	numbs <- (map (\x -> read x :: Integer) . lines) `fmap` readFile pathNumb
	conf <- (map act . lines) `fmap` readFile pathConf
	print $ map (\x -> (runReader $ res x) conf) numbs
		where
			res :: Integer -> Reader [Integer -> Integer] Integer
			res x = do
				actions <- ask
				return $ foldl (flip ($)) x actions
