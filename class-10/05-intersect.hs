{- Пользуясь списком как монадой, вычислите пересечение  заданных списков -}
	
import Control.Monad

intersect :: Eq a => [[a]] -> [a]
intersect [] = []
intersect (a1 : a) = foldr (\acc a2 -> return a2 >>= filter (`elem` acc)) a1 a

{- проверка:
test1
test2 -}

test1 = intersect [[1],[2]]
test2 = intersect [[1,2],[2,3]]
test3 = intersect [[1,2,3],[2,3]]
test4 = intersect [[1,2,3,4],[2,3],[2,3,4]]
test5 = intersect [[1,2],[2,3],[3,4],[4,5]]