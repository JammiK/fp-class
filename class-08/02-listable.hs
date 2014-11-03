{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}

class Listable a where
	toList :: a -> [a]
	fromList :: [a] -> a
{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integer - любое целое число разбивается на список цифр.
-}
instance Listable String where
	toList = words
	fromList = unwords
	
instance Listable Integer
	where
		toList x = reverse $ digits x
			where
				digits x
					| x == 0 = []
					| otherwise  mod x 10 : (digits $ div x 10)
		fromList = read . unwords . map (show)
