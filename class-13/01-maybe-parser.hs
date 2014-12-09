{-
   Тип Parser может быть определён следуюшим образом:
-}

import Data.Maybe
import Control.Monad
import Data.Char

newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}

instance Monad Parser where
	return x = Parser (\s -> Just (x,s))
	p >>= q = Parser ( \_s -> case (apply p _s) of
		Nothing -> Nothing
		Just (r, ss) -> apply (q r) ss)
	fail _ = Parser (\_ -> Nothing)

instance MonadPlus Parser where
	mzero = Parser (\s -> Nothing)
	p `mplus` q = Parser (\s -> case (apply p s) of 
					Nothing -> apply q s
					ps -> ps)
