{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Writer
import System.Environment

import Data.Char
type Args = [Int]

isTrue p = p==1

isValid s (len:p1:p2:p3:_) = do
          return $ length s >= len 
                 && if (isTrue p1) then any isAlpha s else True
                 && if (isTrue p2) then any isNumber s else True
                 && if (isTrue p3) then any isPunctuation s else True


getValidPassword = do
	liftIO $ putStrLn "Введите новый пароль:"
	s <- liftIO getLine
	tell [s]
	params <- ask
	args <- lift ask
	guard (runReader (isValid s params) args)
	return s
 

askPassword = do
	value <- msum $ repeat getValidPassword
	liftIO $ putStrLn "Сохранение в базе данных..."

main = do
	params <- getArgs
	res <- runWriterT (runReaderT (runMaybeT askPassword) (map read params))
	return $ res