import System.Environment
import System.Random
import Data.Maybe
{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a
	| mod a 3 == 0 = 0
	| odd a = a^2
	| otherwise = a^3

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n a = reduceNF (n - 1) (fmap reduce a)

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList = foldr (\(a,b) acc -> ((a+b)*(a-b)) : acc) []

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe [] = Nothing
toMaybe xs = Just (sum $ toList xs)

toEither :: Integral a => [(a, a)]  -> Either String a
toEither [] = Left "Nothing"
toEither xs = Right (sum $ toList xs)

-- воспользуйтесь в этой функции случайными числами
toIO :: Integral a => [(a, a)]  -> IO a
toIO = undefined

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs [fname, n] = (fname, read n)

readData :: FilePath -> IO [(Int, Int)]
readData = undefined

main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  undefined
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
-}
