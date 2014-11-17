{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}

import Control.Monad
import Data.List
import Data.Ord

data Student = Student { name::String, age::Int, course::Int, group::Int } deriving (Eq)

instance Show Student
	where
		show (Student name age course group) = name ++ "\n" ++ (show age) ++ "\n" ++ (show course) ++ "." ++ (show group)
		
readS :: FilePath -> IO [Student]
readS path = readFile path >>= (return . (foldr (\x acc -> toStudent x : acc) []) . groupParam3 . lines)
	where
		groupParam3 :: [a] -> [[a]]
		groupParam3 [] = []
		groupParam3 xs = [take 3 xs] ++ (groupParam3 (drop 3 xs))
		toStudent [n, age, course_group] = Student n (read age) (read (take 1 course_group)) (read (drop 2 course_group))
		
writeS :: FilePath -> [Student] -> IO ()
writeS path students = writeFile path (unlines (map show students))

instance Ord Student where
	compare s1 s2 = compare (name s1) (name s2)
	s1 <= s2 = (name s1) <= (name s2)

main = (++) `liftM` readS "s1.txt" `ap` readS "s2.txt" >>= writeS "students.txt" . sortBy (comparing name)
