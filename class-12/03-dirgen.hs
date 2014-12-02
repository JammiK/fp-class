{-
  Для тестирования программ, работающих с файловой системой, часто необходимо
  достаточно большое дерево каталогов с файлами. Реализуйте случайный генератор
  такого дерева, управляемый набором параметров (минимальная и максимальная ширина
  и глубина дерева, количество и размеры файлов, что-нибудь ещё). В качестве идеи
  для архитектуры приложения используйте пример с подсчётом количества файлов в
  дереве (count.hs). Этот же пример можно использовать для тестирования
  разработанного приложения.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import System.Random
import System.IO.Unsafe
 
data AppConfig = AppConfig {
		cfgMaxDepth :: Int,
		cfgMinDepth :: Int,
		cfgMaxWidth :: Int,
		cfgMinWidth :: Int,
		cfgMinCountFiles :: Int,
		cfgMaxCountFiles :: Int,
		cfgFileSize :: Int
     } deriving (Show)
 
data AppState = AppState {
	  stCurFileNum :: Int,
	  stCurDepth :: Int,
       stCurPath :: FilePath
     } deriving (Show)
 
newtype MyApp a = MyA {
      runA :: ReaderT AppConfig (StateT AppState IO) a
     } deriving (Functor, Applicative, Monad,
                 MonadIO,
                 MonadReader AppConfig,
                 MonadState AppState)
 
runMyApp :: MyApp a -> [Int] -> FilePath -> IO a
runMyApp app [minDep, maxDep, minWidt, maxWidt, minCount, maxCount, file_size] path =
    let config = AppConfig minDep maxDep minWidt maxWidt minCount maxCount file_size
        state = AppState 0 0 path
    in evalStateT (runReaderT (runA app) config) state
 
randomDirName :: Int -> IO String 
randomDirName n = do
	gen <- newStdGen
	return $ take n $ randomRs ('a','z') gen
		
randomFileName :: IO String
randomFileName = do
	gen <- newStdGen
	return $ (take 7 $ randomRs ('a','z') gen) ++ ".txt"
 
randomNumber :: Int -> Int -> IO Int
randomNumber first second = do
	gen <- newStdGen
	return $ fst $ randomR (first, second) gen

createFile :: Int -> String -> IO ()	
createFile n fname = do
	gen <- newStdGen
	writeFile fname $ take n $ randomRs ('a','z') gen


createFiles tFileCount cur_path file_size = do
	forM_ [1..tFileCount] $ \_ -> do	
	     fileName <- liftIO $ randomFileName
	     let filePath = cur_path </> fileName
	     liftIO $ createFile file_size filePath



genFile :: MyApp ()
genFile = do
	minDep <-liftM cfgMinDepth  ask
	maxDep <-liftM cfgMaxDepth  ask
	minWidt <-liftM cfgMinWidth  ask
	maxWidt <-liftM cfgMaxWidth  ask
	minCount <-liftM cfgMinCountFiles  ask
	maxCount <-liftM cfgMaxCountFiles  ask
	
	tFileCount<- liftIO $ randomNumber minCount maxCount
	tDepth <- liftIO $ randomNumber minDep maxDep
	tWidth <- liftIO $ randomNumber minWidt maxWidt

	file_size <-liftM cfgFileSize  ask
	
	st <- get
	let cur_file_name = stCurFileNum st
	let cur_depth = stCurDepth st
	let cur_path = stCurPath st

	when (cur_depth < tDepth) $ do
		createFiles tFileCount cur_path file_size
		
		forM_ [1..tWidth] $ \_ -> do
			dir_name <- liftIO $ randomDirName 5
			let newPath = cur_path </> dir_name
			liftIO $ createDirectory newPath
			let newDepth = cur_depth + 1
			put $ st {stCurDepth = newDepth, stCurPath = newPath, stCurFileNum = cur_file_name + tFileCount}
			genFile


main = do
  x <- getArgs		
  createDirectory $ head x
  result <- runMyApp genFile (map read $ tail x) $ head x
  print result