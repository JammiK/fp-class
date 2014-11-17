import Control.Monad
import Data.List
import System.Environment

{-
   Дан текстовый файл (inventory.txt)  с перечислением всей имеющейся на складе
   лёгкой брони. Сформируйте список имеющихся полных комплектов брони одного
   вида (kind). Указание: в решении рекомендуется пользоваться монадическими
   операциями всюду, где только возможно.
-}
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment


data ArmorType = Shield | Helmet | Gauntlets | Boots | Cuirass
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorKind = Chitin | Hide | Leather | Elven | Scaled | Glass | ImperialLight
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorItem = ArmorItem ArmorKind ArmorType 
   deriving (Show, Eq)
data ArmorKit = ArmorKit ArmorKind [ArmorType]
   deriving (Show, Eq)

loadInventory :: FilePath -> IO [ArmorItem]
loadInventory path = readFile path >>= return . map (createArmor . words) . lines
	where
		createArmor [kind, t] = ArmorItem (read kind) (read t)

buildArmorKit :: ArmorKind -> [ArmorItem] -> Maybe ArmorKit
buildArmorKit kind armors
	| (length $ getType) == 5 = Just (ArmorKit kind getType)
	| otherwise = Nothing
	where
		getType = map (\(ArmorItem _ i) -> i) $ filter (\(ArmorItem a _) -> a == kind) armors
buildKits :: [ArmorItem] -> Maybe [ArmorKit]
buildKits armors = sequence $ filter (isJust) $ zipWith buildArmorKit [Chitin, Hide, Leather, Elven, Scaled, Glass, ImperialLight] (replicate 7 armors)

main = (head `liftM` getArgs) >>= loadInventory >>= return . buildKits >>= print
