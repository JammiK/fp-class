import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)

{- Напишите парсер для вещественных чисел. -}

lngth 0 = 0
lngth x = 1 + lngth (x `div` 10)

sign' = (char '-' >> return (-1)) <|> return 1

float :: Parser Float
float = ((*) <$> sign' <*> floatNumber) <|> fromIntegral <$> integer
	where
		point = char '.' >> return 1
		floatNumber = do
			x1 <- natural
			char '.'
			x2 <- natural
			return $ fromIntegral x1 + (fromIntegral x2 / 10 ^ lngth x2)
		
{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}
complex :: Parser (Float, Float)
complex = bracket "(" ")" $ do
	x <- token float
	char ','
	i <- token float
	return (x, i)

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = bracket "[" "]" $ sepBy (token complex) (symbol ";")

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = bracket "[" "]" $ sepBy (token complex <|> toComplex) (symbol ";")
	where
		toComplex = do
			val <- token float
			return (val, 0)

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = bracket "[" "]" $ sepBy (token complex <|> toComplex) (symbol ",")
	where
		toComplex = do
			val <- token float
			return (val, 0)

