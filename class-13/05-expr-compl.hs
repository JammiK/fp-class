import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Control.Monad

{-
   Добавьте поддержку вещественных и комплексных чисел из второго упражнения.
   Можете считать, что все числа в выражении являются комплексными (и можете
   не считать, если в состоянии красиво обойтись с типами и всё корректно
   проанализировать).
-}
data Numb = I Int | F Float | Complex (Float, Float)
	deriving Show
data Expr = Con Numb | Bin Op Expr Expr
	deriving Show
data Op = Plus | Minus | Mul | Div
  deriving Show

{-
expr   ::= term {addop term}*
term   ::= factor {mulop factor}*
factor ::= nat | '(' expr ')'
addop  ::= '+' | '-'
mulop  ::= '*' | '/'
-}

expr = token (term >>= rest addop term)
  where
    rest op unit e1 = optional e1 $ do 
        p <- op
        e2 <- unit
        rest op unit $ Bin p e1 e2
    term = token (factor >>= rest mulop factor)
    factor = token (constant <|> bracket "(" ")" expr)
    addop = binop ("+", Plus) ("-", Minus)
    mulop = binop ("*", Mul) ("/", Div)
    binop (s1, cons1) (s2, cons2) =
          (symbol s1 >> return cons1) <|>
          (symbol s2 >> return cons2)
    constant = Con `liftM` ( I `liftM` natural <|> F `liftM` float <|> Complex `liftM` complex )

complex :: Parser (Float, Float)
complex = bracket "(" ")" $ do
	x <- token float
	char ','
	i <- token float
	return (x, i)
	
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
