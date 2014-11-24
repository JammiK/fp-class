import Control.Monad.State
type Queue = [Int]

enqueue x = do
	xs <- get
	put (xs ++ [x])

dequeue = do
	(x:xs) <- get
	put xs
	return x

ex = do
	q <- dequeue
	enqueue q
	enqueue q
	q <- dequeue
	enqueue q
	enqueue q

test = (execState ex [1, 2, 1])
