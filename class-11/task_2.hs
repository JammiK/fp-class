import Control.Monad.Writer
import System.Environment

eps = 0.000000000001

taylor'' x prev res n
	| abs (res - nex) < eps = return prev
	| otherwise = taylor'' x (prev + nex) nex (n+2)
	where 
		nex = ((-1) * res * x * x) / ((n+1) * (n+2))

sin' x = sin'' (pi*x/180)		
sin'' x = taylor'' x x x 1

cos' x = cos'' (pi*x/180)
cos'' x = taylor'' x 1 1 0
	