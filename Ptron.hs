module Ptron where

import Utils
import Control.Monad
import Control.Monad.State
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

sgn :: (Num t , Ord t) => t -> Int
sgn x = if x >= 0 then 1 else -1

startWeight :: Vector Double
startWeight = fromList [0.0]

playInstance :: (Vector Double) -> Int -> StateT (Vector Double,Int) IO Int
playInstance xt yt = do
	(wt,mistakes) <- get
	liftIO $ print (wt,mistakes)
	let yt' = alias (2,4) $ sgn $ wt <.> xt
	when (yt' /= yt) $ modify $ \(wt,m) -> (wt + (fromIntegral yt) * xt,m+1)
	return yt'

trainOnSet [] = return ()

trainOnSet (x:xs) = do
	uncurry playInstance x
	trainOnSet xs

set = [(vector [-1.0],-1),(vector [-2.0],-1),(vector[0.5],-1),(vector [0.75],-1),(vector [1.0],1),(vector [2.0],1),(vector [3.0],1),(vector [100.0],1)]

main = evalStateT (trainOnSet set) (startWeight,0)


