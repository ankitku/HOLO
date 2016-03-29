{-# LANGUAGE DataKinds #-}

module Winnow where

import Utils
import Control.Monad
import Control.Monad.State
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

sgn :: (Num t , Ord t) => t -> Int
sgn x = if x >= 0 then 1 else -1

d :: Int
d = 9

updateVec :: Double -> Double -> Double -> Vector Double -> Vector Double
updateVec eta zt yt xt = (cmap (\xti -> exp(eta*yt*xti)/zt) xt)

playInstance :: Vector Double -> Int -> Double -> StateT (Vector Double,Double,Int) IO Int
playInstance xt yt eta = do
	(wt,zt,mistakes) <- get
	liftIO $ print (wt,mistakes)
	let yt' = alias (2,4) $ sgn $ wt <.> xt
	if (yt' == yt) then
          modify (\(wt,zt,mistakes) -> let wt' = wt * (updateVec eta zt (fromIntegral yt) xt) in (wt',sumV wt',mistakes))
          else modify (\(wt,zt,mistakes) -> let wt' = (vector [1/zt])*wt in (wt',sumV wt',mistakes+1))
	return yt'

trainOnSet [] _ = return ()

trainOnSet (x:xs) eta = do
	uncurry playInstance x $ eta
	trainOnSet xs eta

set = [(vector [-1.0,1.0],-1),(vector [-2.0,10.0],-1),(vector[0.5,10.0],-1),(vector [0.75,10.0],-1),(vector [1.0,20.0],1),(vector [2.0,5.0],1),(vector [3.0,1.0],1),(vector [100.0,1.0],1)]

eta = 1.1
main = evalStateT (trainOnSet set eta) (initV d 1.0,1.0,0)


