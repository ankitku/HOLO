module Utils where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

sumV v = v <.> initV (size v) (1.0 :: Double)

initV :: Int -> Double -> Vector Double
initV d v = vector $ replicate d v

alias classes x = if x < 0 then fst classes else snd classes
