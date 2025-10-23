module MathUtils (truncate') where

truncate' :: Double -> Int -> Double
truncate' x n = fromIntegral (floor (x * t)) / t
    where t = 10^n