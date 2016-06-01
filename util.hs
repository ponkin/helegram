module Util
( pollard_rho_brent
, pollard_rho
) where 
	
pollardStep :: (Integer -> Integer) -> Integer -> Integer -> Integer -> Integer
pollardStep f n x y
      | d > 1 = d
      | otherwise = let x1 = f x
                        y1 = f . f $ y
                    in pollardStep f n x1 y1
      where d = gcd n $ abs $ x - y

pollard_rho :: Integer -> Integer
pollard_rho n = 
    let f = \x -> mod (x * x + 3) n
        x0 = f 2
        y0 = f . f $ 2
    in pollardStep f n x0 y0
	
func :: Integer -> Integer -> Integer
func x n = mod ( x * x - 1) n

pollardStep1 :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
pollardStep1 i k n x y
      | d /= 1 && d /= n = d
      | i == k = pollardStep1 (i+1) (2*k) n x1 x1
      | otherwise = pollardStep1 (i+1) k n x1 y
      where d = gcd n $ abs $ y - x
            x1 = func x n

pollard_rho_brent :: Integer -> Integer
pollard_rho_brent n = pollardStep1 1 2 n 2 2
