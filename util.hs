module Util
( pollard_rho_brent
, pollard_rho
) where 
	
pollard_rho_step :: (Integer -> Integer) -> Integer -> Integer -> Integer -> Integer
pollard_rho_step f n x y
      | d > 1 = d
      | otherwise = let x1 = f x
                        y1 = f . f $ y
                    in pollard_rho_step f n x1 y1
      where d = gcd n $ abs $ x - y

pollard_rho :: Integer -> Integer
pollard_rho n = 
    let f = \x -> mod (x * x + 3) n
        x0 = f 2
        y0 = f . f $ 2
    in pollardStep f n x0 y0

pollard_rho_brent_step :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
pollard_rho_brent_step i k n x y
      | d /= 1 && d /= n = d
      | i == k = pollard_rho_brent_step (i+1) (2*k) n x1 x1
      | otherwise = pollard_rho_brent_step (i+1) k n x1 y
      where d = gcd n $ abs $ y - x
            x1 = mod ( x * x - 1) n

pollard_rho_brent :: Integer -> Integer
pollard_rho_brent n = pollard_rho_brent_step 1 2 n 2 2
