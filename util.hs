pollardStep :: (Int -> Int) -> Int -> Int -> Int -> Int
pollardStep f n x y
      | d > 1 = d
      | otherwise = let x1 = f x
                        y1 = f . f $ y
                    in pollardStep f n x1 y1
      where d = gcd n $ abs $ x - y

pollardRhoFact :: Int -> Int
pollardRhoFact n = 
    let f = \x -> rem (x * x + 1) n
        x0 = f 2
        y0 = f . f $ 2
    in pollardStep f n x0 y0