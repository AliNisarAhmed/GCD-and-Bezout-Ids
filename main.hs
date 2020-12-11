main :: IO ()
main = do 
  print "Hello"
  print $ bezout 2347 123
  print $ bezout 44 21

type Coefficients = ((Int, Int), (Int, Int))

bezout :: Int -> Int -> (Int, Int)
bezout r2 r1 = 
  bezout2 r2 r1 initCoeffs
  where 
    initCoeffs = ( (1, 0), (0, 1) )
    bezout2 r2 r1 c@( (s0, s1), (t0, t1) ) = 
      let
         (q2, r3) = quotRem r2 r1 
         (_, r4) = quotRem r1 r3
         (s2, t2) = newCoeffs c q2
         newC = ( (s1, s2), (t1, t2) )
      in 
        if r4 == 0 
          then (s2, t2)
          else bezout2 r1 r3 newC

newCoeffs :: Coefficients -> Int -> (Int, Int)
newCoeffs ((s1, s2), (t1, t2)) q = (s1 - s2 * q, t1 - t2 * q)