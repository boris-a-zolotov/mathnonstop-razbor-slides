import Data.List

type Quot = (Integer,Integer)

quotMult :: Quot -> Quot -> Quot
quotMult (a,b) (c,d) = (a*c , b*d)

quotSubt :: Quot -> Quot -> Quot
quotSubt (a,b) (c,d) = (a*d - b*c , b*d)

uToA :: Quot -> Integer
uToA (a,b) = ceiling ((fromIntegral b) / (fromIntegral a))

engel :: Quot -> [Integer]
engel (0,x) = []
engel u = let a = uToA u in
  a:(engel (  (quotMult u (a,1)) `quotSubt` (1,1)  ))

main=do
  putStrLn "enter chislitel  "
  input1 <- getLine
  putStrLn "enter znamenatel  " 
  input2 <- getLine 
  let a = (read input1 :: Integer)
  let b = (read input2 :: Integer)
  print $ engel (a , b)
  main

