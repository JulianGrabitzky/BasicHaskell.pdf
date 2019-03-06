module Peano where

data Peano = Zero | Succ Peano

add :: Peano -> Peano -> Peano
add Zero     m = m
add (Succ n) m = Succ (add n m)

mul :: Peano -> Peano -> Peano
mul Zero     m = Zero
mul (Succ n) m = add (mul n m) m

double :: Peano -> Peano
double x = add x x

square :: Peano -> Peano
square x = mul x x

-- > f h
-- terminate with lo, ro and po,
-- but not with li, ri and pi
f1 x = Succ Zero
h1 = h1

-- > f2 h2 g2
-- terminate with ro,
-- but not with lo
f2 h2 Zero = Zero
h2 = h2
g2 = Zero

-- > f3 g3 h3
-- terminate with lo,
-- but not with ro
f3 Zero h3 = Zero
h3 = h3
g3 = Zero

-- f5 g5 g5 = True
-- g5 = h5
-- h5 = g5
