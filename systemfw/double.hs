{-# NOINLINE double #-}

double :: (a -> a) -> a -> a
double f x = f (f x)

{-# NOINLINE quadruple #-}

quadruple = double double

main = print (quadruple (+1) 1)

{-
double_rpX :: forall a. (a -> a) -> a -> a
double_rpX
  = \ (@ a_ayq) (f_as1 :: a_ayq -> a_ayq) (x_as2 :: a_ayq) ->
      f_as1 (f_as1 x_as2)

quadruple_rrY :: forall a. (a -> a) -> a -> a
quadruple_rrY
  = \ (@ a_ayv) -> double_rpX @ (a_ayv -> a_ayv) (double_rpX @ a_ayv)
-}
