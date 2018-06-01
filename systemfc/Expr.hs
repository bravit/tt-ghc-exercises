{-# LANGUAGE GADTs #-}

data Exp a where
  Zero :: Exp Int
  Succ :: Exp Int -> Exp Int
  Pair :: Exp b -> Exp c -> Exp (b, c)

eval :: Exp a -> a
eval Zero = 0   -- Int   
eval (Succ e) = eval e + 1   -- Int
eval (Pair x y) = (eval x, eval y)   
                             -- (b, c)

{-
eval_rm9
  = \ (@ a_a1cS) (ds_d1C8 :: Exp a_a1cS) ->
      case ds_d1C8 of {
        Zero cobox_a1cU ->
          fromInteger
            @ a_a1cS
            (GHC.Num.$fNumInt
             `cast` ((Num (Sym cobox))_R
                     :: (Num Int :: Constraint) ~R# (Num a_a1cS :: Constraint)))
            0;
        Succ cobox_a1n5 e_aRt ->
          (+ @ Int GHC.Num.$fNumInt (eval_rm9 @ Int e_aRt) (GHC.Types.I# 1#))
          `cast` (Sub (Sym cobox_a1n5) :: (Int :: *) ~R# (a_a1cS :: *));
        Pair @ b_a1nd @ c_a1ne cobox_a1nf x_aSX y_aSY ->
          (eval_rm9 @ b_a1nd x_aSX, eval_rm9 @ c_a1ne y_aSY)
          `cast` (Sub (Sym cobox_a1nf)
                  :: ((b_a1nd, c_a1ne) :: *) ~R# (a_a1cS :: *))
      }
-}

res = eval (Pair (Succ Zero) Zero)   

main = print res

