data Point x = Point x x

{-# NOINLINE xCoord #-}

xCoord (Point x _) = x
yCoord (Point _ y) = y

main = print $ xCoord (Point 2 3)

{-
xCoord_rq1 :: forall x. Point x -> x
xCoord_rq1
  = \ (@ x_a1dk) (ds_d1y8 :: Point x_a1dk) ->
      case ds_d1y8 of { Point x1_as6 ds1_d1ye -> x1_as6 }

main :: IO ()
main
  = print
      @ Integer
      GHC.Show.$fShowInteger
      (xCoord_rq1 @ Integer (Main.Point @ Integer 2 3))

-}
