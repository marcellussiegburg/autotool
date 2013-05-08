module Network.Counting.Picture where

import Network.Counting.Data

import Autolib.ToDoc
import qualified Data.Map as M
import qualified Data.Array as A

xscale :: Int -> Int
xscale x = 4 * x 

yscale :: Wire -> Int
yscale (Wire y) = 3 * y

picture net = 
    let ws = 1 : wires net
        ylo = minimum ws ; yhi = maximum ws
        ls = levelled net 
        xlo = 0 ; xhi = succ $ maximum $ 1 : map snd ls
        h = do y <- [ylo .. yhi ] 
               horizontal xlo xhi y
        v = do ((up,down),l) <- ls
               vertical_arrow l down up
        bnd = ( (xscale xlo, yscale ylo)
              , (xscale xhi, yscale yhi) )
        a = A.accumArray 
            ( \ o n -> n ) ' ' bnd $ h ++ v
    in
       vcat $ do
            y <- [ yscale ylo .. yscale yhi ]
            return $ text $ do
               x <- [ xscale xlo .. xscale xhi ]
               return $ a A.! (x,y)

horizontal x1 x2 y = do 
        x <- range' (xscale  x1, xscale  x2) 
        return ((x, yscale  y), '-')

vertical_arrow x y1 y2 = 
    do y <- range' (yscale  y1, yscale  y2) 
       return ((xscale  x, y), '|')
    ++ [ ((xscale  x, yscale  y1), 'o')
       , ((xscale  x, yscale  y2), 'o')
       , if y2 > y1 
         then ((xscale  x, pred $ yscale  y2), 'v')
         else ((xscale  x, succ $ yscale  y2), '^')
       ] 


-- | annotate each balancer with its level.
-- levels start at 1.
levelled :: Network -> [(Balancer, Int)]
levelled (Network bs) = 
    helped M.empty bs

helped m [] = []
helped m (b @ (up,down) : bs) =
    let covers = range' (up, down)
        this = succ $ maximum $ do
            c <- covers
            return $ M.findWithDefault 0 c m
        m' = M.fromList $ zip covers $ repeat this 
    in  (b, this) : helped (M.union m' m) bs

range' (x,y) = A.range (min x y, max x y)