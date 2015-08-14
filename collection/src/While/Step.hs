module While.Step where

import While.Type
import While.Memory
import While.State

import qualified RAM.Builtin as B

-- | liste aller direkten nachfolge-zustände

-- die refl-trans. Hülle dieser small-step-Relation
-- ist die big-step-Semantik. Spezifikation:

-- forall f: (memory s0, f) in bigstep (todo s0)
-- <=> let  s1 = step s0 
--   in  (memory s1,f) in bigstep (todo s1)

-- dabei  bigstep (todo s0)
-- = die Sematik von  foldr Seq Skip (todo s0)

step :: State -> [ State ]
step s = case todo s of
  [] -> []
  p : ps -> case p of
    Skip  -> stepped $ update id s 0
    Inc v -> stepped $ update succ s v
    Dec v -> stepped $ update ( \ n -> max 0 (pred n) ) s v
    Assign res f args -> do
      let ( ar, fun ) = B.get f
      let inputs = map ( get (memory s) ) args
      let output = fun inputs
      stepped $ update ( const output ) s res
    Seq p1 p2 ->
      stepped $ s { todo = p1 : p2 : ps }
    IfZ r p1 p2 -> stepped $
      if 0 == get (memory s) r
      then s { todo = p1 : ps }
      else s { todo = p2 : ps }
    Loop v p -> do
      let n = fromIntegral $ get ( memory s ) v
      stepped $ s { todo = replicate n p ++ ps }
    While v q -> stepped $
      if 0 == get ( memory s ) v
        then -- fertig
          s { todo = ps } 
        else -- einmal ausführen, dann nochmal testen
          s { todo = q : p : ps } 

stepped :: State -> [ State ]     
stepped s = return $ s { schritt = succ $ schritt s
                       , past    = s : past s
                       }

-- | wert einer variablen ändern,
-- aktuellen (erledigten) Befehl entfernen
update :: (Integer -> Integer) -> State -> Register -> State
update fun s v = 
    let n = get (memory s) v
    in  s { memory = set ( memory s ) ( v, fun n )
          , todo = tail $ todo s
          }

