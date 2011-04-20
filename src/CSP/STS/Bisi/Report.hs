module CSP.STS.Bisi.Report where

import CSP.STS.Type
import CSP.STS.Bisi.Refine ( domain, codomain, images )

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

import qualified Autolib.Relation as R
import qualified Data.Set as S
import Autolib.Set ( cross )
import Control.Monad ( guard, void, forM )


check_bisi (s,ms) (t, mt) r = do
    let miss = S.difference ( states s ) ( domain r )
    when ( not $ S.null miss ) $ reject 
         $ text "Diese Zustände aus" <+> ms <+> text "sind nicht im Vorbereich von R enthalten:"
         </> toDoc miss
    let mist = S.difference ( states t ) ( codomain r )
    when ( not $ S.null mist ) $ reject 
         $ text "Diese Zustände aus" <+> mt <+> text "sind nicht im Nachbereich von R enthalten:"
         </> toDoc mist
    when ( not $ R.holds r (start s) (start t) ) $ reject
         $ text "Es gilt nicht  R" <> parens ( text "start" <+> ms <+> text ", start" <+> mt )
    is_simulated_by (s, ms) (t, mt) ( r, text "R" )
    is_simulated_by (t, mt) (s, ms) ( R.inverse r, text "R^-" )
    
is_simulated_by (s, ms) (t, mt) (r, mr ) =     
    forM ( visible s ) $ \ (p,a,p') -> 
        forM ( S.toList $ R.images r p ) $ \ q -> do
            let imgs = S.fromList $ images t q a
                sims = R.images r p'
                both = S.intersection imgs sims 
            inform $ text "Transition   (p1,a,q1) in" <+> ms <+> text ":" <+> toDoc (p,a,p') </> vcat 
                          [ text "Simulation  (p1,p2) in" <+> mr <+> text ":" <+> toDoc (p,q)
                          , nest 4 $ vcat [ braces ( text "p2 | (p2,a,q2) in" <+> mt ) <+> equals <+> toDoc imgs
                          , braces ( text "p2 | (p1,p2) in" <+> mr ) <+> equals <+> toDoc sims
                          , nest 4 $ text "gemeinsam erreichbar:" <+> toDoc both
                          ] ]
            when  ( S.null both ) $ reject $ text "nicht zusammenführbar."


