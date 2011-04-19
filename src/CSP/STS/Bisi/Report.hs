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


check_bisi (s,t) r = do
    let miss = S.difference ( states s ) ( domain r )
    when ( not $ S.null miss ) $ reject 
         $ text "Diese Zustände aus S sind nicht im Vorbereich von R enthalten:"
         </> toDoc miss
    let mist = S.difference ( states t ) ( codomain r )
    when ( not $ S.null mist ) $ reject 
         $ text "Diese Zustände aus T sind nicht im Nachbereich von R enthalten:"
         </> toDoc mist
    when ( not $ R.holds r (start s) (start t) ) $ reject
         $ text "Es gilt nicht  R(start s, start t)"
    is_simulated_by (s, text "S") (t, text "T") r
    is_simulated_by (t, text "T") (s, text "S") $ R.inverse r
    
is_simulated_by (s, ms) (t, mt) r =     
    forM ( visible s ) $ \ (p,a,p') -> 
        forM ( S.toList $ R.images r p ) $ \ q -> do
            let imgs = S.fromList $ images t q a
                sims = R.images r p'
                both = S.intersection imgs sims 
            inform $ text "Transition in" <+> ms <+> text ":" <+> toDoc (p,a,p') </> vcat 
                          [ text "Simulation in R:" <+> toDoc (p,q)
                          , text "durch Transition in" <+> mt <+> text "erreichbar:" </> toDoc imgs
                          , text "durch Simulation erreichbar:" </> toDoc sims
                          , text "gemeinsam erreichbar:" </> toDoc both
                          ]
            when  ( S.null both ) $ reject $ text "nicht zusammenführbar."


