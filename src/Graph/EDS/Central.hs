-- | exact dominating set (used for construction of codes, see Koblitz)

{-# language FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Graph.EDS.Central 

( make_fixed
, make_quiz
, EDS ( Exact_Dominating_Set )
)

where

-- import Graph.EDS.SAT ( pvc ) -- TODO
import qualified Graph.EDS.Param as P
-- import SAT.Types

import Autolib.Graph.Kneser ( petersen )

import Graph.Util
import Autolib.Graph.Ops ( normalize )
import Autolib.Graph.Basic ( clique )
import Autolib.Dot ( peng , Layout_Program ( Dot ) )
import qualified Autolib.Reporter.Set ( subeq )
import Autolib.Util.Zufall ( eins , genau, randomRIO, subsequence, selektion )
import Autolib.FiniteMap ( isEmptyFM )

import qualified Data.Set as S

import Inter.Quiz ( Generator , generator , Project , project , quiz )
import Inter.Types ( Make , direct , ScoringOrder (..) , OrderScore (..) )
import Data.Typeable ( Typeable )

import qualified Challenger as C

import Data.List ( (\\) )

-------------------------------------------------------------------------------

data EDS = Exact_Dominating_Set deriving ( Eq , Ord , Show , Read , Typeable )

instance OrderScore EDS where
    scoringOrder _ = Increasing

instance C.Partial EDS (Graph Int) (Set Int) where

    report _ g = do

        inform $ vcat
	       [ text "Geben Sie eine exakte dominierende Menge für diesen Graphen an:"
	       , nest 4 $ toDoc g
	       ]
        peng g { layout_program = if isEmptyFM (graph_layout g) 
		                  then Dot
		                  else layout_program g
	       }

    initial _ g = knoten g

    partial _ g ns = do

        Autolib.Reporter.Set.subeq ( text "Knoten in ihrer Einsendung" , ns )
	                           ( text "Knoten des Graphen" , knoten g )

    total _ g ns = do

        let subsets = do
                u <- S.toList ns
                return (u, S.insert u $ nachbarn g u)
        inform $ text "Die Nachbarschaften dieser Knoten sind"
               </> toDoc subsets
        let covered = do
                u <- lknoten g
                let cs = filter ( \ (c,s) -> S.member u s ) subsets
                return ( u, cs )

        inform $ text "Ist Ihre Einsendung eine dominierende Menge?"
        inform $ text "(Bilden die Nachbarschaftsmengen eine Überdeckung?)"
        let missing = do
                ( u, cs ) <- covered
                guard $ 1 > length cs
                return u
        when ( not $ null missing ) $ reject 
             $ text "Nein, diese Knoten sind in keiner Nachbarschaftsmenge enthalten"
             </> toDoc missing
        inform $ text "Ja."

        inform $ text "Bilden die Nachbarschaftsmengen eine exakte Zerlegung?"
        let toomuch = do
                ( u, cs ) <- covered
                guard $ 1 < length cs
                return ( u, cs )
        when ( not $ null toomuch ) $ reject 
             $ text "Nein, diese Knoten sind in mehreren Nachbarschaftsmengen enthalten:"
             </> toDoc toomuch

        inform $ text "Ja."

instance C.Measure EDS (Graph Int) (Set Int) where
    measure _ _ ns = fromIntegral $ cardinality ns

make_fixed :: Make
make_fixed = direct Exact_Dominating_Set petersen

instance Generator EDS P.Param ( Graph Int , Set Int ) where
    generator _ conf _ = do

        let ns = [ 1 .. P.knoten conf ]

        dss <- randomRIO $ P.solution_size_range conf

        (here, there) <- subsequence dss ns

        let ds = S.fromList here
            extra = S.fromList there

        links <- forM there $ \ v -> do
            u <- eins here
            return $ kante u v

        extra_links <- selektion ( P.knoten conf - length links )
                       $ lkanten $ clique extra
                    
        return ( mkGraph ( S.fromList ns ) 
                         ( S.union (S.fromList links) (S.fromList  extra_links))
               , ds )

instance Project EDS (Graph Int,Set Int) (Graph Int) where
    project _ (g,c) = g

make_quiz :: Make
make_quiz = quiz Exact_Dominating_Set P.p0
