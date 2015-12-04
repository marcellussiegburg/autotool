{-

Realisiere den Semantik-Server für diesen Aufgabentyp:

* Aufgabe: eine positive ganze Zahl n
* Lösung: ein Pratt-Zertifikat für die Primalität von n

Pratt, V. "Every Prime Has a Succinct Certificate." SIAM J. Comput. 4, 214-220, 1975.
http://boole.stanford.edu/pub/SucCert.pdf

Seite 217, 3. Picturesque proof trees

Knotenliste [(p,x,[(q_1,e_1),..]), ... ]

wobei jeder Eintrag: (2,1,[]) oder

      p-1 = q_1^e_1 * ..
  und forall i:  x^(p-1) mod p == 1
  und forall i: x^((p-1)/q_i) mod p /= 1
  und forall i: q_i kommt in Liste vor.

Modell zur Implementierung:

* dieses File:  vgl. Importe und Instanzen in
     collection/src/Faktor/Faktor.hs
* Publikation: in collection/src/Inter/Collector.hs

-}

{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}

module Prime.Certificate where

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Autolib.Size

import Inter.Types
import Inter.Quiz

import Data.Typeable

data Prime_Certificate = Prime_Certificate
  deriving ( Show, Read, Typeable )

instance OrderScore Prime_Certificate where
  scoringOrder _ = Increasing

data Certificate = Certificate -- [(..)]
  deriving Typeable

$(derives [makeReader,makeToDoc] [''Certificate])

instance Size Certificate where
  -- TODO

instance Partial Prime_Certificate Integer Certificate where
  -- TODO
  
make_fixed :: Make
make_fixed = direct Prime_Certificate (474397531::Integer)

