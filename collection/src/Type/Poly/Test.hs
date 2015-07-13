module Type.Poly.Test where

import Type.Poly.Data
import Type.Poly.Check

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter

inst = TI { target = read "Animal" , signature = sig }
       
sig :: Signature
sig = read $ concat
   [ "  class S {                                                     "
   , "     static  Piggy<Kermit> statler ( );                         "
   , "     static <T1> Fozzie<Animal, T1> waldorf ( Piggy<T1> x       "
   , "                                            );                  "
   , "     static <T2> Animal bunsen ( Kermit x                       "
   , "                               , Fozzie<Animal, Piggy<T2>> y ); "
   , "     static <T2> Piggy<Piggy<T2>> chef ( Piggy<T2> x            "
   , "                                       );                       "
   , "     static <T2> Kermit rowlf ( Fozzie<T2, Piggy<Kermit>> x     "
   , "                              );                                "
   , " }                                                              "
   ]

sol :: Expression
sol = read "S.<Kermit>bunsen(S.statler(),S.statler())"
