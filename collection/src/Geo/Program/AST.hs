{-# language DeriveDataTypeable #-}

module Geo.Program.AST where

import Data.Typeable

import Autolib.TES.Identifier

data Op = Add | Subtract | Multiply | Divide
    deriving ( Typeable, Eq )

data Exp v
     = Ref v
     | Const Integer
     | Oper (Exp v) Op (Exp v)
     | Parens (Exp v)  
     | Apply (Exp v) [ Exp v ]
     | Block (Program v)
    deriving Typeable

type Program v = [ Statement v ] 

data Typed v = Typed Type v
    deriving Typeable
             
data Statement v
       = Decl (Typed v) (Maybe [ Typed v ]) (Maybe (Exp v ))
       | Emit Kind (Exp v)
       | Return (Exp v)  
    deriving Typeable

data Type = Void | Boolean | Number | Point | Line | Circle | Angle
    deriving Typeable

data Kind = Prohibit | Assume | Claim
   deriving Typeable

a = mknullary "a" ; b = mknullary "b" ; c = mknullary "c"

exp0 :: Exp Identifier
exp0 = Block
   [ Decl (Typed Line (mk 0 "mc")) Nothing
          (Just $ Apply (Ref $ mk 0 "bisector") [ Ref a, Ref b ])
   , Decl (Typed Line (mk 0 "ma")) Nothing
          (Just $ Apply (Ref $ mk 0 "bisector") [ Ref b, Ref c ])
   , Return  (Apply (Ref $ mk 0 "intersection")
         [ Ref (mk 0 "mc"), Ref (mk 0 "ma") ] )
   ]  
   
