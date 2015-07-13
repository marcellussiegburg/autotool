-- how to add a new geometric data type (e.g., Angle) --

Syntax:

* add the type name to the abstract syntax:  Geo.Program.AST
  ( data Type = Boolean | Number | Point | Line | Circle | Angle)

* note: concrete Syntax is derived automatically
  ( derives [makeReader] [''Type]  in Geo.Program.Reader,
  derives [makeToDoc] [''Type]  in Geo.Program.ToDoc )

Semantics:

* add a value representation to the semantic domain (Geo.Program.Value)
 data Value k s  -- ^  k is the coefficient domain
    = ... | Angle (k, k)

* note: what is s in the above? it is the interpreter's state type.
why is it visible in values? because "functions" are also values,
and "functions" are actually state-changing subprograms

* add a type representation (Geo.Program.Value)

data Type = ... | AngleT

* add conversions in Geo.Program.Eval (typeOf, liftT)

* describe how to create a free object of the type (Geo.Program.Value):

decl env (G.Decl tn Nothing Nothing) = ...
      AngleT -> curry Angle <$> number <*> number

* implement basic (built-in) operations that use the type (Geo.Program.Ops)

angle_sum [Angle(v1,v2), Angle(w1,w2)] = do
  return $ Angle ( v1*w2 + v2*w1, v2*w2 - v1*w1 )

* make them known in the standard environment (Geo.Program.Ops)

std = ...
  , ( mk 0 "angle_sum", Function AngleT [ AngleT, AngleT ] angle_sum )


