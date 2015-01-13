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

