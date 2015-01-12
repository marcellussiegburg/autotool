-- | rational geometric constructions,
--  cf. https://github.com/hg-graebe/GeoProver/blob/master/src/Inline/maxima.inline

module Geo.Program.Ops where

import Prelude hiding (Num (..), (^), (/) )
import Polynomial.Class
import Geo.Domain
import Geo.Program.Value
import Autolib.TES.Identifier (mk, Identifier)

import qualified Data.Map.Strict as M

-- | semantics:
-- Point A :== [a1,a2] <=> A=(a1,a2)
-- Line a :== [a1,a2,a3] <=> a1*x+a2*y+a3 = 0
-- Circle c :== [c0,c1,c2,c3] <=> c0*(x^2+y^2)+c1*x+c2*y+c3 = 0

pp_line [ Point (a1,a2), Point (b1,b2) ] = do
  return $ Line ( b2-a2, a1-b1, a2*b1 - a1*b2 )

intersection_point [ Line(a1,a2,a3), Line(b1,b2,b3) ] = do
  let d = a1*b2 - b1*a2
      d1 = a3 *b2 - b3*a2
      d2 = a1*b3 - b1*a3
  add_ndg d -- actually, the denominator
  return $ Point(negate d1 / d, negate d2 / d)

ortho_line [ Point(p1,p2), Line(a1,a2,a3)] = do
  return $ Line ( a1, negate a2, a2*p2 - a2*p1) 

par_line [ Point (p1,p2), Line (a1,a2,a3)] = do
  return $ Line ( a1,a2, negate a2*p1 + a2*p2)

varpoint [Point (b1,b2), Point (a1,a2), Number l ] = do
  return $ Point (l*a1+(fromInteger 1-l)*b1, l*a2+(fromInteger 1-l)*b2)

sqrdist [ Point (p1,p2), Point (q1,q2) ] = do
  return $ Number $ (p1-q1)^2 + (p2-q2)^2

is_collinear [Point(p1,p2),Point(q1,q2),Point(r1,r2)] = do
  let e = fromInteger 1
  return $ Boolean $ determinant3 [[p1,p2,e],[q1,q2,e],[r1,r2,e]]

determinant2 [[a,b],[c,d]] = a*d-b*c
determinant3 [x:xs,y:ys,z:zs] = x*determinant2[ys,zs] - y*determinant2[xs,zs] + z*determinant2[xs,ys]

is_concurrent [Line(a1,a2,a3),Line(b1,b2,b3),Line(c1,c2,c3)] = do
  return $ Boolean $ determinant3 [[a1,a2,a3],[b1,b2,b3],[c1,c2,c3]]

is_parallel [Line(a1,a2,a3),Line(b1,b2,b3)] = do
  return $ Boolean $ a1*b2-b1*a2

is_orthogonal [Line(a1,a2,a3),Line(b1,b2,b3)] = do
  return $ Boolean $ a1*b1 + a2*b2

on_line [Point(p1,p2), Line(a1,a2,a3)] = do
  return $ Boolean $ p1*a1 + p2*a2 + a3

eq_dist [a,b,c,d] = do
  Number ab <- sqrdist [a, b]
  Number cd <- sqrdist [c, d]
  return $ Boolean $ ab - cd

std :: Domain s d => Env Identifier d s
std = M.fromList
  [ ( mk 0 "pp_line" , Function LineT [ PointT, PointT ] pp_line )
  , ( mk 0 "intersection_point", Function PointT [ LineT, LineT ] intersection_point )
  , ( mk 0 "ortho_line", Function LineT [ PointT, LineT ] ortho_line )
  , ( mk 0 "par_line", Function LineT [ PointT, LineT ] par_line )
  , ( mk 0 "varpoint", Function PointT [ PointT, PointT, NumberT ] varpoint )
  , ( mk 0 "sqrdist", Function NumberT [ PointT, PointT ] sqrdist )
  , ( mk 0 "is_collinear", Function BooleanT [ PointT, PointT, PointT ] is_collinear )
  , ( mk 0 "is_concurrent", Function BooleanT [ LineT, LineT, LineT ] is_concurrent )
  , ( mk 0 "is_parallel", Function BooleanT [ LineT, LineT ] is_parallel )
  , ( mk 0 "is_orthogonal", Function BooleanT [ LineT, LineT ] is_orthogonal )
  , ( mk 0 "on_line", Function BooleanT [ PointT, LineT ] on_line )
  , ( mk 0 "eq_dist", Function BooleanT [ PointT, PointT, PointT, PointT ] eq_dist )    
  ]  
