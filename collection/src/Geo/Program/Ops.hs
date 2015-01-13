-- | rational geometric constructions,
--  cf. https://github.com/hg-graebe/GeoProver/blob/master/src/Inline/maxima.inline
-- we use here the same order, and sectioning, of definitions.

{-# language RebindableSyntax #-}

module Geo.Program.Ops where

import Prelude hiding (Num (..), (^), (/) )
import Polynomial.Class
import Geo.Domain
import Geo.Program.Value
import Autolib.TES.Identifier (mk, Identifier)

import qualified Data.Map.Strict as M
import Control.Applicative

-- | semantics:
-- Point A :== [a1,a2] <=> A=(a1,a2)
-- Line a :== [a1,a2,a3] <=> a1*x+a2*y+a3 = 0
-- Circle c :== [c0,c1,c2,c3] <=> c0*(x^2+y^2)+c1*x+c2*y+c3 = 0
-- Angle w  :== Angle(w1,w2)        <=> tan(w)=w1/w2
   
point [Number a1, Number a2] = do
  return $ Point (a1,a2)

line [Number a1, Number a2, Number a3] = do
  return $ Line (a1,a2,a3)

-- *  /* ==== elementary geometric constructions ==== */

-- | the fourth vertex X of the parallelogram ABCX
--  with vertices in that order (I guess)
par_point [Point (a1,a2),Point(b1,b2),Point(c1,c2)] = do
  return $ Point (a1-b1+c1,a2-b2+c2)

-- | the line through two points  
pp_line [ Point (a1,a2), Point (b1,b2) ] = do
  return $ Line ( b2-a2, a1-b1, a2*b1 - a1*b2 )

intersection_point [ Line(a1,a2,a3), Line(b1,b2,b3) ] = do
  let d = a1*b2 - b1*a2
      d1 = a3 *b2 - b3*a2
      d2 = a1*b3 - b1*a3
  add_ndg d -- actually, the numerator
  return $ Point( - d1 / d, - d2 / d)

-- | the line through p, perpendicular to a
ortho_line [ Point(p1,p2), Line(a1,a2,a3)] = do
  return $ Line ( a2, - a1, a1*p2 - a2*p1) 

-- | the line through p, parallel to a
par_line [ Point (p1,p2), Line (a1,a2,a3)] = do
  return $ Line ( a1,a2, - a2*p1 + a2*p2)

-- | a point on the line through a and b.
-- choice of parameters: 1 => a, 0 => b.
-- note: strange order/naming of parameters.  
varpoint [Point (b1,b2), Point (a1,a2), Number l ] = do
  return $ Point (l*a1+( 1 - l)*b1, l*a2+( 1 - l)*b2)

-- line_slider: this has a case distinction
-- that cannot be evaluated symbolically?  

-- *   /* ======= geometric type Distance ====== */

-- | The square of the distance between the points A and B.
sqrdist [ Point (p1,p2), Point (q1,q2) ] = do
  return $ Number $ (p1-q1)^2 + (p2-q2)^2

-- *    /* ======= elementary geometric properties ====== */

-- | A,B,C are on a common line.
is_collinear [Point(p1,p2),Point(q1,q2),Point(r1,r2)] = do
  return $ Boolean $ determinant3 [[p1,p2,1],[q1,q2,1],[r1,r2,1]]

determinant2 [[a,b],[c,d]] = a*d-b*c
determinant3 [x:xs,y:ys,z:zs] = x*determinant2[ys,zs] - y*determinant2[xs,zs] + z*determinant2[xs,ys]

-- | Lines a,b,c have a common point.
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

-- *  /* ======= angles ====== */

l2_angle [Line(g1,g2,g3), Line (h1,h2,h3)] = do
  return $ Angle ( g1*h2 - g2*h1 , g1*h1+g2*h2 )

eq_angle [Angle(a1,a2), Angle (b1,b2)] = do
  return $ Boolean $ a1*b2 - b2*a1       

angle_sum [Angle(v1,v2), Angle(w1,w2)] = do
  return $ Angle ( v1*w2 + v2*w1, v2*w2 - v1*w1 )
  
-- *    /* ======= circles ====== */

-- | the circle with center m and point a on circumference
pc_circle [Point(m1,m2),Point(a1,a2)] = do
  return $ Circle (1, -2 * m1, -2 * m2
                  ,a1*(2*m1 - a1) + a2*(2*m2-a2) )

circle_center [Circle (c1,c2,c3,c4)] = do
  add_ndg c1
  return $ Point (- c2 / (2 * c1), - c3 / (2 * c1) )

circle_sqradius [Circle (c1,c2,c3,c4)] = do
  add_ndg c1
  return $ Number $ ( c2^2 +c3^2 - 4 * c1*c4) / (2 * c1) ^2

on_circle [Point(p1,p2),Circle(c1,c2,c3,c4)] = do
  return $ Boolean $ c1*(p1^2+p2^2)+c2*p1 + c3*p2 + c4

-- | Sliding point on the circle with center M
-- and circumference point A using parameter u
circle_slider [Point(m1,m2),Point(a1,a2),Number u] = do
  let d = u^2 + 1
  add_ndg d
  return $ Point ( (a1*(u^2-1)+ 2*m1+2*(m2-a2)*u) / d
                 , (a2 + 2*(m1-a1)*u + (2*m2-a2)*u^2) / d
                 )

-- | Radical axis of the circles c and d,
-- i.e. the line through the intersection points
-- of the two circles if they intersect.
radical_axis [Circle(c1,c2,c3,c4),Circle(d1,d2,d3,d4)] = do
  -- apply('Line,makelist(part(c1,1)*part(c2,i)-part(c1,i)*part(c2,1),i,2,4));
  return $ Line (c1*d2-c2*d1, c1*d3-c3*d1, c1*d4-c4*d1)

-- *      /* generic code */

std :: Domain s d => Env Identifier d s
std = M.fromList
  [ ( mk 0 "fresh", Function NumberT [] $ \ _ -> Number <$> number )
  , ( mk 0 "Point", Function PointT [ NumberT, NumberT ] point )
  , ( mk 0 "Line", Function LineT [ NumberT, NumberT, NumberT ] line )
  , ( mk 0 "par_point", Function PointT [ PointT, PointT, PointT ] par_point )  
  , ( mk 0 "pp_line" , Function LineT [ PointT, PointT ] pp_line )
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
  , ( mk 0 "l2_angle", Function AngleT [ LineT, LineT ] l2_angle )
  , ( mk 0 "eq_angle", Function BooleanT [ AngleT, AngleT ] eq_angle )
  , ( mk 0 "angle_sum", Function AngleT [ AngleT, AngleT ] angle_sum )
  , ( mk 0 "pc_circle", Function CircleT [ PointT, PointT ] pc_circle )
  , ( mk 0 "circle_center", Function PointT [ CircleT ] circle_center ) 
  , ( mk 0 "circle_sqradius", Function NumberT [ CircleT ] circle_sqradius )
  , ( mk 0 "on_circle", Function BooleanT [ PointT, CircleT ] on_circle )
  , ( mk 0 "circle_slider", Function PointT [ PointT, PointT, NumberT ] circle_slider )
  , ( mk 0 "radical_axis", Function LineT [ CircleT, CircleT ] radical_axis ) 
  ]  
