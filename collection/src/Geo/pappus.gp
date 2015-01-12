{ Point a = Point (1,0);
  Point b ; --  = Point (2,0)
  Number u ; Point c = varpoint (a, b, u) ;
  Point x = Point (0,1);
  Point y ;
  Number v ; Point z = varpoint (x, y, v) ;
  Point p = intersection_point (pp_line(a,y),pp_line(b,x));
  Point q = intersection_point (pp_line(a,z),pp_line(c,x));
  Point r = intersection_point (pp_line(b,z),pp_line(c,y));
  return is_collinear (p,q,r);
}