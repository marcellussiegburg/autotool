{

  Line pascal_line
  (Point A, Point B, Point C,
   Point D, Point E, Point F) {
    return pp_line(
      intersection_point(pp_line(A,E),pp_line(B,D)),
      intersection_point(pp_line(B,F),pp_line(C,E)) );
  } ; 

  Point a ;  Point b ;  Point c = varpoint (a, b, fresh() ) ;
  Point x ;  Point y ;  Point z = varpoint (x, y, fresh() ) ;
  
  -- Drei Pappusgeraden gehen durch gemeinsamen Punkt
  return is_concurrent(
  	 pascal_line (a,b,c,x,y,z),
  	 pascal_line (a,b,c,y,z,x),
  	 pascal_line (a,b,c,z,x,y));	 
}