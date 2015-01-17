Angle p3_angle(Point A, Point B, Point C) {
  return l2_angle(pp_line(A,B),pp_line(B,C));
} ;

Boolean on_bisector(Point P, Point A, Point B, Point C) {
  return eq_angle(p3_angle(A,B,P),p3_angle(P,B,C));
} ;

Point A = Point(0,0);
Point B = Point(1,0);
Point C ; 

Point P ;

Assume  on_bisector(P,B,A,C);
Assume  on_bisector(P,C,B,A);

Claim on_bisector(P,A,C,B);