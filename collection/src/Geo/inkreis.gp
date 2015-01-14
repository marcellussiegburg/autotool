Point midpoint(Point A, Point B) { return varpoint(A,B,1/2); } ;
Line p_bisector(Point B, Point C) { return ortho_line(midpoint(B,C),pp_line(B,C)); } ;
Line altitude(Point A, Point B, Point C) { return ortho_line(A,pp_line(B,C)); } ;
Line median(Point A, Point B, Point C) { return pp_line(A,midpoint(B,C)); } ;
Point pedalpoint(Point P, Line a) { return intersection_point(ortho_line(P,a),a); } ;

Boolean is_cl_tangent(Circle c, Line l) {
   return on_circle(pedalpoint(circle_center(c),l),c);
};

Boolean is_cc_tangent(Circle c1,Circle c2) {
   return is_cl_tangent(c1,radical_axis(c1,c2));
};

Angle p3_angle(Point A, Point B, Point C) {
  return l2_angle(pp_line(A,B),pp_line(B,C));
} ;

Boolean on_bisector(Point P, Point A, Point B, Point C) {
  return eq_angle(p3_angle(A,B,P),p3_angle(P,B,C));
} ;

Point A = Point (0,0);
Point B = Point (1,0);
Point C;

Point I;

Assume on_bisector (I, A,B,C);
Assume on_bisector (I, B,C,A);

Claim on_bisector (I, C,A,B);

Circle In = pc_circle (I,pedalpoint (I, pp_line(A,B)));

Claim eq_dist (I,pedalpoint (I, pp_line(A,B)), I, pedalpoint (I, pp_line(B,C)));

-- Claim is_cl_tangent (In,pp_line(A,B));
-- Claim is_cl_tangent (In,pp_line(B,C));
-- Claim is_cl_tangent (In,pp_line(C,A));
