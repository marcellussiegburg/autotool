Point midpoint(Point A, Point B) { return varpoint(A,B,1/2); } ;
Line p_bisector(Point B, Point C) { return ortho_line(midpoint(B,C),pp_line(B,C)); } ;
Line altitude(Point A, Point B, Point C) { return ortho_line(A,pp_line(B,C)); } ;
Line median(Point A, Point B, Point C) { return pp_line(A,midpoint(B,C)); } ;
Point pedalpoint(Point P, Line a) { return intersection_point(ortho_line(P,a),a); } ;

Point orthocenter (Point A, Point B, Point C) { return intersection_point (p_bisector (A,B), p_bisector (B,C)); } ;
Circle circumcircle (Point A, Point B, Point C) {
  return pc_circle (orthocenter (A, B, C), A);
};

Circle Feuerbach(Point A, Point B, Point C) {
  return circumcircle (midpoint(A,B),midpoint(B,C), midpoint (C,A));
};

Point A = Point(0,0);
Point B = Point (1,0);
Point C;
Circle F = Feuerbach (A,B,C);

Claim on_circle (pedalpoint (A,pp_line(B,C)),F);

Point H = intersection_point (altitude(A,B,C),altitude(B,C,A));
Claim on_circle (midpoint(H,C), F);

Boolean is_cl_tangent(Circle c, Line l) {
   return on_circle(pedalpoint(circle_center(c),l),c);
};

Boolean is_cc_tangent(Circle c1,Circle c2) {
   return is_cl_tangent(c1,radical_axis(c1,c2));
};

Circle I; -- Inkreis oder Ankreise
Assume is_cl_tangent (I, pp_line(A,B));
Assume is_cl_tangent (I, pp_line(B,C));
Assume is_cl_tangent (I, pp_line(C,A));

Claim is_cc_tangent (I,F);