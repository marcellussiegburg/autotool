{
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
  
  Point A; Point B; Point C;
  Circle F = Feuerbach (A,B,C);
  
  -- return on_circle (pedalpoint (A,pp_line(B,C)),F);

  Point H = intersection_point (altitude(A,B,C),altitude(B,C,A));
  return on_circle (midpoint(H,C), F);
  
}