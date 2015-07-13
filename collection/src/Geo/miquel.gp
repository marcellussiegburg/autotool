Point midpoint(Point A, Point B) {
      return varpoint(A,B,1/2);
};

Line p_bisector(Point B, Point C) {
  return ortho_line(midpoint(B,C),pp_line(B,C));
};

Circle p3_circle(Point A, Point B, Point C) {
  return  pc_circle
    (intersection_point(p_bisector(B,C),p_bisector(A,C)),A);
};

Point A = Point (0,0);
Point B = Point (1,0);
Point C; 

Point P = varpoint(B,C,fresh());
Point Q = varpoint(A,C,fresh());
Point R = varpoint(A,B,fresh ());

Circle c1 = p3_circle(R,Q,A);
Circle c2 = p3_circle(R,P,B);
Circle c3 = p3_circle(P,Q,C);

Point S;

Assume on_circle(S,c1);
Assume on_circle(S,c2);

Claim on_circle(S,c3);
