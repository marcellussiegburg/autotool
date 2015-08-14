Point A = Point(0,0); Point B = Point(1,0);
Point C = Point(1,1); Point D = Point(0,1);

Point P; Point Q = varpoint(C,D,fresh());

Assume on_line(P,par_line(C,pp_line(B,D)));
Assume eq_dist (B,D, B,P);
Assume on_line(Q,pp_line(B,P));

Claim eq_dist (D,Q,  D,P);
