Point O = Point(0,0); Point A = Point(1,0);

Circle c = pc_circle(O,A);

Point B; Point C; Point D; Point E; Point F;

Point P;
Line l = ortho_line(P,pp_line(O,P));

Assume on_circle(B,c);
Assume on_circle(C,c);
Assume on_circle(D,c);

Assume is_collinear(B,D,P);
Assume is_collinear(A,C,P);

Assume on_line(E,l);
Assume is_collinear(A,D,E);

Assume on_line(F,l);
Assume is_collinear(B,C,F);

Claim eq_dist (P,E,P,F);


