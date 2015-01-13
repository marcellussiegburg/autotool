Point M = Point(0,0);
Point A = Point(0,1);
Circle c = pc_circle(M,A);

Point B;
Point C;
Point D;

Point R = varpoint(B,C,fresh());
Point S = varpoint(A,C,fresh());
Point T = varpoint(A,B,fresh());

Assume on_circle(B,c);
Assume on_circle(C,c);
Assume on_circle(D,c);

Assume is_orthogonal(pp_line(A,B),pp_line(D,T));
Assume is_orthogonal(pp_line(A,C),pp_line(D,S));
Assume is_orthogonal(pp_line(B,C),pp_line(D,R));

Claim is_collinear(R,S,T);
