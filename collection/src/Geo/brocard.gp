Point pedalpoint(Point P, Line a) {
      return intersection_point(ortho_line(P,a),a);
} ;

Boolean is_cl_tangent(Circle c, Line l) {
   return on_circle(pedalpoint(circle_center(c),l),c);
};

Point A = Point(0,0);
Point B = Point(1,0);
Point C;

Point M1; Point M2; Point M3; 

Circle c1 = pc_circle(M1,A);
Circle c2 = pc_circle(M2,B);
Circle c3 = pc_circle(M3,C);

Assume is_cl_tangent(c1,pp_line(A,C));
Assume on_circle(B,c1);
Assume is_cl_tangent(c2,pp_line(A,B));
Assume on_circle(C,c2);
Assume is_cl_tangent(c3,pp_line(B,C));
Assume on_circle(A,c3);

Point P;

Assume on_circle(P,c1);
Assume on_circle(P,c2);

Claim  on_circle(P,c3);


