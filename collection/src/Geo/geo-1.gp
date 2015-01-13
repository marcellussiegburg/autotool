{
  Line altitude (Point a, Point b, Point c) { return ortho_line (a, pp_line (b,c)); } ;

  Point A; Point B; Point C;

  -- Satz vom Höhenschnittpunkt
  -- return  is_concurrent(altitude(A,B,C),altitude(B,C,A),altitude(C,A,B));

  Line median (Point a, Point b, Point c) { return pp_line (a, varpoint (b,c, 1/2)); } ;

  -- Satz vom Schnittpunkt der Seitenhalbierenden
  return is_concurrent(median(A,B,C),median(B,C,A),median(C,A,B));

}