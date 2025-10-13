type ('req, 'res, 'req2, 'res2) t =
  ('req, 'res) Service.t -> ('req2, 'res2) Service.t

type ('req, 'res) simple = ('req, 'res, 'req, 'res) t

val ( >>> ) :
   ('q1, 'p1, 'q2, 'p2) t
  -> ('q2, 'p2, 'q3, 'p3) t
  -> ('q1, 'p1, 'q3, 'p3) t

val apply_all :
   ('req, 'res) simple list
  -> ('req, 'res) Service.t
  -> ('req, 'res) Service.t
