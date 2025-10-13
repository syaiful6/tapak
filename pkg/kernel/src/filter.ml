type ('req, 'res, 'req2, 'res2) t =
  ('req, 'res) Service.t -> ('req2, 'res2) Service.t

type ('req, 'res) simple = ('req, 'res, 'req, 'res) t

let ( >>> ) f1 f2 s = s |> f1 |> f2

let apply_all filters service =
  List.fold_right (fun f acc -> f acc) filters service
