include Tapak_kernel.Request

let header key request = Headers.get (headers request) key

let add_header key value request =
  let headers = Headers.add (headers request) key value in
  with_ ~headers request

let remove_header key request =
  let headers = Headers.remove (headers request) key in
  with_ ~headers request

let replace_header key value request =
  let headers = Headers.replace (headers request) key value in
  with_ ~headers request
