module type ENDPOINT = Endpoint_intf.ENDPOINT

val endpoint_to_spec :
  (module ENDPOINT) ->
  string * (Netcgi.cgi -> 'a -> 'b -> Nethttp.http_status)

val print_schemas : (module ENDPOINT) -> unit

val endpoints : (module ENDPOINT) list
