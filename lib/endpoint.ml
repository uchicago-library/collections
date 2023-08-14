module type ENDPOINT = sig
  val endpoint_name : string
  val subservice : Netcgi.cgi ->
                   'a ->
                   'b ->
                   Nethttp.http_status
  module Schema : sig
    val input_write : unit -> unit
    val output_write : unit -> unit
  end
end

let endpoint_to_spec (module E : ENDPOINT) =
  E.(endpoint_name, subservice)

let print_schemas (module E : ENDPOINT) =
  Prelude.print ("Printing input schema for " ^ E.endpoint_name) ;
  E.Schema.input_write () ;
  Prelude.print ("Printing output schema for " ^ E.endpoint_name) ;
  E.Schema.output_write ()

let endpoints = [ (module GetBrowseListLanguages : ENDPOINT) ]
