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
