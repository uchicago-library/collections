open Prelude
open Restful

module Service = struct
  include Service

  let endpoints =
    let open Lib.Endpoint in
    map endpoint_to_spec endpoints

  let main _ _ cgi =
    Pathinfo.dispatch "collections" endpoints cgi
end

module R = Make (Service) (Error (Log.Default))
let () = R.main ()
