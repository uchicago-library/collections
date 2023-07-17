open Restful

module Service = struct
  include Service

  let endpoints = [
      Lib.GetBrowseListLanguages.(endpoint_name, subservice);
    ]

  let main _ _ cgi =
    Pathinfo.dispatch "endpoint" endpoints cgi
end

module R = Make (Service) (Error (Log.Default))

let () = R.main ()
