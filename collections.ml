open Restful

(* open Lib.Fetch *)

module Service = struct
  include Service
  (* open Valid *)
  open Param
  module D = Lib.Defaults

  let getBrowseListLanguages cgi _ _ =
    let open Lib.GetBrowseListLanguages in
    let spec = Spec.spec in
    let ps = process cgi spec in
    let group = (value ps "group") in
    let collection = (value ps "collection") in
    Content.write ~content_type:"text/plain" cgi
      (Printf.sprintf "%s" (gimme ~group ~collection ()))

  let main _ _ cgi =
    Pathinfo.dispatch "multi" ["getBrowseListLanguages", getBrowseListLanguages] cgi


end

module R = Make (Service) (Error (Log.Default))

let () = R.main ()
