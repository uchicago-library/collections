open Restful

(* open Lib.Fetch *)

module Service = struct
  include Service
  open Valid
  open Param
  module D = Lib.Defaults

  let spec = Lib.GetBrowseListLanguages.Spec.spec

  let getBrowseListLanguages = Lib.GetBrowseListLanguages.gimme

  let gimme
        ?(group=D.group)
        ?(collection=D.collection)
        _endpoint
    = getBrowseListLanguages ~group ~collection

  let main _ _ cgi =
    let ps = process cgi spec in
    let group = (value ps "group") in
    let collection = (value ps "collection") in
    let endpoint = "getBrowseListLanguages" in
    Content.write ~content_type:"text/plain" cgi
      (Printf.sprintf "%s" (gimme ~group ~collection endpoint ()))

end

module R = Make (Service) (Error (Log.Default))

let () = R.main ()
