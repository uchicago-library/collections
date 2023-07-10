open Restful

open Lib.Fetch

module Service = struct
  include Service
  open Valid
  open Param

  let spec = [
      "id",   Mandatory, conjunction [numeric; atleast 1; atmost 10];
      "name", Mandatory, notblank;
    ]

  let main _ _ cgi =
    let ps = process cgi spec in
    Content.write ~content_type:"text/plain" cgi
      (Printf.sprintf "Hello, %s #%d!\n" (value ps "name") (Convert.int (value ps "id")));

end

module R = Make (Service) (Error (Log.Default))

let () = R.main ()
