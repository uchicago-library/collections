open Restful
module R = Make (Service) (Error (Log.Default))
let () = R.main ()
