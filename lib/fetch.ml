let fetch url =
  let open Lwt.Infix in
  let stringify (_, body) = Cohttp_lwt.Body.to_string body in
  Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= stringify

