(* httpr-ocamlnet
 * httpr-ocamlnet.ml
 * Keith Waclena <https://www.lib.uchicago.edu/keith/>
 *
 * Copyright 2020 Keith Waclena. All rights reserved.
 * Distributed under the GPL2 license, see terms at the end of the file.
 *)

open Prelude

include Httpr_intf

module This = struct
  module Response = Httpr_intf.Response

  (** [ssl_init ()]: perform one-time global initialization for SAL. *)
  let ssl_init () = Nettls_gnutls.init ()

  let gets_keyed ?(timeout=0) ?(verbose=false) ?(redirects=(-1)) ?(headers=[]) alist =
    let open Nethttp_client in
    let ht = Hashtbl.create (len alist) in
    let results = ref [] in
    let getstatus call = match call # status with
      | `Unserved                -> 999, call # response_status_text
      | `Redirection             -> 599, "Too many redirects"
      | `Http_protocol_error exn -> 999, Printexc.to_string exn
      | `Successful              -> call # response_status_code, "Successful"
      | `Client_error | `Server_error ->call # response_status_code, call # response_status_text
    in
    let callback call =
      match Hashtbl.find ht call with
      | exception Not_found -> assert false
      | the_uri, data ->
         let status, reason = getstatus call in
         let headers = call # response_header # fields in
         let ctype = map (Pair.onleft String.lowercase_ascii) headers |> default "UNKNOWN" (assoc "content-type") in
         results @:= Ok (data,
         { Response.uri = the_uri; status; reason; ctype;
                    headers = map (fun (k,v) -> sprintf "%s: %s" k v) headers;
                    body = call # response_body # value;
         })
    in
    let add pipeline (data,uri) =
      let uristring = Uri.to_string uri in (* TODO ERROR catch it *)
      let call = new Nethttp_client.get uristring in
      let alist =
        let each str = match String.cut ~sep:":" str with
          | key, Some value -> key, value
          | key, None       -> key, ""
        in
        map each headers in
      let defhead = Assoc.merge alist [ "Content-length", "0"; "Content-type", "text/plain";] in
      let reqhead = new Netmime.basic_mime_header defhead in
      call # set_request_header reqhead;
      Hashtbl.add ht call (uri, data);
      pipeline # add_with_callback call callback
    in
    Debug.enable := verbose;
    let pipeline = new pipeline in
    let opts = pipeline # get_options in
    pipeline # set_options {
        opts with maximum_connection_failures = 0;
                  verbose_connection = false; (* too much... *)
                  verbose_status = true;      (* this is actually the default *)
                  maximum_redirections = (match redirects with
                                          |  0 -> assert false (* TODO NYI refuse *)
                                          | -1 -> max_int
                                          |  n -> n
                                         );
                  connection_timeout = if timeout=0 then 300.0 else float (timeout / 1000) };
    iter (add pipeline) alist;
    let rec loop () = match pipeline # run () with
      | exception exn -> Exn.to_string exn |> eprintf "pipeline aborted: %s\n"; loop ()
      | () -> Ok !results
    in
    loop ()

  let gets ?timeout ?verbose ?redirects ?headers uris =
    let open Result.Ops in
    map Pair.dup uris |> gets_keyed ?timeout ?verbose ?redirects ?headers
    >>| map (Result.map snd)

  let get ?timeout ?verbose ?redirects ?headers uri =
    match gets ?timeout ?verbose ?redirects ?headers [uri] with
    | Error _ as r -> r
    | Ok []        -> Error "no response"
    | Ok [r]       -> r
    | Ok _         -> assert false
end

module _ : S = This

include This


(*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *)
