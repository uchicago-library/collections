(* httpr-intf
 * httpr-intf.ml
 * Keith Waclena <https://www.lib.uchicago.edu/keith/>
 *
 * Copyright 2020 Keith Waclena. All rights reserved.
 * Distributed under the GPL2 license, see terms at the end of the file.
 *)

module Response = struct
  type err = string

  (* TODO FEATURE cookies *)
  (* TODO FEATURE authentication *)
  type t = {
      uri : Uri.t;                (** the ORIGINAL uri of the initiated call *)
      status : int;
      reason : string;
      headers : string list;
      ctype : string;
      body : string;
    }
  let uri t     = t.uri
  let status t  = t.status
  let reason t  = t.reason
  let headers t = t.headers
  let ctype t   = t.ctype
  let body t    = t.body
end

let to_string r =
  let open Response in
  Printf.sprintf "%d %s\n\
                  \  %s\n\
                  ContentType: %s\n\
                  %sSIZE = %d\n"
    r.status
    (Uri.to_string r.uri)
    r.reason
    r.ctype
    (String.concat "\n" r.headers)
    (String.length r.body)

let reasons = [                 (* TODO expand to complete official list *)
    1, "Informational";
    2, "Successful";
    3, "Redirection";
    4, "Client Error";
    5, "Server Error";
  ]

(* RFC 2616 §6.1 only 1 space as sep *)
let reason str =
  let default d f x = try f x with _ -> d in
  match String.split_on_char ' ' str with
  | _proto:: code::[]     -> default "UNKNOWN" (List.assoc (default 900 int_of_string code / 100)) reasons
  | _proto::_code::reason -> String.concat " " reason
  | _                     -> "UNKNOWN"

let _only2xx =                  (* TODO DELETE? *)
  function
  | Error _ as err -> err
  | Ok resp as ok  -> if 2 = resp.Response.status / 100
                      then ok
                      else let reason = resp.Response.reason in
                           Error (if reason = "" then string_of_int resp.Response.status else reason)

module type S = sig
  module Response : (module type of Response)
  val ssl_init : unit -> unit
  val get  : ?timeout:int -> ?verbose:bool -> ?redirects:int -> ?headers:(string list) -> Uri.t
             -> (Response.t, Response.err) result
  val gets : ?timeout:int -> ?verbose:bool -> ?redirects:int -> ?headers:(string list) -> Uri.t list
             -> ((Response.t, Response.err) result list, Response.err) result
  val gets_keyed : ?timeout:int -> ?verbose:bool -> ?redirects:int -> ?headers:string list
                   -> ('a * Uri.t) list
                   -> (('a * Response.t, string) result list, string) result
end

module Test (I : S) = struct
  let error uri msg = Uri.to_string uri |> Printf.sprintf "%s: %s" msg |> Result.error
  let run i (t,u) = i, Uri.of_string u |> t
  let withstatus status r u =
    let got = I.Response.status r in
    if got = status
    then Ok true
    else error u (Printf.sprintf "expected status %d, got %d" status got)
  let statuscheck status u = match I.get u with
    | Ok r -> withstatus status r u
    | Error _ as err -> err
  let redirectcheck status redirects u = match I.get ~redirects u with
    | Ok r -> withstatus status r u
    | Error _ as err -> err
  let tests = [
      statuscheck 200,       "http://example.com";
      statuscheck 200,       "https://example.com";
      statuscheck 404,       "https://gnu.org/fdhjgdjhgdjhgdjhg";
      statuscheck 999,       "http://examplekjdhgdkjghdkjghdjkghdjkghdkjhgdkjghdjkghd.com";
      statuscheck 999,       "https://examplekjdhgdkjghdkjghdjkghdjkghdkjhgdkjghdjkghd.com";
      redirectcheck 200 ~-1, "http://gnu.org";
      redirectcheck 399 0,   "http://gnu.org";
      redirectcheck 200 1,   "http://gnu.org";
      redirectcheck 200 2,   "http://gnu.org";
    ]
  let runtests () =
    I.ssl_init ();
    List.mapi run tests
end


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
