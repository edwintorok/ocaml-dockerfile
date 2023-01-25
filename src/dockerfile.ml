(*
 * Copyright (c) 2014-2016 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Sexplib.Conv

type shell_or_exec =
  [ `Shell of string | `Shells of string list | `Exec of string list ]
[@@deriving sexp]

type sources_to_dest =
  [ `From of string option ]
  * [ `Src of string list ]
  * [ `Dst of string ]
  * [ `Chown of string option ]
  * [ `Link of bool option ]
[@@deriving sexp]

type from = {
  image : string;
  tag : string option;
  alias : string option;
  platform : string option;
}
[@@deriving sexp]

type parser_directive = [ `Syntax of string | `Escape of char ]
[@@deriving sexp]

type heredoc = {
  here_document : string;
  word : string;
  delimiter : string;
  strip : bool;
}
[@@deriving sexp]

type heredocs_to_dest = [ `Chown of string option ] * heredoc list * string
[@@deriving sexp]

type healthcheck_options = {
  interval : string option;
  timeout : string option;
  start_period : string option;
  retries : int option;
}
[@@deriving sexp]

type healthcheck = [ `Cmd of healthcheck_options * shell_or_exec | `None ]
[@@deriving sexp]

let escape_string ~char_to_escape ~escape v =
  let len = String.length v in
  let buf = Buffer.create len in
  let j = ref 0 in
  for i = 0 to len - 1 do
    if v.[i] = char_to_escape || v.[i] = escape then (
      if i - !j > 0 then Buffer.add_substring buf v !j (i - !j);
      Buffer.add_char buf escape;
      j := i)
  done;
  Buffer.add_substring buf v !j (len - !j);
  Buffer.contents buf

module Mount = struct
  type t =
      { mount_type : string
      ; target:string
      ; options : (string * string) list } [@@deriving sexp]

  let v ~mount_type ~target options = { mount_type; target; options }

  let equal (a:t) (b:t) = (a = b)

  let merge_consecutive acc next =
      match acc with
      | prev :: _ when equal prev next -> acc
      | _ -> next :: acc

  (** [merge a b] merges consecutive duplicates *)
  let merge (a:t list) (b:t list) =
      List.fold_left merge_consecutive [] (a @ b)
      |> List.rev

  let to_string ~escape t =
  	let do_escape = escape_string ~char_to_escape:' ' ~escape in
    ("target", t.target) :: t.options
    |> List.map (fun (k, v) -> (do_escape k) ^ "=" ^ (do_escape v))
    |> String.concat ","
    |> Printf.sprintf "--mount=type=%s,%s " t.mount_type

  type from_source = string option * string
  type sharing = Shared | Locked

  let string_of_sharing = function Shared -> "shared" | Locked -> "locked"

  let add_option name optval options =
    match optval with None -> options | Some v -> (name, v) :: options

  let bind ?(options = []) ?rw ~from_source:(from, source) target =
    options
    |> add_option "source" (Some source)
    |> add_option "from" from
    |> add_option "rw" (Option.map string_of_bool rw)
    |> v ~mount_type:"bind" ~target

  let cache ?(options = []) ?id ?ro ?sharing ?from_source ?mode
      ?uid ?gid target =
    options |> add_option "id" id
    |> add_option "ro" (Option.map string_of_bool ro)
    |> add_option "sharing" (Option.map string_of_sharing sharing)
    |> add_option "from" (Option.map fst from_source |> Option.join)
    |> add_option "source" (Option.map snd from_source)
    |> add_option "mode" (Option.map (Printf.sprintf "0%o") mode)
    |> add_option "uid" (Option.map string_of_int uid)
    |> add_option "gid" (Option.map string_of_int gid)
    |> v ~mount_type:"cache" ~target

  let tmpfs ?(options = []) target = v ~mount_type:"tmpfs" ~target options
  let secret ?(options = []) target = v ~mount_type:"secret" ~target options
end

type line =
  [ `ParserDirective of parser_directive
  | `Comment of string
  | `From of from
  | `Maintainer of string
  | `Run of Mount.t list * shell_or_exec
  | `Cmd of shell_or_exec
  | `Expose of int list
  | `Arg of string * string option
  | `Env of (string * string) list
  | `Add of sources_to_dest
  | `Copy of sources_to_dest
  | `Copy_heredoc of heredocs_to_dest
  | `Entrypoint of shell_or_exec
  | `Shell of string list
  | `Volume of string list
  | `User of string
  | `Workdir of string
  | `Onbuild of line
  | `Label of (string * string) list
  | `Healthcheck of healthcheck
  | `Stopsignal of string ]
[@@deriving sexp]

type t = line list [@@deriving sexp]

let ( @@ ) = ( @ )
let ( @@@ ) = List.fold_left (fun a b -> a @@ b)
let empty = []
let maybe f = function None -> empty | Some v -> f v

open Printf

(* Multiple RUN lines will be compressed into a single one in
   order to reduce the number of layers used *)
let crunch l =
  let pack l =
    let rec aux acc = function
      | [] -> acc
      | `Run (mounts1, `Shell a) :: `Run (mounts2, `Shell b) :: tl when mounts1 = mounts2->
          aux (`Run (mounts1, `Shells [a; b]) :: acc) tl
      | `Run (mounts1, `Shells a) :: `Run (mounts2, `Shell b) :: tl when mounts1 = mounts2 ->
          aux (`Run (mounts1, `Shells (a @ [b])) :: acc) tl
      | `Run (mounts1, `Shells a) :: `Run (mounts2, `Shells b) :: tl when mounts1 = mounts2->
          aux (`Run (mounts1, `Shells (a @ b)) :: acc) tl
      | hd :: tl -> aux (hd :: acc) tl
    in
    List.rev (aux [] l)
  in
  let rec fixp fn l =
    let a = fn l in
    if a = l then l else fixp fn a
  in
  fixp pack l

let quote s = sprintf "%S" s
let cmd c r = c ^ " " ^ r

let json_array_of_list sl =
  sprintf "[ %s ]" (String.concat ", " (List.map quote sl))

let string_of_shell_or_exec ~escape (t : shell_or_exec) =
  match t with
  | `Shell s -> s
  | `Shells [] -> ""
  | `Shells [ s ] -> s
  | `Shells l -> String.concat (" && " ^ String.make 1 escape ^ "\n  ") l
  | `Exec sl -> json_array_of_list sl

let quote_env_var = escape_string ~char_to_escape:'"'

let string_of_env_var ~escape (name, value) =
  sprintf {|%s="%s"|} name (quote_env_var ~escape value)

let string_of_env_list ~escape el =
  List.map (string_of_env_var ~escape) el |> String.concat " "

let string_of_arg ~escape = function
  | name, Some value -> string_of_env_var ~escape (name, value)
  | name, None -> name

let optional name = function
  | None -> []
  | Some value -> [ sprintf "%s=%s" name value ]

let optional_int name = function
  | None -> []
  | Some value -> [ sprintf "%s=%d" name value ]

let optional_flag name = function
  | Some true -> [ name ]
  | Some false | None -> []

let string_of_sources_to_dest (t : sources_to_dest) =
  let `From frm, `Src sl, `Dst d, `Chown chown, `Link link = t in
  String.concat " "
    (optional_flag "--link" link
    @ optional "--chown" chown @ optional "--from" frm
    @ [ json_array_of_list (sl @ [ d ]) ])

let string_of_label_list ls =
  List.map (fun (k, v) -> sprintf "%s=%S" k v) ls |> String.concat " "

let string_of_copy_heredoc (t : heredocs_to_dest) =
  let `Chown chown, heredocs, dst = t in
  let header, docs =
    List.fold_left
      (fun (header, docs) t ->
        ( sprintf "<<%s%s" (if t.strip then "-" else "") t.word :: header,
          sprintf "%s\n%s\n%s" docs t.here_document t.delimiter ))
      ([], "") heredocs
  in
  String.concat " " (optional "--chown" chown @ List.rev header @ [ dst ])
  ^ docs

let string_of_mounts ~escape map =
  map
  |> List.map Mount.(to_string ~escape)
  |> String.concat " "

let rec string_of_line ~escape (t : line) =
  match t with
  | `ParserDirective (`Escape c) -> cmd "#" ("escape=" ^ String.make 1 c)
  | `ParserDirective (`Syntax str) -> cmd "#" ("syntax=" ^ str)
  | `Comment c -> cmd "#" c
  | `From { image; tag; alias; platform } ->
      cmd "FROM"
        (String.concat ""
           [
             (match platform with
             | None -> ""
             | Some p -> "--platform=" ^ p ^ " ");
             image;
             (match tag with None -> "" | Some t -> ":" ^ t);
             (match alias with None -> "" | Some a -> " as " ^ a);
           ])
  | `Maintainer m -> cmd "MAINTAINER" m
  | `Run (mounts, c) ->
      cmd "RUN" (string_of_mounts ~escape mounts) ^ string_of_shell_or_exec ~escape c
  | `Cmd c -> cmd "CMD" (string_of_shell_or_exec ~escape c)
  | `Expose pl -> cmd "EXPOSE" (String.concat " " (List.map string_of_int pl))
  | `Arg a -> cmd "ARG" (string_of_arg ~escape a)
  | `Env el -> cmd "ENV" (string_of_env_list ~escape el)
  | `Add c -> cmd "ADD" (string_of_sources_to_dest c)
  | `Copy c -> cmd "COPY" (string_of_sources_to_dest c)
  | `Copy_heredoc c -> cmd "COPY" (string_of_copy_heredoc c)
  | `User u -> cmd "USER" u
  | `Volume vl -> cmd "VOLUME" (json_array_of_list vl)
  | `Entrypoint el -> cmd "ENTRYPOINT" (string_of_shell_or_exec ~escape el)
  | `Shell sl -> cmd "SHELL" (json_array_of_list sl)
  | `Workdir wd -> cmd "WORKDIR" wd
  | `Onbuild t -> cmd "ONBUILD" (string_of_line ~escape t)
  | `Label ls -> cmd "LABEL" (string_of_label_list ls)
  | `Stopsignal s -> cmd "STOPSIGNAL" s
  | `Healthcheck (`Cmd (opts, c)) ->
      cmd "HEALTHCHECK" (string_of_healthcheck ~escape opts c)
  | `Healthcheck `None -> "HEALTHCHECK NONE"

and string_of_healthcheck ~escape options c =
  String.concat " "
    (optional "--interval" options.interval
    @ optional "--timeout" options.timeout
    @ optional "--start-period" options.start_period
    @ optional_int "--retries" options.retries)
  ^ sprintf " %c\n  %s" escape (string_of_line ~escape (`Cmd c))

(* Function interface *)
let parser_directive pd : t = [ `ParserDirective pd ]

(* 1 should be enough for cache mounts, but here documents want 1.4 *)
let buildkit_syntax = parser_directive (`Syntax "docker/dockerfile:1")

let heredoc ?(strip = false) ?(word = "EOF") ?(delimiter = word) fmt =
  ksprintf (fun here_document -> { here_document; strip; word; delimiter }) fmt

let from ?alias ?tag ?platform image = [ `From { image; tag; alias; platform } ]
let comment fmt = ksprintf (fun c -> [ `Comment c ]) fmt
let maintainer fmt = ksprintf (fun m -> [ `Maintainer m ]) fmt

let run ?(mounts = []) fmt =
  ksprintf (fun b -> [ `Run (mounts, `Shell b) ]) fmt

let run_exec ?(mounts = []) cmds : t =
  [ `Run (mounts, `Exec cmds) ]

let prefix_mounts_line prefix = function
  | `Run (mounts, s) ->
      `Run (Mount.merge prefix mounts, s)
  | other -> other

let with_mounts prefix dockerfiles =
  dockerfiles |> List.concat |> crunch
  |> List.map (prefix_mounts_line prefix)

let cmd fmt = ksprintf (fun b -> [ `Cmd (`Shell b) ]) fmt
let cmd_exec cmds : t = [ `Cmd (`Exec cmds) ]
let expose_port p : t = [ `Expose [ p ] ]
let expose_ports p : t = [ `Expose p ]
let arg ?default a : t = [ `Arg (a, default) ]
let env e : t = [ `Env e ]

let add ?link ?chown ?from ~src ~dst () : t =
  [ `Add (`From from, `Src src, `Dst dst, `Chown chown, `Link link) ]

let copy ?link ?chown ?from ~src ~dst () : t =
  [ `Copy (`From from, `Src src, `Dst dst, `Chown chown, `Link link) ]

let copy_heredoc ?chown ~src ~dst () : t =
  [ `Copy_heredoc (`Chown chown, src, dst) ]

let user fmt = ksprintf (fun u -> [ `User u ]) fmt
let onbuild t = List.map (fun l -> `Onbuild l) t
let volume fmt = ksprintf (fun v -> [ `Volume [ v ] ]) fmt
let volumes v : t = [ `Volume v ]
let label ls = [ `Label ls ]
let entrypoint fmt = ksprintf (fun e -> [ `Entrypoint (`Shell e) ]) fmt
let entrypoint_exec e : t = [ `Entrypoint (`Exec e) ]
let shell s : t = [ `Shell s ]
let workdir fmt = ksprintf (fun wd -> [ `Workdir wd ]) fmt
let stopsignal s = [ `Stopsignal s ]

let healthcheck ?interval ?timeout ?start_period ?retries fmt =
  let opts = { interval; timeout; start_period; retries } in
  ksprintf (fun b -> [ `Healthcheck (`Cmd (opts, `Shell b)) ]) fmt

let healthcheck_exec ?interval ?timeout ?start_period ?retries cmds : t =
  let opts = { interval; timeout; start_period; retries } in
  [ `Healthcheck (`Cmd (opts, `Exec cmds)) ]

let healthcheck_none () : t = [ `Healthcheck `None ]

let string_of_t tl =
  let rec find_escape = function
    | `ParserDirective (`Escape c) :: _ -> c
    | `ParserDirective _ :: tl -> find_escape tl
    | _ -> '\\'
  in
  String.concat "\n" (List.map (string_of_line ~escape:(find_escape tl)) tl)

let pp ppf tl = Fmt.pf ppf "%s" (string_of_t tl)
