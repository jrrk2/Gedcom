open Gedcom_lex
open Gedcom_rewrite_types
open Gedcom
open Printf

let indih = Hashtbl.create 65535
let famh = Hashtbl.create 65535

let conc = function CONC t -> t | CONT t -> t | oth -> failwith (Gedcom_tokens.getstr oth)

let rec rw = function
| TUPLE2(TEXT t, TLIST lst) -> TEXT (t ^ String.concat "" (List.map conc lst))
| TLIST lst -> TLIST (List.map rw lst)
| TUPLE2(arg1,arg2) -> TUPLE2 (rw arg1, rw arg2)
| TUPLE3(arg1,arg2,arg3) -> TUPLE3 (rw arg1, rw arg2, rw arg3)
| TUPLE4(AT, STRING kw, SUBM, TLIST lst) as tup -> print_endline ("submitter "^kw); tup
| TUPLE4(AT, STRING kw, INDI, TLIST lst) as tup -> Hashtbl.add indih kw lst; tup
| TUPLE4(AT, STRING kw, FAM, TLIST lst) as tup -> Hashtbl.add famh kw lst; tup
| TUPLE4(arg1,arg2,arg3,arg4) -> TUPLE4 (rw arg1, rw arg2, rw arg3, rw arg4)
| TUPLE5(arg1,arg2,arg3,arg4,arg5) -> TUPLE5 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5)
| x -> x

let parse_output_ast_from_chan ch =
  let lb = Lexing.from_channel ch in
  let output = try
      start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  output

let parse arg =
  let ch = open_in arg in
  let p = parse_output_ast_from_chan ch in
  close_in ch;
  List.map rw p
