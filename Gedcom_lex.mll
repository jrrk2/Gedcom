(**************************************************************************)
(*                                                                        *)
(* OCaml template Copyright (C) 2004-2010                                 *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(* Adapted to boolean logic by Jonathan Kimmitt                           *)
(*  Copyright 2016 University of Cambridge                                *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

{
  open Lexing
  open Gedcom
  open Gedcom_types

  let verbose = try int_of_string(Sys.getenv "LEXER_VERBOSE") > 0 with _ -> false
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
(ACCEPT, "ACCEPT");
(ADOP, "ADOP");
(ALIA, "ALIA");
(AMPERSAND, "AMPERSAND");
(AT, "AT");
(BACKQUOTE, "BACKQUOTE");
(BACKSLASH, "BACKSLASH");
(BAPL, "BAPL");
(BAPM, "BAPM");
(BARM, "BARM");
(BASM, "BASM");
(BIRT, "BIRT");
(BURI, "BURI");
(CARET, "CARET");
(CHIL, "CHIL");
(CHR, "CHR");
(CHAR, "CHAR");
(CITY, "CITY");
(COLON, "COLON");
(COMMA, "COMMA");
(CORP, "CORP");
(CTRY, "CTRY");
(Cont, "Cont");
(DEAT, "DEAT");
(DEBUG, "DEBUG");
(DEFAULT, "DEFAULT");
(DEST, "DEST");
(DIV, "DIV");
(DOLLAR, "DOLLAR");
(DOT, "DOT");
(DOUBLEQUOTE, "DOUBLEQUOTE");
(EMPTY_TOKEN, "EMPTY_TOKEN");
(END, "END");
(EOF_TOKEN, "EOF_TOKEN");
(ERROR_TOKEN, "ERROR_TOKEN");
(EVEN, "EVEN");
(Error, "Error");
(FAMC, "FAMC");
(FAMS, "FAMS");
(FIL, "FIL");
(FORM, "FORM");
(Fam, "Fam");
(GEDC, "GEDC");
(GREATER, "GREATER");
(HASH, "HASH");
(HUSB, "HUSB");
(INIT, "INIT");
(INDI, "INDI");
(LBRACE, "LBRACE");
(LBRACK, "LBRACK");
(LESS, "LESS");
(LINEFEED, "LINEFEED");
(MARR, "MARR");
(NATI, "NATI");
(NATU, "NATU");
(PERCENT, "PERCENT");
(PHON, "PHON");
(PLING, "PLING");
(QUERY, "QUERY");
(QUOTE, "QUOTE");
(RBRACE, "RBRACE");
(RBRACK, "RBRACK");
(SEX, "SEX");
(SOUR, "SOUR");
(SUBM, "SUBM");
(Sub, "Sub");
(TILDE, "TILDE");
(TRLR, "TRLR");
(UNDERSCORE, "UNDERSCORE");
(VBAR, "VBAR");
(VERS, "VERS");
(WIFE, "WIFE");
(SUBN, "SUBN");
(LANG, "LANG");
(HEAD, "HEAD");
(FILE, "FILE");
(DATA, "DATA");
(COPR, "COPR");
(CENS, "CENS");
(PUBLISH, "_PUBLISH");
(RIN, "RIN");
(FAM, "FAM");
(RESI, "RESI");
(QUAY, "QUAY");
(NSFX, "NSFX");
];
    fun s -> Hashtbl.find h s

let tok' x = "\""^Gedcom_tokens.getstr x^"\""

let import_seen = ref false

let tok arg =
  if false then print_endline (tok' arg);
  arg
}

let ident = ['a'-'z' 'A'-'Z' '$' '_' '\\'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '$' '\\' '.' '[' ']' '/' '-']*
let number = ['0'-'9']['0'-'9' 'a'-'f' 'A'-'F']*
let space = [' ' '\t' '\r']+
let newline = ['\n']
let qstring = '"'[^'"']*'"'
let name = "NAME "[^'\r']*
let author = "AUTH "[^'\r']*
let title = "TITL "[^'\r']*
let publisher = "PUBL "[^'\r']*
let surname = "SURN "[^'\r']*
let religion = "RELI "[^'\r']*
let type = "TYPE "[^'\r']*
let note = "NOTE "[^'\r']*
let page = "PAGE "[^'\r']*
let age = "AGE "[^'\r']*
let text = "TEXT "[^'\r']*
let conc = "CONC "[^'\r']*
let cont = "CONT"[^'\r']*
let address = "ADDR"[^'\r']*
let address1 = "ADR1"[^'\r']*
let occupation = "OCCU"[^'\r']*
let cause = "CAUS "[^'\r']*
let place = "PLAC "[^'\r']*
let email = "EMAIL "[^'\r']*
let given = "GIVN "[^'\r']*
let nickname = "NICK "[^'\r']*
let date = "DATE "[^'\r']*
let time = "TIME "[^'\r']*
let underscore = '_'['A'-'Z' '_']+' '[^'\r']*
   
rule token = parse
| '#' { tok ( HASH ) }
| ''' { tok ( QUOTE ) }
| '[' { tok ( LBRACK ) }
| '{' { tok ( LBRACE ) }
| '<' { tok ( LESS ) }
| ']' { tok ( RBRACK ) }
| '}' { tok ( RBRACE ) }
| ',' { tok ( COMMA ) }
| ':' { tok ( COLON ) }
| '|' { tok ( VBAR ) }
| '~' { tok ( TILDE ) }
| '.' { tok ( DOT ) }
| '@' { tok ( AT ) }
| '-' { tok ( HYPHEN ) }
| ';' { tok ( SEMICOLON ) }
| '&' { tok ( AMPERSAND ) }
| '(' { tok ( LPAREN ) }
| ')' { tok ( RPAREN ) }
| '/' { tok ( SLASH ) }
| '=' { tok ( EQUALS ) }
| '"' { tok ( DOUBLEQUOTE ) }
| '?' { tok ( QUERY ) }
| '*' { tok ( STAR ) }
(*
| '+' { tok ( PLUS ) }
| '!' { tok ( PLING ) }
| '$' { tok ( DOLLAR ) }
| '%' { tok ( PERCENT ) }
| '>' { tok ( GREATER ) }
| '\\' { tok ( BACKSLASH ) }
| '^' { tok ( CARET ) }
| '_' { tok ( UNDERSCORE ) }
*)

  | name as s
      { tok (NAME (String.sub s 5 (String.length s - 5))) }
  | author as s
      { tok (AUTH (String.sub s 5 (String.length s - 5))) }
  | title as s
      { tok (NAME (String.sub s 5 (String.length s - 5))) }
  | publisher as s
      { tok (NAME (String.sub s 5 (String.length s - 5))) }
  | surname as s
      { tok (SURN (String.sub s 5 (String.length s - 5))) }
  | religion as s
      { tok (RELI (String.sub s 5 (String.length s - 5))) }
  | type as s
      { tok (TYPE (String.sub s 5 (String.length s - 5))) }
  | note as s
      { tok (NOTE (String.sub s 5 (String.length s - 5))) }
  | page as s
      { tok (PAGE (String.sub s 5 (String.length s - 5))) }
  | age as s
      { tok (AGE (String.sub s 4 (String.length s - 4))) }
  | text as s
      { tok (TEXT (String.sub s 5 (String.length s - 5))) }
  | conc as s
      { tok (CONC (String.sub s 5 (String.length s - 5))) }
  | cont as s
      { tok (CONT (String.sub s 4 (String.length s - 4))) }
  | address as s
      { tok (ADDR (String.sub s 4 (String.length s - 4))) } 
  | address1 as s
      { tok (ADR1 (String.sub s 4 (String.length s - 4))) } 
  | occupation as s
      { tok (OCCU (String.sub s 4 (String.length s - 4))) }
  | cause as s
      { tok (CAUS (String.sub s 5 (String.length s - 5))) }
  | place as s
      { tok (PLAC (String.sub s 5 (String.length s - 5))) }
  | given as s
      { tok (GIVN (String.sub s 5 (String.length s - 5))) }
  | nickname as s
      { tok (NICK (String.sub s 5 (String.length s - 5))) }
  | email as s
      { tok (EMAIL (String.sub s 6 (String.length s - 6))) }
  | date as s
      { tok (DATE (String.sub s 5 (String.length s - 5))) }
  | time as s
      { tok (TIME (String.sub s 5 (String.length s - 5))) }
  | underscore as s
      { tok (let blnk = String.index s ' ' in UNDERSCORE_KW (String.sub s 1 (blnk-1), String.sub s (blnk+1) (String.length s - blnk - 2) )) }
  | space
      { token lexbuf }
  | newline
      { incr lincnt; token lexbuf }
  | number as n
      { tok ( try (match int_of_string n with 0 -> LEV0 | 1 -> LEV1 | 2 -> LEV2 | oth -> NUM oth) with _ -> HEXN n ) }
  | ident as s
      { tok ( try keyword s with Not_found -> STRING s ) }
(*
  | qstring as s
      { tok ( TOK_STRING (String.sub s 1 (String.length s - 2)) ) }
*)
  | eof { tok ( EOF_TOKEN ) }
  | _ as oth { tok ( match int_of_char oth with 187 -> ASCII_187 | 191 -> ASCII_191 | 239 -> ASCII_239 | oth -> ASCII oth ) }

and comment = parse
| newline { incr lincnt; comment lexbuf }
| '*''/' as com { if false then print_endline ("/*"^com); token lexbuf }
| '*'')' as com { if false then print_endline ("/*"^com); token lexbuf }
| _ { comment lexbuf }

(* pre-processing should have removed this stuff, but if not, skip over it *)
   
and ifdef = parse
| "`endif" { token lexbuf }
| newline { incr lincnt; ifdef lexbuf }
| _ { ifdef lexbuf }

and ifndef = parse
| "`endif" { token lexbuf }
| newline { incr lincnt; ifndef lexbuf }
| _ { ifndef lexbuf }
