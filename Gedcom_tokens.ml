open Gedcom
let getstr = function
| ACCEPT  -> "ACCEPT"
| ADDR s -> "ADDR "^s
| ADR1 s -> "ADR1 "^s
| ADOP  -> "ADOP"
| AGE s -> "AGE "^s
| ALIA  -> "ALIA"
| AMPERSAND  -> "AMPERSAND"
| AT  -> "AT"
| AUTH s -> "AUTH "^s
| BACKQUOTE  -> "BACKQUOTE"
| BACKSLASH  -> "BACKSLASH"
| BAPL  -> "BAPL"
| BAPM  -> "BAPM"
| BARM  -> "BARM"
| BASM  -> "BASM"
| BIRT  -> "BIRT"
| BURI  -> "BURI"
| CARET  -> "CARET"
| CAUS s -> "CAUS "^s
| ASCII_187 -> "ASCII_187"
| ASCII_191 -> "ASCII_191"
| ASCII_239 -> "ASCII_239"
| ASCII n -> "ASCII "^string_of_int n
| CHIL  -> "CHIL"
| CHR  -> "CHR"
| CENS  -> "CENS"
| CHAR  -> "CHAR"
| CITY  -> "CITY"
| COLON  -> "COLON"
| COMMA  -> "COMMA"
| CONC s -> "CONC "^s
| CONT s -> "CONT "^s
| CORP  -> "CORP"
| CTRY  -> "CTRY"
| Cont  -> "Cont"
| DATE d -> "DATE "^d
| DEAT  -> "DEAT"
| DEBUG  -> "DEBUG"
| DEFAULT  -> "DEFAULT"
| DEST  -> "DEST"
| DIV  -> "DIV"
| DOLLAR  -> "DOLLAR"
| DOT  -> "DOT"
| DOUBLEQUOTE  -> "DOUBLEQUOTE"
| EMAIL s -> "EMAIL "^s
| EMPTY_TOKEN  -> "EMPTY_TOKEN"
| END  -> "END"
| EOF_TOKEN  -> "EOF_TOKEN"
| ERROR_TOKEN  -> "ERROR_TOKEN"
| EVEN  -> "EVEN"
| Error  -> "Error"
| FAMC  -> "FAMC"
| FAMS  -> "FAMS"
| FIL  -> "FIL"
| FORM  -> "FORM"
| Fam  -> "Fam"
| GEDC  -> "GEDC"
| GIVN s -> "GIVN "^s
| NICK s -> "NICK "^s
| GREATER  -> "GREATER"
| HYPHEN  -> "HYPHEN"
| HASH  -> "HASH"
| HUSB  -> "HUSB"
| INIT  -> "INIT"
| INDI  -> "INDI"
| LANGUAGE  _ -> "LANGUAGE"
| LBRACE  -> "LBRACE"
| LBRACK  -> "LBRACK"
| LESS  -> "LESS"
| LINEFEED  -> "LINEFEED"
| MARR  -> "MARR"
| NAME s -> "NAME: "^s
| NATI  -> "NATI"
| NATU  -> "NATU"
| NOTE s -> "NOTE "^s
| Nam  _ -> "Nam"
| OCCU s -> "OCCU "^s
| PAGE s -> "PAGE "^s
| PERCENT  -> "PERCENT"
| PHON  -> "PHON"
| PLAC s -> "PLAC "^s
| RELI s -> "RELI "^s
| PLING  -> "PLING"
| QUAY  -> "QUAY"
| QUERY  -> "QUERY"
| QUOTE  -> "QUOTE"
| RBRACE  -> "RBRACE"
| RBRACK  -> "RBRACK"
| FAM  -> "FAM"
| SEX  -> "SEX"
| STAR  -> "STAR"
| SURN s -> "SURN "^s
| NSFX  -> "NSFX"
| SLIST  _ -> "SLIST"
| SOUR  -> "SOUR"
| STRING s -> "STRING "^s
| SUBM  -> "SUBM"
| Sub  -> "Sub"
| TEXT s -> "TEXT "^s
| TILDE  -> "TILDE"
| TITL s -> "TITL "^s
| PUBL s -> "PUBL "^s
| TLIST  _ -> "TLIST"
| TUPLE2  _ -> "TUPLE2"
| TUPLE3  _ -> "TUPLE3"
| TUPLE4  _ -> "TUPLE4"
| TUPLE5  _ -> "TUPLE5"
| UNDERSCORE  -> "UNDERSCORE"
| VBAR  -> "VBAR"
| VERS  -> "VERS"
| WIFE  -> "WIFE"
| TIME s -> "TIME "^s
| SUBN -> "SUBN"
| LANG -> "LANG"
| HEAD -> "HEAD"
| FILE -> "FILE"
| DATA -> "DATA"
| COPR -> "COPR"
| RESI -> "RESI"
| RIN -> "RIN"
| RINS -> "RINS"
| TYPE s -> "TYPE "^s
| PUBLISH -> "_PUBLISH"
| UNDERSCORE_KW (k,s) -> "_UNDERSCORE_KW("^k^", "^s^")"
| NUM n -> "NUM "^string_of_int n
| HEXN s -> "HEXN "^s
| SEMICOLON -> "SEMICOLON"
| LPAREN -> "LPAREN"
| RPAREN -> "RPAREN"
| SLASH -> "SLASH"
| EQUALS -> "EQUALS"
| TRLR -> "TRLR"
| LEV0 -> "LEV0"
| LEV1 -> "LEV1"
| LEV2 -> "LEV2"

let (typehash:(string,unit)Hashtbl.t) = Hashtbl.create 257

let (packhash:(string,unit)Hashtbl.t) = Hashtbl.create 257
