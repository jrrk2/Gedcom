%{
  open Parsing
  open Gedcom_types
  let stderr = open_out "parser_stderr.log" (* to capture parser trace mode info *)
  let declst = ref []
  let packhash_add id_t = Hashtbl.add packhash id_t ()
  let typehash_add id_t = Hashtbl.add typehash id_t ()
%}

%token LEV0
%token LEV1
%token LEV2
%token TRLR
%token <string> PUBL
%token <string> SURN
%token FAM
%token CENS
%token RESI
%token NSFX
%token <string> TYPE
%token <string> RELI
%token QUAY
%token LPAREN
%token RPAREN
%token HYPHEN
%token SEMICOLON
%token STAR
%token <string> GIVN
%token <string> NICK
%token <string> TEXT
%token <string> CONC
%token <string> CONT
%token <string> HEXN
%token INDI
%token <string*string> UNDERSCORE_KW
%token  RIN
%token  RINS
%token <string> PAGE
%token  ACCEPT
%token <string> ADDR
%token <string> ADR1
%token  ADOP
%token <string> AGE
%token  ALIA
%token  AMPERSAND
%token  AT
%token <string> AUTH
%token  BACKQUOTE
%token  SLASH
%token  BACKSLASH
%token  BAPL
%token  BAPM
%token  BARM
%token  BASM
%token  BIRT
%token  BURI
%token  CARET
%token <string> CAUS
%token  CHAR
%token  CHIL
%token  CHR
%token  CITY
%token  COLON
%token  COMMA
%token <string> TIME
%token  COPR
%token  FILE
%token <int> NUM
%token <int> ASCII
%token  ASCII_187
%token  ASCII_191
%token  ASCII_239
%token  LANG
%token  DATA
%token  HEAD
%token  SUBN
%token  CORP
%token  CTRY
%token  Cont
%token <string> DATE
%token  DEAT
%token  DEBUG
%token  DEFAULT
%token  DEST
%token  DIV
%token  DOLLAR
%token  DOT
%token  DOUBLEQUOTE
%token  EQUALS
%token <string> EMAIL
%token  EMPTY_TOKEN
%token  END
%token  EOF_TOKEN
%token  ERROR_TOKEN
%token  EVEN
%token  Error
%token  FAMC
%token  FAMS
%token  FIL
%token  FORM
%token  Fam
%token  GEDC
%token  GREATER
%token  HASH
%token  HUSB
%token  INIT
%token <string> LANGUAGE
%token  LBRACE
%token  LBRACK
%token  LESS
%token  LINEFEED
%token  MARR
%token <string> NAME
%token  NATI
%token  NATU
%token <string> NOTE
%token <string> Nam
%token <string> OCCU
%token  PERCENT
%token  PHON
%token <string> PLAC
%token  PLING
%token  QUERY
%token  QUOTE
%token  RBRACE
%token  RBRACK
%token  SEX
%token <string list> SLIST
%token  SOUR
%token <string> STRING
%token  SUBM
%token  Sub
%token  TILDE
%token <string> TITL
%token  PUBLISH
%token <token list> TLIST
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token  UNDERSCORE
%token  VBAR
%token  VERS
%token  WIFE
%type <token list> start
%start start
%%

start: ASCII_239 ASCII_187 ASCII_191 BODY EOF_TOKEN { $4 }

BODY: lev0_lst { List.rev $1 }

lev0:
  | HEAD lev1_lst { TUPLE2(HEAD, TLIST (List.rev $2)) }
  | PUBLISH lev1_lst { TUPLE2(PUBLISH, TLIST (List.rev $2)) }
  | AT STRING AT SUBM lev1_lst { TUPLE4(AT, STRING $2, SUBM, TLIST (List.rev $5)) }
  | AT STRING AT INDI lev1_lst { TUPLE4(AT, STRING $2, INDI, TLIST (List.rev $5)) }
  | AT STRING AT FAM lev1_lst { TUPLE4(AT, STRING $2, FAM, TLIST (List.rev $5)) }
  | AT STRING AT SOUR lev1_lst { TUPLE4(AT, STRING $2, SOUR, TLIST (List.rev $5)) }
  | TRLR { TRLR }

lev0_lst: { [] }
  | lev0_lst LEV0 lev0 { $3 :: $1 }

lev1_lst: { [] }
  | lev1_lst LEV1 lev1 { $3 :: $1 }

lev2_lst: { [] }
  | lev2_lst LEV2 lev2 { $3 :: $1 }

string_lst: { [] }
  | string_lst STRING { STRING $2 :: $1 }
  | string_lst ADDR { STRING "ADDR" :: $1 }

surn: STRING { STRING $1 }
  | STAR { STRING "*" }
  
lev2:
  | VERS VERSION_NUMBER {TUPLE2( VERS, TLIST (List.rev $2)) }
  | FORM GEDCOM_FORM { TUPLE2(FORM, $2) }
  | GIVN { GIVN $1 }
  | NICK { NICK $1 }
  | SURN { SURN $1 }
  | NSFX STRING { TUPLE2(NSFX, STRING $2) }
  | UNDERSCORE_KW { UNDERSCORE_KW $1 }
  | RIN STRING COLON STRING { TUPLE3(RIN, STRING $2, STRING $4) }
  | NAME { NAME $1 }
  | CORP NAME_OF_BUSINESS { TUPLE2(CORP, $2) }
  | DATE { DATE $1 }
  | TIME { TIME $1 }
  | PLAC { PLAC $1 }
  | TYPE { TYPE $1 }
  | EMAIL { EMAIL $1 }
  | CAUS { CAUS $1 }
  | ADDR lev3_lst { TUPLE2(ADDR $1, TLIST (List.rev $2)) }
  | EVEN lev3_lst { TUPLE2(EVEN, TLIST (List.rev $2)) }
  | DATA lev3_lst { TUPLE2(DATA, TLIST (List.rev $2)) }
  | QUAY NUM { TUPLE2(QUAY, NUM $2) }
  | PAGE { PAGE $1 }
  | NOTE lev3_lst { TUPLE2 (NOTE $1, TLIST (List.rev $2)) }
  | AGE { AGE $1 }
  
lev3_lst: { [] }
  | lev3_lst lev3 { $2 :: $1 }

lev4_con: { [] }
  | lev4_con NUM CONC { CONC $3 :: $1 }
  | lev4_con NUM CONT { CONT $3 :: $1 }

lev3:
  | NUM { NUM $1 }
  | ADR1 { ADR1 $1 }
  | DATE { DATE $1 }
  | TEXT lev4_con { TUPLE2(TEXT $1, TLIST (List.rev $2)) }
  | HEXN { HEXN $1 }
  | STRING { STRING $1 }
  | EQUALS { EQUALS }
  | DOUBLEQUOTE { DOUBLEQUOTE }
  | QUOTE { QUOTE }
  | QUERY { QUERY }
  | HASH { HASH }
  | SLASH { SLASH }
  | COMMA { COMMA }
  | LPAREN { LPAREN }
  | RPAREN { RPAREN }
  | AMPERSAND { AMPERSAND }
  | COLON { COLON }
  | SEMICOLON { SEMICOLON }
  | HYPHEN { HYPHEN }
  | UNDERSCORE_KW { UNDERSCORE_KW $1 }

lev1: 
  | GEDC lev2_lst { TUPLE2(GEDC, TLIST (List.rev $2)) }
  | CHAR CHARACTER_SET { TUPLE2(CHAR, $2) }
  | LANG LANGUAGE_OF_TEXT { TUPLE2(LANG, $2) }
  | SOUR APPROVED_SYSTEM_ID lev2_lst { TUPLE3(SOUR, $2, TLIST (List.rev $3)) }
  | SUBM AT XREF AT { TUPLE2(SUBM, $3) }
  | DEST RECEIVING_SYSTEM_NAME { TUPLE2(DEST, $2) }
  | DATE lev2_lst { TUPLE2(DATE $1, TLIST (List.rev $2)) }
  | RINS RINS_LST { TUPLE2(RINS, TLIST (List.rev $2)) }
  | RIN STRING COLON STRING { TUPLE3(RIN, STRING $2, STRING $4) }
  | NAME lev2_lst { TUPLE2(NAME $1, TLIST (List.rev $2)) }
  | EMAIL { EMAIL $1 }
  | UNDERSCORE_KW { UNDERSCORE_KW $1 }
  | SEX STRING { TUPLE2(SEX, STRING $2) }
  | BIRT lev2_lst { TUPLE2(BIRT, TLIST (List.rev $2)) }
  | RESI lev2_lst { TUPLE2(RESI, TLIST (List.rev $2)) }
  | FAMC AT XREF AT { TUPLE2(FAMC, $3) }
  | DEAT lev2_lst { TUPLE2(DEAT, TLIST (List.rev $2)) }
  | BURI lev2_lst { TUPLE2(BURI, TLIST (List.rev $2)) }
  | CENS lev2_lst { TUPLE2(CENS, TLIST (List.rev $2)) }
  | OCCU lev2_lst { TUPLE2(OCCU $1, TLIST (List.rev $2)) }
  | FAMS AT XREF AT { TUPLE2(FAMS, $3) }
  | SOUR AT XREF AT lev2_lst { TUPLE3(SOUR, $3, TLIST (List.rev $5)) }
  | EVEN lev2_lst { TUPLE2(EVEN, TLIST (List.rev $2)) }
  | CHR lev2_lst { TUPLE2(CHR, TLIST (List.rev $2)) }
  | NOTE lev2_con { TUPLE2 (NOTE $1, TLIST (List.rev $2)) }
  | BAPM lev2_lst { TUPLE2(BAPM, TLIST (List.rev $2)) }
  | RELI lev2_lst { TUPLE2(RELI $1, TLIST (List.rev $2)) }
  | HUSB AT XREF AT { TUPLE2(HUSB, $3) }
  | WIFE AT XREF AT { TUPLE2(WIFE, $3) }
  | CHIL AT XREF AT { TUPLE2(CHIL, $3) }
  | MARR lev2_lst { TUPLE2(MARR, TLIST (List.rev $2)) }
  | TITL { TITL $1 }
  | PUBL { PUBL $1 }
  | TEXT lev2_con { TUPLE2(TEXT $1, TLIST (List.rev $2)) }
  | AUTH { AUTH $1 }
  
lev2_con: { [] }
  | lev2_con LEV2 CONC { CONC $3 :: $1 }
  | lev2_con LEV2 CONT { CONT $3 :: $1 }
  
VERSION_NUMBER: NUM { NUM $1 :: [] }
  | VERSION_NUMBER DOT num { $3 :: $1 }

RINS_LST: STRING { STRING $1 :: [] }
  | RINS_LST COMMA STRING { STRING $3 :: $1 }

num: NUM { NUM $1 }
| LEV0 { NUM 0 }
| LEV1 { NUM 1 }
| LEV2 { NUM 2 }

XREF: STRING { STRING $1 }

NAME_OF_BUSINESS: STRING { STRING $1 }

LANGUAGE_OF_TEXT: STRING { STRING $1 }

RECEIVING_SYSTEM_NAME: STRING { STRING $1 }

GEDCOM_FORM: STRING { STRING $1 }

CHARACTER_SET: STRING { STRING $1 }

APPROVED_SYSTEM_ID: STRING { STRING $1 }
