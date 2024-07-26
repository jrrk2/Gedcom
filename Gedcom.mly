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
%token <token> CONS1
%token <token*token> CONS2
%token <token*token*token> CONS3
%token <token*token*token*token> CONS4
%token <token*token*token*token*token> CONS5
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
%token <token list> ELIST
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
%type <token> ml_start
%start ml_start
%%

ml_start: ASCII_239 ASCII_187 ASCII_191 BODY EOF_TOKEN { TUPLE3(STRING("ml_start1"),$4,EOF_TOKEN) }

Gedcom: INIT Header Body END { TUPLE5(STRING("Gedcom1"),INIT,$2,$3,END) }

cont: Cont continuation { TUPLE3(STRING("cont1"),Cont,$2) }
	|	/* empty */ { EMPTY_TOKEN }

continuation: CONT cont { TUPLE3(STRING("continuation1"),CONT,$2) }
	|	CONC cont { TUPLE3(STRING("continuation2"),CONC,$2) }

Header: HeaderLine HeaderLines { TUPLE3(STRING("Header1"),$1,$2) }

HeaderLines: HeaderLine HeaderLines { TUPLE3(STRING("HeaderLines1"),$1,$2) }
	|	/* empty */ { EMPTY_TOKEN }

HeaderLine: HeaderTag { ($1) }

HeaderTag: SOUR cont SdList { TUPLE4(STRING("HeaderTag1"),SOUR,$2,$3) }
	|	DEST cont SdList { TUPLE4(STRING("HeaderTag2"),DEST,$2,$3) }
	|	DATE cont Cont { TUPLE4(STRING("HeaderTag3"),DATE,$2,Cont) }
	|	FileStruct Cont { TUPLE3(STRING("HeaderTag4"),$1,Cont) }
	|	SUBM Sub { TUPLE3(STRING("HeaderTag5"),SUBM,Sub) }
	|	AUTH cont { TUPLE3(STRING("HeaderTag6"),AUTH,$2) }
	|	GEDC SdList { TUPLE3(STRING("HeaderTag7"),GEDC,$2) }

SdList: Sd SdList { CONS2($1,$2) }
	|	/* empty */ { EMPTY_TOKEN }

Sd: NAME Nam Cont { TUPLE4(STRING("Sd1"),NAME,Nam $2,Cont) }
	|	VERS cont Cont { TUPLE4(STRING("Sd2"),VERS,$2,Cont) }
	|	CORP cont Cont CorpList { TUPLE5(STRING("Sd3"),CORP,$2,Cont,$4) }
	|	FORM cont { TUPLE3(STRING("Sd4"),FORM,$2) }

CorpList: Corp Cont CorpList { CONS3($1,Cont,$3) }
	|	/* empty */ { EMPTY_TOKEN }

Corp: ADDR cont { TUPLE3(STRING("Corp1"),ADDR,$2) }
	|	PHON cont { TUPLE3(STRING("Corp2"),PHON,$2) }

FileStruct: FIL cont { TUPLE3(STRING("FileStruct1"),FIL,$2) }
	|	CHAR cont { TUPLE3(STRING("FileStruct2"),CHAR,$2) }
	|	LANGUAGE cont { TUPLE3(STRING("FileStruct3"),LANGUAGE $1,$2) }

Body: BodyLine BodyList { TUPLE3(STRING("Body1"),$1,$2) }

BodyList: BodyLine BodyList { CONS2($1,$2) }
	|	/* empty */ { EMPTY_TOKEN }

BodyLine: TagId { ($1) }

Tags: Tag TagList { TUPLE3(STRING("Tags1"),$1,$2) }

TagList: Tag TagList { CONS2($1,$2) }
	|	/* empty */ { EMPTY_TOKEN }

Tag: ContextlessTag Cont { TUPLE3(STRING("Tag1"),$1,Cont) }
	|	Event { ($1) }

ContextlessTag: NAME Nam { TUPLE3(STRING("ContextlessTag1"),NAME,Nam $2) }
	|	TITL cont { TUPLE3(STRING("ContextlessTag2"),TITL,$2) }
	|	NATI cont { TUPLE3(STRING("ContextlessTag3"),NATI,$2) }
	|	NOTE cont { TUPLE3(STRING("ContextlessTag4"),NOTE,$2) }
	|	NATU cont { TUPLE3(STRING("ContextlessTag5"),NATU,$2) }
	|	ALIA cont { TUPLE3(STRING("ContextlessTag6"),ALIA,$2) }
	|	EMAIL cont { TUPLE3(STRING("ContextlessTag7"),EMAIL,$2) }
	|	OCCU cont { TUPLE3(STRING("ContextlessTag8"),OCCU,$2) }
	|	Famx { ($1) }
	|	FamElem { ($1) }
	|	ADDR cont { TUPLE3(STRING("ContextlessTag11"),ADDR,$2) }
	|	PHON cont { TUPLE3(STRING("ContextlessTag12"),PHON,$2) }
	|	DEBUG { (DEBUG) }
	|	DEST cont { TUPLE3(STRING("ContextlessTag14"),DEST,$2) }
	|	AGE cont { TUPLE3(STRING("ContextlessTag15"),AGE,$2) }
	|	SEX cont { TUPLE3(STRING("ContextlessTag16"),SEX,$2) }

Famx: FAMS Fam { TUPLE3(STRING("Famx1"),FAMS,Fam) }
	|	FAMC Fam { TUPLE3(STRING("Famx2"),FAMC,Fam) }

Event: DEAT EventTail { TUPLE3(STRING("Event1"),DEAT,$2) }
	|	BIRT EventTail { TUPLE3(STRING("Event2"),BIRT,$2) }
	|	BURI EventTail { TUPLE3(STRING("Event3"),BURI,$2) }
	|	CHR EventTail { TUPLE3(STRING("Event4"),CHR,$2) }
	|	BAPL EventTail { TUPLE3(STRING("Event5"),BAPL,$2) }
	|	BAPM EventTail { TUPLE3(STRING("Event6"),BAPM,$2) }
	|	BARM EventTail { TUPLE3(STRING("Event7"),BARM,$2) }
	|	BASM EventTail { TUPLE3(STRING("Event8"),BASM,$2) }
	|	EVEN cont EventTail { TUPLE4(STRING("Event9"),EVEN,$2,$3) }

EventTail: Param ParamList { TUPLE3(STRING("EventTail1"),$1,$2) }

ParamList: Param ParamList { CONS2($1,$2) }
	|	/* empty */ { EMPTY_TOKEN }

Param: PLAC cont { TUPLE3(STRING("Param1"),PLAC,$2) }
	|	CITY cont { TUPLE3(STRING("Param2"),CITY,$2) }
	|	CTRY cont { TUPLE3(STRING("Param3"),CTRY,$2) }
	|	DATE cont { TUPLE3(STRING("Param4"),DATE,$2) }
	|	CAUS cont { TUPLE3(STRING("Param5"),CAUS,$2) }

TagId: Fam Family { TUPLE3(STRING("TagId1"),Fam,$2) }
	|	INDI Tags { TUPLE3(STRING("TagId2"),INDI,$2) }
	|	Sub Tags { TUPLE3(STRING("TagId3"),Sub,$2) }

Family: FamElem FamList { TUPLE3(STRING("Family1"),$1,$2) }

FamList: FamElem FamList { CONS2($1,$2) }
	|	/* empty */ { EMPTY_TOKEN }

FamElem: HUSB INDI { TUPLE3(STRING("FamElem1"),HUSB,INDI) }
	|	WIFE INDI { TUPLE3(STRING("FamElem2"),WIFE,INDI) }
	|	CHIL INDI { TUPLE3(STRING("FamElem3"),CHIL,INDI) }
	|	MARR ParamList { TUPLE3(STRING("FamElem4"),MARR,$2) }
	|	DIV cont { TUPLE3(STRING("FamElem5"),DIV,$2) }
	|	ADOP INDI { TUPLE3(STRING("FamElem6"),ADOP,INDI) }

BODY: lev0_lst { TLIST (List.rev $1) }

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
  | UNDERSCORE_KW  { UNDERSCORE_KW $1 }
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
  | SOUR APPROVED_SYSTEM_ID lev2_lst { SOUR }
  | SUBM AT XREF AT { SUBM }
  | DEST RECEIVING_SYSTEM_NAME { DEST }
  | DATE lev2_lst { TUPLE2(DATE $1, TLIST (List.rev $2)) }
  | RINS RINS_LST { RINS }
  | RIN STRING COLON STRING { RIN }
  | NAME lev2_lst { TUPLE2(NAME $1, TLIST (List.rev $2)) }
  | EMAIL { EMAIL $1 }
  | UNDERSCORE_KW { UNDERSCORE_KW $1 }
  | SEX STRING { SEX }
  | BIRT lev2_lst { TUPLE2(BIRT, TLIST (List.rev $2)) }
  | RESI lev2_lst { TUPLE2(RESI, TLIST (List.rev $2)) }
  | FAMC AT XREF AT { FAMC }
  | DEAT lev2_lst { TUPLE2(DEAT, TLIST (List.rev $2)) }
  | BURI lev2_lst { TUPLE2(BURI, TLIST (List.rev $2)) }
  | CENS lev2_lst { TUPLE2(CENS, TLIST (List.rev $2)) }
  | OCCU lev2_lst { TUPLE2(OCCU $1, TLIST (List.rev $2)) }
  | FAMS AT XREF AT { FAMS }
  | SOUR AT XREF AT lev2_lst { TUPLE2(SOUR, TLIST (List.rev $5)) }
  | EVEN lev2_lst { TUPLE2(EVEN, TLIST (List.rev $2)) }
  | CHR lev2_lst { TUPLE2(CHR, TLIST (List.rev $2)) }
  | NOTE lev2_con { NOTE $1 }
  | BAPM lev2_lst { TUPLE2(BAPM, TLIST (List.rev $2)) }
  | RELI lev2_lst { TUPLE2(RELI $1, TLIST (List.rev $2)) }
  | HUSB AT XREF AT { HUSB }
  | WIFE AT XREF AT { WIFE }
  | CHIL AT XREF AT { CHIL }
  | MARR lev2_lst { TUPLE2(MARR, TLIST (List.rev $2)) }
  | TITL { TITL $1 }
  | PUBL { PUBL $1 }
  | TEXT lev2_con { TEXT $1 }
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

levn:
 COLON SUBM
 num3 ADDRESS_STRUCTURE
num2 DATA NAME_OF_SOURCE_DATA
num3 DATE PUBLICATION_DATE
num3 COPR COPYRIGHT_SOURCE_DATA
num4 con COPYRIGHT_SOURCE_DATA
num1 SUBN AT XREF COLON SUBN AT 
num1 FILE FILE_NAME
num1 COPR COPYRIGHT_GEDCOM_FILE
num2 VERS VERSION_NUMBER
num1 PLAC
num2 FORM PLACE_HIERARCHY
num1 NOTE GEDCOM_CONTENT_DESCRIPTION
num2 con GEDCOM_CONTENT_DESCRIPTION { (NUM $1) }

NAME_OF_SOURCE_DATA: STRING { STRING $1 }

COPYRIGHT_GEDCOM_FILE: STRING { STRING $1 }

NAME_OF_BUSINESS: STRING { STRING $1 }

ADDRESS_STRUCTURE: STRING { STRING $1 }

PLACE_HIERARCHY: STRING { STRING $1 }

PUBLICATION_DATE: STRING { STRING $1 }

LANGUAGE_OF_TEXT: STRING { STRING $1 }

RECEIVING_SYSTEM_NAME: STRING { STRING $1 }

GEDCOM_FORM: STRING { STRING $1 }

COPYRIGHT_SOURCE_DATA: STRING { STRING $1 }

TRANSMISSION_DATE: STRING { STRING $1 }

CHARACTER_SET: STRING { STRING $1 }

TIME_VALUE: STRING { STRING $1 }

FILE_NAME: STRING { STRING $1 }

NAME_OF_PRODUCT: STRING { STRING $1 }

GEDCOM_CONTENT_DESCRIPTION: STRING { STRING $1 }

APPROVED_SYSTEM_ID: STRING { STRING $1 }

XREF: STRING { STRING $1 }

num1: NUM { NUM $1 }

num2: NUM { NUM $1 }

num3: NUM { NUM $1 }

num4: NUM { NUM $1 }

con:
  | CONT { CONT }
  | CONC { CONC }
