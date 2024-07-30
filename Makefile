#**************************************************************************)
#*                                                                        *)
#* OCaml template Copyright (C) 2004-2010                                 *)
#*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
#* Adapted to boolean logic by Jonathan Kimmitt                           *)
#*  Copyright 2016 University of Cambridge                                *)
#*                                                                        *)
#*  This software is free software; you can redistribute it and/or        *)
#*  modify it under the terms of the GNU Library General Public           *)
#*  License version 2.1, with the special exception on linking            *)
#*  described in file LICENSE.                                            *)
#*                                                                        *)
#*  This software is distributed in the hope that it will be useful,      *)
#*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
#*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
#*                                                                        *)
#**************************************************************************)

.PHONY: everything convert

PARSER=ocamlyacc
MENHIRFLAGS= --infer # --trace
MENHIRFLAGST= --infer --trace
PARSER=menhir $(MENHIRFLAGS)

###########################################################################

Gedcom_top: Gedcom.cmo Gedcom_tokens.cmo Gedcom_rewrite_types.mli Gedcom_lex.ml Gedcom_dump.ml Gedcom_rewrite.ml Gedcom_main.ml
	ocamlmktop -g -o $@ Gedcom_tokens.cmo Gedcom_rewrite_types.mli Gedcom_types.ml Gedcom.cmo Gedcom_lex.ml Gedcom_dump.ml Gedcom_rewrite.ml Gedcom_main.ml

Gedcom.cmo: Gedcom_tokens.ml Gedcom.ml Gedcom.mli
	ocamlc.opt -g -c Gedcom.mli Gedcom_tokens.ml Gedcom_types.ml Gedcom.ml

Gedcom.cmx: Gedcom_tokens.ml Gedcom.ml Gedcom.mli
	ocamlopt.opt -g -c Gedcom.mli Gedcom_tokens.ml Gedcom.ml

Gedcom_lex.ml: Gedcom_lex.mll
	ocamllex $<

Gedcom.ml Gedcom.mli: Gedcom.mly
	ocamlc -c Gedcom_types.ml
	menhir $(MENHIRFLAGS) $<

############################################################################
