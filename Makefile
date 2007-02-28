##########################################################################
#                                                                        #
#  Menhir                                                                #
#                                                                        #
#  François Pottier and Yann Régis-Gianas, INRIA Rocquencourt            #
#                                                                        #
#  Copyright 2005 Institut National de Recherche en Informatique et      #
#  en Automatique. All rights reserved. This file is distributed         #
#  under the terms of the Q Public License version 1.0, with the         #
#  change described in file LICENSE.                                     #
#                                                                        #
##########################################################################

# ----------------------------------------------------------------------------
# Locating the ocaml compilers.
# If ocamlfind is available, then it is used for that purpose.

CAMLC           := $(shell if ocamlfind ocamlc -v >/dev/null 2>&1 ; \
                       then echo ocamlfind ocamlc ; \
		       elif ocamlc.opt -v >/dev/null 2>&1 ; \
                       then echo ocamlc.opt ; \
		       else echo ocamlc ; fi)

CAMLOPT         := $(shell if ocamlfind ocamlopt -v >/dev/null 2>&1 ; \
                       then echo ocamlfind ocamlopt ; \
		       elif ocamlopt.opt -v >/dev/null 2>&1 ; \
                       then echo ocamlopt.opt ; \
		       else echo ocamlopt ; fi)

CAMLDEP         := $(shell if ocamlfind ocamldep -version >/dev/null 2>&1 ; \
                       then echo ocamlfind ocamldep ; \
		       elif ocamldep.opt -version >/dev/null 2>&1 ; \
                       then echo ocamldep.opt ; \
		       else echo ocamldep ; fi)

CAMLLEX         := ocamllex

CAMLYACC        := ocamlyacc -v

# ----------------------------------------------------------------------------
# Installation paths.

ifneq ($(findstring install,$(MAKECMDGOALS)),)
  ifndef PREFIX
    $(error Please define PREFIX.)
  endif
endif

# TEMPORARY GODIVA and Linux do not agree on the standard paths...

EXECUTABLE      := menhir
bindir          := ${PREFIX}/bin
docdir		:= ${PREFIX}/share/doc/$(EXECUTABLE)
libdir	        := ${PREFIX}/share/$(EXECUTABLE)
mandir          := ${PREFIX}/share/man/man1
MANS            := menhir.1
DOCS            := manual.pdf demos
LIBS            := standard.mly

# ----------------------------------------------------------------------------
# Common compilation rules.

.PHONY: all install uninstall versioncheck

COLD            += all install uninstall versioncheck

all: versioncheck bootstrap

-include Makefile.common

# ----------------------------------------------------------------------------
# Recording the standard library path that was chosen at installation
# time. Do not rely on $(libdir) or $(EXECUTABLE) here, because
# $(EXECUTABLE) is set to a different value than the one shown above
# during bootstrap stage one.

stdlib.ml:
	echo "let path = \"${PREFIX}/share/menhir\"" > stdlib.ml

# ----------------------------------------------------------------------------
# Installation.

install:
	mkdir -p $(bindir)
	mkdir -p $(libdir)
	mkdir -p $(docdir)
	mkdir -p $(mandir)
	install $(EXECUTABLE) $(bindir)
	install -m 644 $(LIBS) $(libdir)
	cp -r $(DOCS) $(docdir)
	cp -r $(MANS) $(mandir)

uninstall:
	rm -rf $(bindir)/$(EXECUTABLE)
	rm -rf $(libdir)
	rm -rf $(docdir)
	rm -rf $(mandir)

# ----------------------------------------------------------------------------
# Checking the version of the ocaml compiler.

versioncheck:
	@ echo Checking that $(CAMLOPT) is recent enough...
	@ $(CAMLOPT) str.cmxa check-ocaml-version.ml -o check-ocaml-version
	@ ./check-ocaml-version --verbose --gt "3.09"

