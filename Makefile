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

CAMLC           := $(shell if which ocamlfind &>/dev/null ; \
                       then echo ocamlfind ocamlc ; \
		       elif which ocamlc.opt &>/dev/null ; \
                       then echo ocamlc.opt ; \
		       else echo ocamlc ; fi)

CAMLOPT         := $(shell if which ocamlfind &>/dev/null ; \
                       then echo ocamlfind ocamlopt ; \
		       elif which ocamlopt.opt &>/dev/null ; \
                       then echo ocamlopt.opt ; \
		       else echo ocamlopt ; fi)

CAMLDEP         := $(shell if which ocamlfind &>/dev/null ; \
                       then echo ocamlfind ocamldep ; \
		       elif which ocamldep.opt &>/dev/null ; \
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

EXECUTABLE      := menhir
bindir          := ${PREFIX}/bin
docdir		:= ${PREFIX}/doc/$(EXECUTABLE)
libdir	        := ${PREFIX}/share/$(EXECUTABLE)
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
	install $(EXECUTABLE) $(bindir)
	install -m 644 $(LIBS) $(libdir)
	cp -r $(DOCS) $(docdir)

uninstall:
	rm -rf $(bindir)/$(EXECUTABLE)
	rm -rf $(libdir)
	rm -rf $(docdir)

# ----------------------------------------------------------------------------
# Checking the version of the ocaml compiler.

versioncheck:
	@ echo Checking that $(CAMLOPT) is recent enough...
	@ $(CAMLOPT) str.cmxa check-ocaml-version.ml -o check-ocaml-version
	@ ./check-ocaml-version --verbose --gt "3.09"

