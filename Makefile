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
# Installation paths.

# TEMPORARY GODIVA and Linux do not agree on the standard paths...

ifndef PREFIX
  $(error Please define PREFIX)
endif

bindir          := ${PREFIX}/bin
docdir		:= ${PREFIX}/share/doc/menhir
libdir	        := ${PREFIX}/share/menhir
mandir          := ${PREFIX}/share/man/man1
MANS            := menhir.1
DOCS            := manual.pdf demos
LIBS            := standard.mly

# ----------------------------------------------------------------------------
# Common compilation rules.

.PHONY: all install uninstall

COLD            += all install uninstall

all: menhir

-include Makefile.common

# ----------------------------------------------------------------------------
# Recording the standard library path that was chosen at installation
# time.

stdlib.ml:
	echo "let path = \"${PREFIX}/share/menhir\"" > stdlib.ml

# ----------------------------------------------------------------------------
# Installation.

install:
	mkdir -p $(bindir)
	mkdir -p $(libdir)
	mkdir -p $(docdir)
	mkdir -p $(mandir)
	install menhir $(bindir)
	install -m 644 $(LIBS) $(libdir)
	cp -r $(DOCS) $(docdir)
	cp -r $(MANS) $(mandir)

uninstall:
	rm -rf $(bindir)/menhir
	rm -rf $(libdir)
	rm -rf $(docdir)
	rm -rf $(mandir)/$(MANS)

