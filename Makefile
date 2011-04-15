# This is the main Makefile that is shipped as part of the source package.
# Keep in mind that the hierarchy that is shipped is not identical to the
# hierarchy within the svn repository: some sub-directories are not shipped;
# the documentation is pre-built.

# The hierarchy that is shipped includes:
#   demos
#   menhir.1
#   manual.pdf
#   src
#   Makefile (this one)

# ----------------------------------------------------------------------------

# By default, we attempt to use ocamlfind (if present in the PATH), but it it
# is possible to prevent that externally by setting USE_OCAMLFIND to false.

ifndef USE_OCAMLFIND
  USE_OCAMLFIND = ocamlfind ocamlc -v >/dev/null 2>&1
endif

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
MLYLIB          := src/standard.mly

# -------------------------------------------------------------------------

# Building menhirLib.

ifeq ($(TARGET),byte)
MENHIRLIB       := menhirLib.cmi menhirLib.cmo
else
MENHIRLIB       := menhirLib.cmi menhirLib.cmo menhirLib.cmx menhirLib.o
endif

# ----------------------------------------------------------------------------
# Compilation.

.PHONY: all install uninstall

all: src/menhir

src/menhir: src/installation.ml
	$(MAKE) -C src -f Makefile
	$(MAKE) -C src -f Makefile $(MENHIRLIB)

# Record some installation time settings within the menhir binary.

src/installation.ml:
	echo "let libdir = \"${libdir}\"" > $@
	if $(USE_OCAMLFIND) ; then \
	  echo "let ocamlfind = true" >> $@ ; \
	else \
	  echo "let ocamlfind = false" >> $@ ; \
	fi

# ----------------------------------------------------------------------------
# Installation.

install: src/menhir
	mkdir -p $(bindir)
	mkdir -p $(libdir)
	mkdir -p $(docdir)
	mkdir -p $(mandir)
	install src/menhir $(bindir)
	install -m 644 $(MLYLIB) $(libdir)
	cp -r $(DOCS) $(docdir)
	cp -r $(MANS) $(mandir)
	@cd src && if $(USE_OCAMLFIND) ; then \
	  echo Installing MenhirLib via ocamlfind. ; \
	  ocamlfind install menhirLib META $(MENHIRLIB) ; \
	else \
	  echo Installing MenhirLib manually. ; \
	  install -m 644 $(MENHIRLIB) $(libdir) ; \
	fi

uninstall:
	rm -rf $(bindir)/menhir
	rm -rf $(libdir)
	rm -rf $(docdir)
	rm -rf $(mandir)/$(MANS)
	@if $(USE_OCAMLFIND) ; then \
	  echo Un-installing MenhirLib via ocamlfind. ; \
	  ocamlfind remove menhirLib ; \
	fi

