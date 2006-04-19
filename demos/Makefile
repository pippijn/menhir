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

DEMOS := calc calc-two

.PHONY: all clean

all:
	@for i in $(DEMOS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) ; \
	done

clean:
	/bin/rm -f *~ .*~
	@for i in $(DEMOS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) clean ; \
	done
