#
# Authors:     SW
# Maintainers: SW, AF
# Copyright:   2023, Data Cívica, GPL v2 or newer
# ===============================================
# abogadas-mx/endireh/Makefile

.PHONY: all \
	import-endireh \
	join-endireh \
	clean-endireh

all: clean-endireh

# Last step to first step 
clean-endireh: join-endireh
	cd $@ && make

join-endireh: import-endireh
	cd $@ && make

import-endireh: 
	cd $@ && make

# done.
