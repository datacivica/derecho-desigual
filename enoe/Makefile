#
# Authors:     SW
# Maintainers: SW, AF
# Copyright:   2023, Data Cívica, GPL v2 or newer
# ===============================================
# abogadas-mx/enoe/Makefile

.PHONY: all \
	import-enoe \
	join-enoe \
	clean-enoe

all: clean-enoe

# Last step to first step 
clean-enoe: join-enoe
	cd $@ && make

join-enoe: import-enoe
	cd $@ && make

import-enoe: 
	cd $@ && make

# done.
