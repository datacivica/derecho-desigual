#
# Authors:     SW
# Maintainers: SW, AF
# Copyright:   2023, Data Cívica, GPL v2 or newer
# ===============================================
# abogadas-mx/censo/Makefile

.PHONY: all \
	import-censo \
	clean-censo

all: clean-censo

# Last step to first step 
clean-censo: import-censo
	cd $@ && make

import-censo: 
	cd $@ && make

# done.
