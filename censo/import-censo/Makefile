#
# Authors:     SW
# Maintainers: SW, AF
# Copyright:   2023, Data Cívica, GPL v2 or newer
# ===============================================
# abogadas-mx/censo/import-censo/Makefile

output := output/censo_filtered.txt

hand_vars := hand/vars-to-keep.yaml

.PHONY: all clean

all: $(output)

clean:
	-rm -r output/*

$(output): src/filter-censo.R $(hand_vars)
	-mkdir output
	Rscript --vanilla $<

# done.
