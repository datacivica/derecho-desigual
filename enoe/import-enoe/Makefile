#
# Authors:     SW
# Maintainers: SW, AF
# Copyright:   2023, Data Cívica, GPL v2 or newer
# ===============================================
# abogadas-mx/enoe/import-enoe/Makefile

output := output/coe2t123_filtered.txt
# Listed just one filtered output because all made in same script

hand_trim := hand/trims-ampliado.yaml
hand_vars := hand/vars-to-keep.yaml

.PHONY: all clean

all: $(output)

clean:
	-rm -r output/*

$(output): src/filter-tables.R $(hand_vars)
	-mkdir output
	Rscript --vanilla $<

# done.
