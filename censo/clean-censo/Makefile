#
# Authors:     SW
# Maintainers: SW, AF
# Copyright:   2023, Data Cívica, GPL v2 or newer
# ===============================================
# abogadas-mx/censo/clean-enoe/Makefile

input := input/censo_filtered.txt

output := output/censo_cleaned.txt

.PHONY: all clean

all: $(output)

clean:
	-rm -r output/*

$(output): src/clean-censo.R $(input)
	-mkdir output
	Rscript --vanilla $<

# done.
