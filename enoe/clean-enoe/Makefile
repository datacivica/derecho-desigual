#
# Authors:     SW
# Maintainers: SW, AF
# Copyright:   2023, Data Cívica, GPL v2 or newer
# ===============================================
# abogadas-mx/enoe/clean-enoe/Makefile

input := input/enoe_joined.txt

output := output/enoe_cleaned.txt

.PHONY: all clean

all: $(output)

clean:
	-rm -r output/*

$(output): src/clean-enoe.R $(input)
	-mkdir output
	Rscript --vanilla $<

# done.
