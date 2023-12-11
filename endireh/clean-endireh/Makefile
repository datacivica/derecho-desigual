#
# Authors:     SW
# Maintainers: SW, AF
# Copyright:   2023, Data Cívica, GPL v2 or newer
# ===============================================
# abogadas-mx/endireh/clean-endireh/Makefile

input := input/endireh_joined_2021.txt \
	 input/endireh_joined_2016.txt

output := output/endireh_cleaned.txt

.PHONY: all clean

all: $(output)

clean:
	-rm -r output/*

$(output): src/clean-endireh.R $(input)
	-mkdir output
	Rscript --vanilla $<

# done.
