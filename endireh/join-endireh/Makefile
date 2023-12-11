#
# Authors:     SW
# Maintainers: SW, AF
# Copyright:   2023, Data Cívica, GPL v2 or newer
# ===============================================
# abogadas-mx/endireh/join-endireh/Makefile

input := $(wildcard input/*) 

output := output/endireh_joined_2021.txt \
	  output/endireh_joined_2016.txt

.PHONY: all clean

all: $(output)

clean:
	-rm -r output/*

$(output): src/join-endireh.R $(input)
	-mkdir output
	Rscript --vanilla $<

# done.

