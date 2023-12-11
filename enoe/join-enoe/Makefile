#
# Authors:     SW
# Maintainers: SW, AF
# Copyright:   2023, Data Cívica, GPL v2 or newer
# ===============================================
# abogadas-mx/enoe/join-enoe/Makefile

input := $(wildcard input/*) 

output := output/enoe_joined.txt

.PHONY: all clean

all: $(output)

clean:
	-rm -r output/*

$(output): src/join-enoe.R $(input)
	-mkdir output
	Rscript --vanilla $<

# done.
