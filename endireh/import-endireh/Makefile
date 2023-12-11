#
# Authors:     SW
# Maintainers: SW, AF
# Copyright:   2023, Data Cívica, GPL v2 or newer
# ===============================================
# abogadas-mx/import-endireh/Makefile

output := output/escolar_2021.txt \
	  output/laboral_2021.txt \
	  output/sdem_2021.txt \
	  output/ingresos_2021.txt \
	  output/escolar_2016.txt \
	  output/laboral_2016.txt \
	  output/sdem_2016.txt \
	  output/ingresos_2016.txt

hand_vars := hand/vars-to-keep-2021.yaml \
	     hand/vars-to-keep-2016.yaml

.PHONY: all clean

all: $(output)

clean:
	-rm -r output/*

$(output): src/filter-tables.R $(hand_vars)
	-mkdir output
	Rscript --vanilla $<

# done.
