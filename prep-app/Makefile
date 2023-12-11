#
# Authors:     SW
# Maintainers: SW, AF
# Copyright:   2023, Data Cívica, GPL v2 or newer
# ===============================================
# abogadas-mx/prep-app/Makefile

input := input/enoe_cleaned.txt \
	 input/endireh_cleaned.txt \
	 input/censo_cleaned.txt

# List one output from each script/module
output_all := output/perfiles.csv \
	      output/paridad_genero_sector.csv \
	      output/cuidados_resumen.csv \
	      output/violencia_abogadas.csv \
	      output/univ_dist.csv

.PHONY: all clean

all: $(output_all)

clean:
	-rm -r output/*

output/perfiles.csv: src/mod_perfiles_prep.R $(input)
	-mkdir output
	Rscript --vanilla $<

output/paridad_genero_sector.csv: src/mod_genero_prep.R $(input)
	-mkdir output
	Rscript --vanilla $<

output/cuidados_resumen.csv: src/mod_cuidados_prep.R $(input)
	-mkdir output
	Rscript --vanilla $<

output/violencia_abogadas.csv: src/mod_violencia_prep.R $(input)
	-mkdir output
	Rscript --vanilla $<

output/univ_dist.csv: src/mod_abogadas_prep.R $(input)
	-mkdir output
	Rscript --vanilla $<

# done.
