#!/bin/bash

# Download new data
# CONFIRMAR RUTAS EN NUEVO SERVIDOR
/usr/local/bin/Rscript ~/git/abogadas-mx/enoe/import-enoe/src/download-enoe.R

# Run ENOE Makefile
cd ~/git/abogadas-mx/enoe && make clean && make && echo "Directorio enoe actualizado"

# Run prep-app Makefile
cd ~/git/abogadas-mx/prep-app && make clean && make && echo "Directorio prep-app actualizado"
