# Cargamos todos los paquetes
pacman::p_load(shiny, stringr, ggplot2, dplyr, here, data.table, scales,
	       ggrepel, bslib, googledrive)

# Corremos todos los módulos
source(here("app/modules/src/mod_perfiles.R"))
source(here("app/modules/src/mod_genero.R"))
source(here("app/modules/src/mod_abogadas.R"))
source(here("app/modules/src/mod_cuidados.R"))
source(here("app/modules/src/mod_violencia.R"))

# Corremos el ui y el server
runApp(here("app"))
