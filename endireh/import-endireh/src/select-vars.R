#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/endireh/import-endireh/src/select-vars.R 

# TODO
# Import script where year is defined by name of most recent downloaded ENDIREH file

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, R.utils, janitor, data.table, foreign, here, yaml)

paths <- list(input = here("endireh/import-endireh/input"),
              output = here("endireh/import-endireh/output"),
              vars_to_keep = here("endireh/import-endireh/hand/vars-to-keep.yaml"))

# Define vars to keep
vars_to_keep <- read_yaml(paths$vars_to_keep)

all_vars_to_keep <- vars_to_keep$all

sdem_vars_to_keep <- c(all_vars_to_keep, vars_to_keep$sdem)

iv_vars_to_keep <- c(all_vars_to_keep, vars_to_keep$tb_sec_iv)

vii_vars_to_keep_exact <- c(all_vars_to_keep, vars_to_keep$tb_sec_vii_exact)

vii_vars_to_keep_regex <- vars_to_keep$tb_sec_vii_regex

viii_vars_to_keep_exact <- c(all_vars_to_keep, vars_to_keep$tb_sec_viii_exact)

viii_vars_to_keep_regex <- vars_to_keep$tb_sec_viii_regex

# Define year # UPDATE TO MAKE AUTOMATIC
year_endireh <- 2021

# Sociodemográfico
sdem_raw <- fread(paste0(paths$input, "/tsdem.csv"))

sdem_out <- sdem_raw %>% 
  clean_names %>% 
  select(any_of(sdem_vars_to_keep)) %>% 
  mutate(year = year_endireh)

write_csv(sdem_out, paste0(paths$output, "/sdem.csv"))

message("SDem done.")

# Ingresos (IV)
iv_raw <- fread(paste0(paths$input, "/tb_sec_iv.csv"))

iv_out <- iv_raw %>% 
  clean_names %>% 
  select(any_of(iv_vars_to_keep)) %>% 
  mutate(year = year_endireh)

write_csv(iv_out, paste0(paths$output, "/ingresos.csv"))

message("Ingresos done.")

# Ámbito escolar (vii)
vii_raw <- fread(paste0(paths$input, "/tb_sec_vii.csv"))

vii_out <- vii_raw %>% 
  clean_names %>% 
  select(any_of(vii_vars_to_keep_exact), starts_with(vii_vars_to_keep_regex)) %>% 
  mutate(year = year_endireh)

write_csv(vii_out, paste0(paths$output, "/escolar.csv"))

message("Escolar done.")

# Ámbito laboral (viii)
viii_raw <- fread(paste0(paths$input, "/tb_sec_viii.csv"))

viii_out <- viii_raw %>% 
  clean_names %>% 
  select(any_of(viii_vars_to_keep_exact), starts_with(viii_vars_to_keep_regex)) %>% 
  mutate(year = year_endireh)

write_csv(viii_out, paste0(paths$output, "/laboral.csv"))

message("Laboral done.")

# done.
