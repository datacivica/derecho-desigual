#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/endireh/import-endireh/src/filter-tables.R 

# TODO

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, R.utils, janitor, data.table, foreign, here, yaml,
               googledrive, digest)

paths <- list(
  output = here("endireh/import-endireh/output/"),
  vars_2021 = here("endireh/import-endireh/hand/vars-to-keep-2021.yaml"),
  vars_2016 = here("endireh/import-endireh/hand/vars-to-keep-2016.yaml"))

# Define Drive folders
drive_in <- drive_ls(as_id("1Y8bsb5J7t8qn-MLejbdr0Pkez20-hVKH"))

# Create list of files to read
file_ids <- setNames(drive_in$id, drive_in$name)

# Read raw files from Drive
for (file_name in names(file_ids)) {
  file_id <- file_ids[file_name]
  
  drive_download(as_id(file_id), path = paste0(tempdir(), "/", file_name), overwrite = TRUE)
  
  tempo <- fread(paste0(tempdir(), "/", file_name))
  
  assign(str_replace(file_name, ".csv", "_raw"), tempo)
  
  file.remove(paste0(tempdir(), "/", file_name))
  
  message(paste0(file_name, " read."))
}

# 2021 ====
# Define vars to keep
vars_2021 <- read_yaml(paths$vars_2021)

all_vars_2021 <- vars_2021$all

sdem_vars_2021 <- c(all_vars_2021, vars_2021$sdem)

iv_vars_2021 <- c(all_vars_2021, vars_2021$tb_sec_iv)

vii_vars_2021_exact <- c(all_vars_2021, vars_2021$tb_sec_vii_exact)

vii_vars_2021_regex <- vars_2021$tb_sec_vii_regex

viii_vars_2021_exact <- c(all_vars_2021, vars_2021$tb_sec_viii_exact)

viii_vars_2021_regex <- vars_2021$tb_sec_viii_regex

x_vars_2021 <- c(all_vars_2021, vars_2021$tb_sec_x)

xix_vars_2021 <- vars_2021$tb_sec_xix_regex

# Sociodemográfico
sdem_2021 <- tsdem_2021_raw %>% 
  clean_names %>% 
  select(any_of(sdem_vars_2021)) %>% 
  mutate(year = 2021)

stopifnot(nrow(sdem_2021) * ncol(sdem_2021) > 0)

write_csv(sdem_2021, paste0(paths$output, "/sdem_2021.csv"))

message("SDem 2021 processed")

# Ingresos (iv)
ingresos_2021 <- tb_sec_iv_2021_raw %>% 
  clean_names %>% 
  select(any_of(iv_vars_2021)) %>% 
  mutate(year = 2021)

stopifnot(nrow(ingresos_2021) * ncol(ingresos_2021) > 0)

write_csv(ingresos_2021, paste0(paths$output, "/ingresos_2021.csv"))

message("Ingresos 2021 processed.")

# Ámbito escolar (vii)
escolar_2021 <- tb_sec_vii_2021_raw %>% 
  clean_names %>% 
  select(any_of(vii_vars_2021_exact), starts_with(vii_vars_2021_regex)) %>% 
  mutate(year = 2021)

stopifnot(nrow(escolar_2021) * ncol(escolar_2021) > 0)

write_csv(escolar_2021, paste0(paths$output, "/escolar_2021.csv"))

message("Escolar 2021 processed")

# Ámbito laboral (viii)
laboral_2021 <- tb_sec_viii_2021_raw %>%  
  clean_names %>% 
  select(any_of(viii_vars_2021_exact), starts_with(viii_vars_2021_regex)) %>% 
  mutate(year = 2021)

stopifnot(nrow(laboral_2021) * ncol(laboral_2021) > 0)

write_csv(laboral_2021, paste0(paths$output, "/laboral_2021.csv"))

message("Laboral 2021 processed.")

# Atención obstétrica (x)
obstetrica_2021 <- tb_sec_x_2021_raw %>%  
  clean_names %>% 
  select(any_of(x_vars_2021)) %>% 
  mutate(year = 2021)

write_csv(obstetrica_2021, paste0(paths$output, "/obstetrica_2021.csv"))

message("Obstétrica 2021 processed.")

# Discapacidad (xix)
discapacidad_2021 <- tb_sec_xix_2021_raw %>%  
  clean_names %>% 
  select(any_of(all_vars_2021), starts_with(xix_vars_2021)) %>% 
  mutate(year = 2021)

write_csv(discapacidad_2021, paste0(paths$output, "/discapacidad_2021.csv"))

message("Discapacidad 2021 processed.")

# 2016 ====
# Define vars to keep
vars_2016 <- read_yaml(paths$vars_2016)

all_vars_2016 <- vars_2016$all

sdem_vars_2016 <- c(all_vars_2016, vars_2016$sdem)

iv_vars_2016 <- c(all_vars_2016, vars_2016$tb_sec_iv)

vi_vars_2016_exact <- c(all_vars_2016, vars_2016$tb_sec_vi_exact)

vi_vars_2016_regex <- vars_2016$tb_sec_vi_regex

vii_vars_2016_exact <- c(all_vars_2016, vars_2016$tb_sec_vii_exact)

vii_vars_2016_regex <- vars_2016$tb_sec_vii_regex

vii_2_vars_2016_regex <- vars_2016$tb_sec_vii_2_regex

ix_vars_2016 <- c(all_vars_2016, vars_2016$tb_sec_ix)

# Sociodemográfico
sdem_2016 <- tsdem_2016_raw %>% 
  clean_names %>% 
  select(any_of(sdem_vars_2016)) %>% 
  mutate(year = 2016)

stopifnot(nrow(sdem_2016) * ncol(sdem_2016) > 0)

write_csv(sdem_2016, paste0(paths$output, "/sdem_2016.csv"))

message("SDem 2016 done.")

# Ingresos (iv)
ingresos_2016 <- tb_sec_iv_2016_raw %>% 
  clean_names %>% 
  select(any_of(iv_vars_2016)) %>% 
  mutate(year = 2016)

stopifnot(nrow(ingresos_2016) * ncol(ingresos_2016) > 0)

write_csv(ingresos_2016, paste0(paths$output, "/ingresos_2016.csv"))

message("Ingresos 2016 done.")

# Ámbito escolar (vi)
escolar_2016 <- tb_sec_vi_2016_raw %>% 
  clean_names %>% 
  select(any_of(vi_vars_2016_exact), starts_with(vi_vars_2016_regex)) %>% 
  mutate(year = 2016)

stopifnot(nrow(escolar_2016) * ncol(escolar_2016) > 0)

write_csv(escolar_2016, paste0(paths$output, "/escolar_2016.csv"))

message("Escolar 2016 done.")

# Ámbito laboral (vii)
laboral_a <- tb_sec_vii_2016_raw %>% 
  clean_names %>% 
  select(any_of(vii_vars_2016_exact), starts_with(vii_vars_2016_regex)) %>% 
  mutate(year = 2016)

laboral_b <- tb_sec_vii_2_2016_raw %>%  
  clean_names %>% 
  select(any_of(all_vars_2016), starts_with(vii_2_vars_2016_regex))

stopifnot(nrow(laboral_a) * ncol(laboral_a) > 0,
          nrow(laboral_b) * ncol(laboral_b) > 0)

laboral_2016 <- laboral_a %>% 
  left_join(laboral_b)

write_csv(laboral_2016, paste0(paths$output, "/laboral_2016.csv"))

message("Laboral 2016 done.")

# Atención obstétrica (ix)
obstetrica_2016 <- tb_sec_ix_2016_raw %>%
  clean_names %>% 
  select(any_of(ix_vars_2016)) %>% 
  mutate(year = 2016)

# stopifnot("p9_4_1" %in% names(obstetrica_2016))

write_csv(obstetrica_2016, paste0(paths$output, "/obstetrica_2016.csv"))

message("Obstétrica 2016 done.")

# Write hashes ====
message("Writing hashes...")

file_bases <- c("obstetrica", "laboral", "escolar", "ingresos", "sdem")

files_to_hash <- c(paste0(file_bases, "_2021"),
                   paste0(file_bases, "_2016"),
                   "discapacidad_2021")

for (file in files_to_hash){
  hash <- digest(get(file), algo = "sha256")
  
  writeLines(hash, paste0(paths$output, file, ".txt"))
}

# done.
