#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/enoe/import-enoe/src/filter-tables.R 

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, R.utils, janitor, data.table, here, yaml, googledrive, digest)

paths <- list(vars_to_keep = here("enoe/import-enoe/hand/vars-to-keep.yaml"),
              trims_to_keep = here("enoe/import-enoe/hand/trims-ampliado.yaml"),
              output = here("enoe/import-enoe/output/"))

# Define Drive directories
drive_in <- drive_ls(as_id("1CfL43fMp7D_xwa4PThsysk3aNfbTFpFQ"))

# Define periods to keep (ampliado, last 5 years)
two_digit_year <- as.integer(substr(format(Sys.Date(), "%Y"), 3, 4))
five_years_ago <- as.integer(two_digit_year) - 5

trims_to_keep <- paste0("t1", five_years_ago:two_digit_year)

# Define vars to keep
vars_to_keep <- read_yaml(paths$vars_to_keep)

all_vars_to_keep <- vars_to_keep$all

hogar_vars_to_keep <- c(all_vars_to_keep, vars_to_keep$hogar)

sdem_vars_to_keep <- c(all_vars_to_keep, vars_to_keep$sdem)

coe1_vars_to_keep <- c(all_vars_to_keep, vars_to_keep$coe1t)

coe2_vars_to_keep <- c(all_vars_to_keep, vars_to_keep$coe2t)

# Read raw data
drive_names <- drive_in %>%
  filter(grepl(paste(trims_to_keep, collapse = "|"), name)) %>% 
  pull(name)

stopifnot(length(drive_names) == length(trims_to_keep) * 4)

# Hogar
hogar_files <- drive_names[grep("^hog", drive_names)]

import_hogar <- function(x){
  hogar <- drive_read_string(x) %>% 
    textConnection("r") %>% 
    read.csv() %>%
    clean_names() %>% 
    select(any_of(hogar_vars_to_keep)) %>% 
    mutate(year = paste0("20", str_sub(x, 6, 7))) %>% 
    write_csv(paste0(paths$output, str_replace(x, ".csv", "_filtered.csv")))
  
  hash <- digest(hogar, algo = "sha256")
  
  writeLines(hash, paste0(paths$output, str_replace(x, ".csv", "_filtered.txt")))
}

walk(hogar_files, import_hogar)

message("Hogar done.")

# Sociodemográficos

# Por alguna razón, para los archivos sdem, drive_read_string me devuelve NA
# Entonces aquí descargo los archivos en lugar de leerlos como string

sdem_files <- drive_names[grep("^sdem", drive_names)]

import_sdem <- function(x){
  drive_download(x, path = paste0(tempdir(), "/", x))
  
  sdem <- read_csv(paste0(tempdir(), "/", x)) %>%
    clean_names() %>% 
    select(any_of(sdem_vars_to_keep)) %>% 
    mutate(year = paste0("20", str_sub(x, 7, 8))) %>% 
    write_csv(paste0(paths$output,
                     str_replace(str_remove(x, "_test_"), ".csv", "_filtered.csv")))
  
  file.remove(paste0(tempdir(), "/", x))
  
  hash <- digest(sdem, algo = "sha256")
  
  writeLines(hash, paste0(paths$output, str_replace(x, ".csv", "_filtered.txt")))

}

# Suppressed warnings because a few typos in open text fields create warnings when read
suppressWarnings(walk(sdem_files, import_sdem))

message("SDem done.")

# COE1
coe1_files <- drive_names[grep("^coe1", drive_names)]

import_coe1 <- function(x){
  coe1 <- drive_read_string(x) %>% 
    textConnection("r") %>% 
    read.csv() %>%
    clean_names() %>% 
    select(any_of(coe1_vars_to_keep)) %>% 
    mutate(year = paste0("20", str_sub(x, 7, 8))) %>% 
    write_csv(paste0(paths$output,
                     str_replace(str_remove(x, "_test_"), ".csv", "_filtered.csv")))
  
  hash <- digest(coe1, algo = "sha256")
  
  writeLines(hash, paste0(paths$output, str_replace(x, ".csv", "_filtered.txt")))
}

walk(coe1_files, import_coe1)

message("COE1 done.")

# COE2
coe2_files <- drive_names[grep("^coe2", drive_names)]

import_coe2 <- function(x){
  coe2 <- drive_read_string(x) %>% 
    textConnection("r") %>% 
    read.csv() %>% 
    clean_names() %>% 
    select(any_of(coe2_vars_to_keep)) %>% 
    mutate(year = paste0("20", str_sub(x, 7, 8))) %>% 
    write_csv(paste0(paths$output, str_replace(x, ".csv", "_filtered.csv")))
  
  hash <- digest(coe2, algo = "sha256")
  
  writeLines(hash, paste0(paths$output, str_replace(x, ".csv", "_filtered.txt")))
}

walk(coe2_files, import_coe2)

message("COE2 done.")

# done.