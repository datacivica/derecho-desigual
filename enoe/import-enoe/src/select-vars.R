#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/enoe/import-enoe/src/select-vars.R 

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, R.utils, janitor, data.table, foreign, here, yaml)

paths <- list(input = here("enoe/import-enoe/input"),
              output = here("enoe/import-enoe/output"),
              vars_to_keep = here("enoe/import-enoe/hand/vars-to-keep.yaml"))

# Define vars to keep
vars_to_keep <- read_yaml(paths$vars_to_keep)

all_vars_to_keep <- vars_to_keep$all

hogar_vars_to_keep <- c(all_vars_to_keep, vars_to_keep$hogar)

sdem_vars_to_keep <- c(all_vars_to_keep, vars_to_keep$sdem)

coe1_vars_to_keep <- c(all_vars_to_keep, vars_to_keep$coe1t)

coe2_vars_to_keep <- c(all_vars_to_keep, vars_to_keep$coe2t)

# Hogar
hogar_files <- dir(paths$input, pattern = "hog")

import_hogar <- function(x){
  hogar <- fread(paste0(paths$input, "/", x)) %>% 
    clean_names() %>% 
    select(any_of(hogar_vars_to_keep)) %>% 
    mutate(year = paste0("20", str_sub(x, 6, 7))) %>% 
    write_csv(paste0(paths$output, "/", str_replace(x, ".csv", "_filtered.csv")))
}

walk(hogar_files, import_hogar)

message("Hogar done.")

# Sociodemográficos
sdem_files <- dir(paths$input, pattern = "sdem")

import_sdem <- function(x){
  sdem <- fread(paste0(paths$input, "/", x)) %>% 
    clean_names() %>% 
    select(any_of(sdem_vars_to_keep)) %>% 
    mutate(year = paste0("20", str_sub(x, 7, 8))) %>% 
    write_csv(paste0(paths$output, "/", str_replace(x, ".csv", "_filtered.csv")))
}

walk(sdem_files, import_sdem)

message("SDem done.")

# COE1
coe1_files <- dir(paths$input, pattern = "coe1")

import_coe1 <- function(x){
  coe1 <- fread(paste0(paths$input, "/", x)) %>% 
    clean_names() %>% 
    select(any_of(coe1_vars_to_keep)) %>% 
    mutate(year = paste0("20", str_sub(x, 7, 8))) %>% 
    write_csv(paste0(paths$output, "/", str_replace(x, ".csv", "_filtered.csv")))
}

walk(coe1_files, import_coe1)

message("COE1 done.")

# COE2
coe2_files <- dir(paths$input, pattern = "coe2")

import_coe2 <- function(x){
  coe2 <- fread(paste0(paths$input, "/", x)) %>% 
    clean_names() %>% 
    select(any_of(coe2_vars_to_keep)) %>% 
    mutate(year = paste0("20", str_sub(x, 7, 8))) %>% 
    write_csv(paste0(paths$output, "/", str_replace(x, ".csv", "_filtered.csv")))
}

walk(coe2_files, import_coe2)

message("COE2 done.")

# done.