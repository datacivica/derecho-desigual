#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/censo/import-censo/src/filter-censo.R 

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, R.utils, janitor, data.table, here, yaml, googledrive, digest)

paths <- list(vars_to_keep = here("censo/import-censo/hand/vars-to-keep.yaml"),
              output = here("censo/import-censo/output/"))

# Read data ====
# Define Drive folders
drive_in <- drive_ls(as_id("1GJtMUqrgPB0IjhaFnErVlsY2Yf2wHi99"))

# Read raw files from Drive
file_id <- drive_in$id

drive_download(as_id(file_id), path = paste0(tempdir(), "/censo.csv"), overwrite = TRUE)

censo_raw <- fread(paste0(tempdir(), "/censo.csv"))

file.remove(paste0(tempdir(), "/censo.csv"))

message("Raw Censo read.")

# Filter ====
# Define vars to keep
vars_to_keep <- read_yaml(paths$vars_to_keep)

censo_filtered <- censo_raw %>% 
  clean_names() %>% 
  select(any_of(vars_to_keep)) %>% 
  # Filter to include only adult women
  filter(edad >= 21 & sexo == 3)

stopifnot(all(vars_to_keep %in% names(censo_filtered)),
          names(censo_filtered) == tolower(names(censo_filtered)))

# Export ====
message("Saving local .csv")
# Save local CSV
write_csv(censo_filtered, paste0(paths$output, "censo_filtered.csv"))

# Write hash
message("Writing hash...")
hash <- digest(censo_filtered, algo = "sha1")
writeLines(hash, paste0(paths$output, "censo_filtered.txt"))

# done.