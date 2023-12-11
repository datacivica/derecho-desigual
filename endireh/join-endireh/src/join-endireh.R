#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/endireh/join-endireh/src/join-tables.R

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, digest, here, googledrive, data.table)

# Clear environment
rm(list = ls())

# Files
paths <- list(input = here("endireh/join-endireh/input"),
              output = here("endireh/join-endireh/output/"))

# Define vars for merge
vars_merge <- c("upm", "viv_sel", "hogar", "n_ren", "year",
                "id_viv", "id_per", "fac_viv", "fac_muj")

# Read data ====
input_files <- list.files(paths$input, pattern = "\\.csv$", full.names = TRUE)

for (input_file in input_files) {
  file_name <- str_extract(input_file,"(?<=input/)(.*?)(?=\\.csv)")
  
  file_data <- read.csv(input_file)

  assign(file_name, file_data)
  
  message(paste0(file_name, " read."))
  
}
# 2021 ====
# Define vars for merge
vars_merge_2021 <- c("upm", "viv_sel", "hogar", "n_ren", "year",
                "id_viv", "id_per", "fac_viv", "fac_muj")

# Merge tables
endireh_joined_2021 <- sdem_2021 %>% 
  right_join(laboral_2021, by = vars_merge_2021) %>% 
  right_join(ingresos_2021, by = vars_merge_2021) %>% 
  right_join(escolar_2021, by = vars_merge_2021) %>% 
  right_join(obstetrica_2021, by = vars_merge_2021) %>% 
  right_join(discapacidad_2021, by = vars_merge_2021)
  
stopifnot(nrow(endireh_joined_2021) == nrow(laboral_2021))

# Save data locally
write_csv(endireh_joined_2021, paste0(paths$output, "endireh_joined_2021.csv"))

# 2016 ====
# Define vars for merge
vars_merge_2016 <- c("upm", "viv_sel", "hogar", "year", "n_ren" = "ren_m_ele",
                      "id_viv", "fac_viv", "fac_muj")

# Merge tables
endireh_joined_2016 <- sdem_2016 %>% 
  right_join(laboral_2016, by = vars_merge_2016) %>% 
  right_join(ingresos_2016, by = vars_merge_2016) %>% 
  right_join(escolar_2016, by = vars_merge_2016)  %>% 
  right_join(obstetrica_2016, by = vars_merge_2016)

stopifnot(nrow(endireh_joined_2016) == nrow(laboral_2016))

# Save data locally
write_csv(endireh_joined_2016, paste0(paths$output, "endireh_joined_2016.csv"))

# Write hashes ====
files_to_hash <- c("endireh_joined_2021", "endireh_joined_2016")

for (file in files_to_hash){
  hash <- digest(get(file), algo = "sha256")
  
  writeLines(hash, paste0(paths$output, file, ".txt"))
}

# done.
