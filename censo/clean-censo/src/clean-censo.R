#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/censo/import-censo/src/filter-censo.R 

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, R.utils, janitor, data.table, here, yaml, googledrive, digest)

paths <- list(input = here("censo/clean-censo/input/censo_filtered.csv"),
              vars_to_keep = here("censo/import-censo/hand/vars-to-keep.yaml"),
              output = here("censo/clean-censo/output/"))

# Define Drive output folder
drive_out <- as_id("1TuZc3R2Bt5eX7owFfS8-Qa6r0darYBMC")

# Read raw file
censo_filtered <- fread(paths$input)

message("Filtered Censo read.")

# Clean ====
stopifnot(all(censo_filtered$sexo == 3))

censo_cleaned <- censo_filtered %>% 
  mutate(
    hlengua = case_when(
      hlengua == 1 ~ T,
      hlengua == 3 ~ F,
      T ~ NA),
    perte_indigena = case_when(
      perte_indigena == 1 ~ T,
      perte_indigena == 3 ~ F,
      T ~ NA),
    lawyer = case_when(
      # Studied law
      startsWith(as.character(nomcar_c), "331") &
        # In a broad occupation category that includes lawyers
        as.character(ocupacion_c) %in% c("213", "122", "152") ~ T,
                       T ~ F),
    # Previously filtered to include only women
    sexo = "mujer",
    edad = case_when(
      edad > 130 ~ NA_real_,
      T ~ edad),
    ingtrmen = case_when(
      ingtrmen > 999998 ~ NA_real_,
      T ~ ingtrmen),
    situa_conyugal = case_when(
      situa_conyugal %in% c(1, 5:7) ~ "Casada o unión libre",
      situa_conyugal %in% c(2:4, 8) ~ "Soltera o divorciada",
      T ~ NA_character_),
    servicio_medico = case_when(
      servicio_medico == 5 ~ T,
      servicio_medico == 6 ~ F,
      T ~ NA),
    hijos_nac_vivos = case_when(
      hijos_nac_vivos %in% 0:25 ~ hijos_nac_vivos,
      T ~ NA_real_)) %>% 
  # Filter to include only lawyers
  filter(lawyer)
  
stopifnot(nrow(censo_cleaned) > 0,
          # Assert both T & F vals in hlengua & perte_indigena
          all(c(T, F) %in% censo_cleaned$hlengua),
          all(c(T, F) %in% censo_cleaned$perte_indigena))

# Export data ====
# Save local CSV
write_csv(censo_cleaned, paste0(paths$output, "censo_cleaned.csv"))

# Upload to Drive
drive_upload(paste0(paths$output, "censo_cleaned.csv"), path = drive_out, overwrite = TRUE)

# Write hash
message("Writing hash...")
hash <- digest(censo_cleaned, algo = "sha1")
writeLines(hash, paste0(paths$output, "censo_cleaned.txt"))

# Delete local CSVs (ARCHIVED) ====
# Clean output
file.remove(paste0(paths$output, "censo_cleaned.csv"))

# Import output
file.remove(paste0(paths$input))

# done.
  