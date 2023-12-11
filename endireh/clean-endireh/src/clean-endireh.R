#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/endireh/clean-endireh/src/clean-endireh.R

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, yaml, janitor, googledrive, data.table, digest)

# Files
paths <- list(input_2016 = here("endireh/clean-endireh/input/endireh_joined_2016.csv"),
              input_2021 = here("endireh/clean-endireh/input/endireh_joined_2021.csv"),
              output = here("endireh/clean-endireh/output/"),
              vars_dict_2021 = here("endireh/clean-endireh/hand/vars-rename-dict-2021.yaml"),
              vars_dict_2016 = here("endireh/clean-endireh/hand/vars-rename-dict-2016.yaml"),
              geog_cat = here("endireh/clean-endireh/hand/geog-catalog.csv"))

# Read ENDIREH ====
message("Reading data.")
endireh_joined_2016 <- read.csv(paths$input_2016)
endireh_joined_2021 <- read.csv(paths$input_2021)

# Define Drive folders
drive_out <- as_id("1iVZscdnkjFbPOFgsIoXxwi5idNyyyRsQ")

# Read entidad/muni catalogs ====
geog_cat <- read.csv(paths$geog_cat) %>% 
  clean_names %>% 
  select(contains(c("ent", "mun"))) %>% 
  mutate(cve_ent = sprintf("%02d", cve_ent),
         cve_mun = paste0(cve_ent, sprintf("%03d", cve_mun))) %>% 
  distinct()

# Read variable rename dictionaries ====
vars_rename_dict_raw_2021 <- read_yaml(paths$vars_dict_2021)
vars_rename_dict_2021 <- list()
for (i in seq_along(vars_rename_dict_raw_2021)){
  vars_rename_dict_2021[i] <- vars_rename_dict_raw_2021[[i]]
  names(vars_rename_dict_2021)[i] <- names(vars_rename_dict_raw_2021[[i]])
}

vars_rename_dict_raw_2016 <- read_yaml(paths$vars_dict_2016)
vars_rename_dict_2016 <- list()
for (i in seq_along(vars_rename_dict_raw_2016)){
  vars_rename_dict_2016[i] <- vars_rename_dict_raw_2016[[i]]
  names(vars_rename_dict_2016)[i] <- names(vars_rename_dict_raw_2016[[i]])
}

# Assert all original column names in dict exist in imported ENDIREH
stopifnot(all(names(vars_rename_dict_2021) %in% names(endireh_joined_2021)),
          all(names(vars_rename_dict_2016) %in% names(endireh_joined_2016)))

# Rename columns ====
message("Renaming columns.")
endireh_renamed_2021 <- endireh_joined_2021
for (orig_col in names(vars_rename_dict_2021)){
  names(endireh_renamed_2021)[which(names(endireh_renamed_2021) == orig_col)] <- vars_rename_dict_2021[[orig_col]]
}

endireh_renamed_2016 <- endireh_joined_2016
for (orig_col in names(vars_rename_dict_2016)){
  names(endireh_renamed_2016)[which(names(endireh_renamed_2016) == orig_col)] <- vars_rename_dict_2016[[orig_col]]
}

# Assert all renamed columns have unique names
stopifnot(length(names(endireh_renamed_2021)) == length(unique(names(endireh_renamed_2021))),
          length(names(endireh_renamed_2016)) == length(unique(names(endireh_renamed_2016))))

# Remove unwanted columns
endireh_renamed_2021 <- endireh_renamed_2021 %>% select(-starts_with("remove"))

endireh_renamed_2016 <- endireh_renamed_2016 %>% select(-starts_with("remove"))

# Combine 2016 and 2021 data ====
message("Combining 2016 and 2021 data.")

endireh_renamed <- bind_rows(endireh_renamed_2016, endireh_renamed_2021)

# Combine equivalent columns ====
message("Combining equivalent columns.")

endireh_to_comb <- endireh_renamed

# Columns to be combined are those ending in digits
digit_cols <- names(endireh_to_comb)[grepl("\\d+$", names(endireh_to_comb))]

# Extract prefixes (prior to trailing digits) among cols to be combined
prefixes <- c()
for (name in digit_cols) {
  prefixes <- unique(c(prefixes, sub("_[0-9_]+$", "", name)))}

# Combine columns sharing same prefix
for (prefix in prefixes) {
  # Find columns starting with prefix
  columns_to_combine <- grep(paste0("^", prefix, "_"), names(endireh_to_comb))
  
  # Combine columns and take the minimum value, handling missing values
  combined_column <- apply(endireh_to_comb[, columns_to_combine], 1, function(x) {
    if (all(is.na(x))) {
      NA
    } else {
      min(x, na.rm = TRUE)
    }
  })
  
  # Add the combined column to the new dataframe
  stopifnot(!all(is.na(combined_column)))
  endireh_to_comb[[prefix]] <- combined_column
  
  message(paste0(prefix, " combined."))
}

# Remove digit columns (already combined)
endireh_combined <- endireh_to_comb %>% 
  select(-all_of(digit_cols))

# Assert new columns have non-NA values
stopifnot(any(!is.na(endireh_combined$embzo_prueba_trab)),
          any(!is.na(endireh_combined$lab_obligar_sexo)),
          any(!is.na(endireh_combined$perp_lab_com_mujer_no_trabajar)),
          any(!is.na(endireh_combined$esc_obligar_sexo)))

# Recode variable values ====
message("Recoding variable values.")

endireh_recoded <- endireh_combined %>% 
  filter(fac_viv > 0 & fac_muj > 0 & 
           # Filter to include only adults
           edad %in% 18:97) %>% 
  mutate(sexo = case_when(sexo == 1 ~ "hombre",
                          sexo == 2 ~ "mujer"),
         edad_categ_10 = case_when(edad %in% 21:29 ~ "21 a 29 años",
                                   edad %in% 30:39 ~ "30 a 39 años",
                                   edad %in% 40:49 ~ "40 a 49 años",
                                   edad %in% 50:59 ~ "50 a 59 años",
                                   edad >= 60 ~ "60 años o más"),
         edad_categ_5 = case_when(edad %in% 21:25 ~ "21 a 25 años",
                                  edad %in% 26:29 ~ "26 a 29 años",
                                  edad %in% 30:34 ~ "30 a 34 años",
                                  edad %in% 35:39 ~ "35 a 39 años",
                                  edad %in% 40:44 ~ "40 a 44 años",
                                  edad %in% 45:49 ~ "45 a 49 años",
                                  edad %in% 50:54 ~ "50 a 54 años",
                                  edad %in% 55:59 ~ "55 a 59 años",
                                  edad >= 60 ~ "60 años o más"),
         escolaridad_aprobado = case_when(escolaridad_aprobado <= 1 ~ "preescolar o menos",
                                          escolaridad_aprobado == 2 ~ "primaria",
                                          escolaridad_aprobado == 3 ~ "secundaria",
                                          escolaridad_aprobado == 4 ~ "preparatoria",
                                          escolaridad_aprobado %in% 5:8 ~ "carrera técnica",
                                          escolaridad_aprobado %in% 9:10 ~ "licenciatura",
                                          escolaridad_aprobado == 11 ~ "posgrado"),
         indigena = case_when(indigena == 1 ~ T,
                              indigena %in% 2:3 ~ F,
                              T ~ NA),
         e_con = case_when(e_con %in% c(1, 5) ~ "Casada o unión libre",
                           e_con %in% c(2, 3, 4, 6) ~ "Soltera o divorciada",
                           T ~ NA_character_),
         sector_trabajo = case_when(sector_trabajo == 1 ~ "Gobierno estatal o municipal",
                                    sector_trabajo == 2 ~ "Gobierno federal",
                                    sector_trabajo == 3 ~ "Escuela o universidad pública",
                                    sector_trabajo == 4 ~ "Clínica u hospital público",
                                    sector_trabajo %in% 5:6 ~ "Empresa del sector privado",
                                    sector_trabajo == 7 ~ "Escuela privada",
                                    sector_trabajo == 8 ~ "Clínica u hospital particular",
                                    sector_trabajo == 12 ~ "Empresa personal",
                                    sector_trabajo %in% c(9:11, 13:14) ~ "Otro"),
         ingreso_cantidad = case_when(ingreso_cantidad %in% c(0:999997) ~ ingreso_cantidad,
                                      T ~ NA_integer_),
         ingreso_frecuencia = case_when(ingreso_frecuencia == 1 ~ 4,
                                        ingreso_frecuencia == 2 ~ 2,
                                        ingreso_frecuencia == 3 ~ 1,
                                        T ~ NA_real_),
         ingreso_mensual = ingreso_cantidad * ingreso_frecuencia,
         escuela_tipo = case_when(escuela_tipo == 1 ~ "pública",
                                  escuela_tipo == 2 ~ "privada",
                                  T ~ NA_character_),
         escuela_nivel = case_when(escuela_nivel %in% 1:3 ~ "prepa o menos",
                                   escuela_nivel %in% 4:5 ~ "técnica",
                                   escuela_nivel == 6 ~ "universidad",
                                   T ~ NA_character_),
         # Same in SINCO 2011 & SINCO 2019
         ocup_der = case_when(sinco %in% c("2135", "1224", "1524") ~ T,
                              T ~ F),
         ocup_der_desc = case_when(sinco == "2135" ~ "abogado",
                                   sinco == "1224" ~ "juez",
                                   sinco == "1524" ~ "coordinar/jefe"),
         ocups_de_interes = case_when(
           ocup_der ~ "Abogadas",
           substr(sinco, 1, 3) %in% c("241", "242") ~ "Médicas",
           year == 2021 & sinco == "2436" |
             year == 2016 & startsWith(as.character(sinco), "281") ~ "Enfermeras",
           year == 2021 & startsWith(as.character(sinco), "121") |
             year == 2016 & startsWith(as.character(sinco), "233") ~ "Docentes de educación básica",
           startsWith(as.character(sinco), "321") ~ "Recepcionistas",
           substr(as.character(sinco), 1, 3) %in% c("224", "225", "226", "228") ~ "Ingenieras",
           startsWith(as.character(sinco), "95") ~ "Vendedoras ambulantes",
           TRUE ~ NA_character_),
         # Perpetrator vars
         across(.cols = starts_with("perp"),
                .fns = ~ case_when(. %in% 1:3 ~ "jefe, supervisor o gerente",
                                   . == 4 ~ "compañero",
                                   . == 5 ~ "cliente",
                                   . %in% 6:8 ~ "otro",
                                   T ~ NA_character_)),
         # T/F vars
         across(.cols = c("lengua_indigena", starts_with(c("embzo_", "anio_", "lab_", "esc_"))),
                .fns = ~ case_when(. == 1 ~ T,
                                   . == 2 ~ F,
                                   T ~ NA)),
         # Disability vars
         across(.cols = starts_with("disc"),
                .fns = ~ case_when(. %in% 1:2 ~ T,
                                   . %in% 3:4 ~ F,
                                   T ~ NA)),
         # Pad muni & ent codes
         cve_ent = sprintf("%02d", cve_ent),
         cve_mun = paste0(cve_ent, sprintf("%03d", cve_mun))) %>% 
  left_join(geog_cat, by = c("cve_ent", "cve_mun"))

stopifnot(sum(endireh_recoded$ocup_der) == sum(!is.na(endireh_recoded$ocup_der_desc)))

# Export ====
message("Exporting data.")

# Save local output file
write_csv(endireh_recoded, paste0(paths$output, "endireh_cleaned.csv"))

# Upload to Drive
drive_upload(paste0(paths$output, "endireh_cleaned.csv"), path = drive_out, overwrite = TRUE)

# Write hash
hash <- digest(endireh_recoded, algo = "sha256")

writeLines(hash, paste0(paths$output, "endireh_cleaned.txt"))

# Delete local files (ARCHIVED) ====
# Clean output
file.remove(paste0(paths$output, "endireh_cleaned.csv"))

# Join output
file.remove(paths$input_2016)
file.remove(paths$input_2021)

# Import output
endireh_base_path <- substr(paths$output, 1, 43)
import_output_dir <- list.files(paste0(endireh_base_path, "import-endireh/output/"), pattern = ".csv")

for (file in import_output_dir){
  file.remove(paste0(endireh_base_path, "import-endireh/output/", file))
}

# done.