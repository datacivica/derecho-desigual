#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/enoe/clean-enoe/src/clean-enoe.R

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, yaml, janitor, googledrive, digest, data.table)

# Files
paths <- list(input = here("enoe/clean-enoe/input/enoe_joined.csv"),
              output = here("enoe/clean-enoe/output/"),
              vars_dict = here("enoe/clean-enoe/hand/vars-rename-dict.yaml"),
              geog_cat = here("endireh/clean-endireh/hand/geog-catalog.csv"))

# Define Drive folder
drive_out <- as_id("1fQDbnBduTrth9RroQUhJemw9nB5Wp6RP")

# Read data ====
enoe_raw <- fread(paths$input)

geog_cat <- read.csv(paths$geog_cat) %>% 
  clean_names %>% 
  select(contains(c("ent", "mun"))) %>% 
  mutate(cve_ent = sprintf("%02d", cve_ent),
         cve_mun = paste0(cve_ent, sprintf("%03d", cve_mun))) %>% 
  distinct()

stopifnot(all(nchar(geog_cat$cve_mun) == 5))

# Read variable rename dictionary ====
vars_rename_dict_raw <- read_yaml(paths$vars_dict)
vars_rename_dict <- list()
for (i in seq_along(vars_rename_dict_raw)){
  vars_rename_dict[i] <- vars_rename_dict_raw[[i]]
  names(vars_rename_dict)[i] <- names(vars_rename_dict_raw[[i]])
}

# Combine equivalent columns =====
message("Combining equivalent columns.")
enoe_combined <- enoe_raw %>% 
  mutate(fac = ifelse(!is.na(fac), fac, fac_tri),
         t_loc = ifelse(!is.na(t_loc), t_loc, t_loc_tri)) %>% 
  select(-c(fac_tri, t_loc_tri)) %>% 
  filter(fac > 0 &
           # Filter to include only adults
           eda %in% 18:97)

stopifnot(
  all(!is.na(enoe_combined$fac)),
  all(!is.na(enoe_combined$t_loc)))

# Sum hours dedicated to unpaid care labor ====
message("Summing care hours.")
enoe_care_summed <- enoe_combined %>% 
  mutate(p11_h6 = ifelse(year %in% 2008:2012, 0, p11_h6),
         across(.cols = starts_with("p11"),
                .fns = ~ ifelse(. %in% 98:99, NA, .)),
         cuidados_thrs = rowSums(select(., p11_h2:p11_h7), na.rm = TRUE)) %>% 
  select(-starts_with("p11"))

stopifnot("cuidados_thrs" %in% names(enoe_care_summed))

# Recode variable values ====
message("Recoding variable values.")
enoe_recoded <- enoe_care_summed %>% 
  mutate(p4_1 = case_when(p4_1 %in% 1:3 ~ T,
                          p4_1 == 4 ~ F),
         t_loc = case_when(t_loc == 1 ~ "más de 100,000",
                           t_loc == 2 ~ "15,000 a 99,999",
                           t_loc == 3 ~ "2,500 a 14,999",
                           t_loc == 4 ~ "menos de 2,500"),
         sex = case_when(sex == 1 ~ "hombre",
                         sex == 2 ~ "mujer"),
         edad_categ_10 = case_when(eda %in% 21:29 ~ "21 a 29 años",
                             eda %in% 30:39 ~ "30 a 39 años",
                             eda %in% 40:49 ~ "40 a 49 años",
                             eda %in% 50:59 ~ "50 a 59 años",
                             eda >= 60 ~ "60 años o más"),
         edad_categ_5 = case_when(eda %in% 21:25 ~ "21 a 25 años",
                                  eda %in% 26:29 ~ "26 a 29 años",
                                  eda %in% 30:34 ~ "30 a 34 años",
                                  eda %in% 35:39 ~ "35 a 39 años",
                                  eda %in% 40:44 ~ "40 a 44 años",
                                  eda %in% 45:49 ~ "45 a 49 años",
                                  eda %in% 50:54 ~ "50 a 54 años",
                                  eda %in% 55:59 ~ "55 a 59 años",
                                  eda >= 60 ~ "60 años o más"),
         cs_p13_1 = case_when(cs_p13_1 <= 1 ~ "Preescolar o menos",
                              cs_p13_1 == 2 ~ "Primaria",
                              cs_p13_1 == 3 ~ "Secundaria",
                              cs_p13_1 == 4 ~ "Preparatoria",
                              cs_p13_1 == 5 | cs_p13_1 == 6 ~ "Carrera técnica",
                              cs_p13_1 == 7 ~ "Licenciatura",
                              cs_p13_1 == 8 ~ "Maestría",
                              cs_p13_1 == 9 ~ "Doctorado"),
         cs_p16 = case_when(cs_p16 == 1 ~ T,
                            cs_p16 == 2 ~ F),
         cs_p17 = case_when(cs_p17 == 1 ~ T,
                            cs_p17 == 2 ~ F),
         e_con = case_when(e_con %in% c(1, 5) ~ "casado o unión libre",
                           e_con %in% c(2, 3, 4, 6) ~ "soltero o divorciado",
                           T ~ NA_character_),
         clase1 = case_when(clase1 == 1 ~ "pea",
                            clase1 == 2 ~ "pnea"),
         clase2 = case_when(clase2 == 1 ~ "ocupado",
                            clase2 == 2 ~ "desocupado",
                            clase2 == 3 ~ "disponible",
                            clase2 == 4 ~ "no disponible"),
         ing7c = case_when(ing7c == 1 ~ "hasta un s.m",
                           ing7c == 2 ~ "1 - 2 s.m.",
                           ing7c == 3 ~ "2 - 3 s.m.",
                           ing7c == 4 ~ "3 - 5 s.m.",
                           ing7c == 5 ~ "más de 5 s.m.",
                           ing7c == 6 ~ "no recibe ingresos",
                           T ~ NA_character_),
         dur9c = case_when(dur9c == 2 ~ "menos de 15 hrs",
                           dur9c == 3 ~ "15 a 24 hrs",
                           dur9c == 4 ~ "25 a 34 hrs",
                           dur9c == 5 ~ "35 a 39 hrs",
                           dur9c == 6 ~ "40 a 48 hrs",
                           dur9c == 7 ~ "49 a 56 hrs",
                           dur9c == 8 ~ "más de 56 hrs",
                           T ~ NA_character_),
         emp_ppal = case_when(emp_ppal == 1 ~ "informal",
                              emp_ppal == 2 ~ "formal",
                              T ~ NA_character_),
         p1 = case_when(p1 == 1 ~ T,
                        p1 == 2 ~ F),
         p1a1 = ifelse(p1a1 == 1, T, NA),
         p1b = case_when(p1b == 1 ~ T,
                        p1b == 2 ~ F),
         p3b = case_when(p3b == 1 ~ T,
                        p3b == 2 ~ F),
         p3d = case_when(p3d == 1 ~ T,
                         p3d == 2 ~ F,
                         T ~ NA),
         p3j = case_when(p3j == 1 ~ T,
                         p3j == 2 ~ F,
                         T ~ NA),
         p3k1 = case_when(p3j == 1 ~ "temporal",
                         p3j == 2 ~ "indefinido",
                         T ~ NA_character_),
         p3m2 = ifelse(p3m2 == 2, T, F),
         p3m3 = ifelse(p3m3 == 3, T, F),
         # Didn't code p3q b/c I don't think we'll end up using it
         p4c = case_when(p4c == 1 ~ "Empresa personal",
                         p4c == 2 ~ "Empresa del sector privado",
                         T ~ NA_character_),
         p4d2 = case_when(p4d1 == 1 & p4d2 == 1 ~ "Poder judicial o legislativo",
                          p4d1 == 1 & p4d2 == 2 ~ "Empresa pública",
                          p4d1 == 1 & p4d2 == 3 ~ "Escuela u hospital público",
                          p4d1 == 1 & p4d2 == 4 ~ "Gobierno federal",
                          p4d1 == 1 & p4d2 == 5 ~ "Gobierno estatal",
                          p4d1 == 1 & p4d2 == 6 ~ "Gobierno municipal",
                          p4d1 == 2 & p4d2 == 1 ~ "Escuela u hospital particular",
                          p4d1 == 2 & p4d2 == 2 ~ "Universidad pública",
                          p4d1 == 2 & p4d2 == 3 ~ "Organismo autónomo",
                          p4d1 == 2 & p4d2 %in% 4:5 ~ "ONG",
                          p4d1 == 2 & p4d2 == 6 ~ "Organismo internacional",
                          p4d1 == 2 & p4d2 == 7 ~ "Partido político",
                          T ~ p4c),
         sector = case_when(p4d2 %in% c("Empresa pública", "Organismo autónomo", "Organismo internacional", "Partido político") ~ "Gobierno (otro)",
                            p4d2 %in% c("Escuela u hospital público", "Escuela u hospital particular", "Universidad pública") ~ "Escuela u hospital",
                            p4d2 %in% c("Gobierno estatal", "Gobierno municipal") ~ "Gobierno estatal o municipal",
                            T ~ p4d2),
         p4d1 = case_when(p4d1 == 1 ~ "gobierno",
                          p4d1 == 2 ~ "no gobierno",
                          T ~ NA_character_),
         p5e_thrs = ifelse(p5e_thrs == 999, NA, p5e_thrs),
         # Didn't code p6c because seems to ask the same as ing7c
         p7 = ifelse(p7 %in% 1:6, T, F),
         ocup_der = case_when(p3 %in% c("2135", "1224", "1524") ~ T,
                              T ~ F),
         ocup_der_desc = case_when(p3 == "2135" ~ "abogado",
                                   p3 == "1224" ~ "juez",
                                   p3 == "1524" ~ "coordinar/jefe"),
         educ_der = case_when(year <= 2012 & cs_p14_c == "3741" ~ T,
                              year %in% 2013:2021 & str_detect(cs_p14_c, "341") ~ T,
                              year > 2021 & cs_p14_c == "33100" ~ T,
                              T ~ F),
         ocups_de_interes = case_when(
           ocup_der ~ "Abogades",
           substr(p3, 1, 3) %in% c("241", "242") ~ "Médiques",
           p3 == "2436" ~ "Enfermeres",
           startsWith(as.character(p3), "121") ~ "Docentes de educación básica",
           startsWith(as.character(p3), "321") ~ "Recepcionistas",
           substr(as.character(p3), 1, 3) %in% c("224", "225", "226", "228") ~ "Ingenieres",
           startsWith(as.character(p3), "95") ~ "Vendedores ambulantes",
           TRUE ~ NA_character_),
         carreras_de_interes = case_when(
           # DOES NOT INCLUDE PRIOR TO 2022
           educ_der ~ "Derecho",
           year > 2021 & startsWith(cs_p14_c, "911") ~ "Medicina",
           year > 2021 & startsWith(cs_p14_c, "92") ~ "Enfermería",
           # year > 2021 & startsWith(cs_p14_c, "311") ~ "Psicología",
           year > 2021 & startsWith(cs_p14_c, "4") ~ "Administración",
           year > 2021 & startsWith(cs_p14_c, "71") ~ "Ingeniería",
           year > 2021 & startsWith(cs_p14_c, "1") ~ "Educación",
           TRUE ~ NA_character_),
         maternidad_e_con = case_when(
           n_hij %in% 1:25 & e_con == "casado o unión libre"~ "Casada o unión libre, con hijes",
           n_hij %in% 1:25 & e_con == "soltero o divorciado"~ "Soltera o divorciada, con hijes",
           n_hij == 0 & e_con == "casado o unión libre"~ "Casada o unión libre, sin hijes",
           n_hij == 0 & e_con == "soltero o divorciado" ~ "Soltera o divorciada, sin hijes",
           sex == "hombre" ~ str_to_sentence(e_con),
           T ~ NA_character_),
         # Pad muni & ent codes
         ent = sprintf("%02d", ent),
         mun = paste0(ent, sprintf("%03d", mun)))

stopifnot(sum(enoe_recoded$ocup_der) == sum(!is.na(enoe_recoded$ocup_der_desc)),
          sum(!is.na(enoe_recoded$sector)) == sum(!is.na(enoe_recoded$p4d2)))

# Rename columns ====
message("Renaming columns.")
enoe_renamed <- enoe_recoded 
for (orig_col in names(vars_rename_dict)){
  names(enoe_renamed)[which(names(enoe_renamed) == orig_col)] <- vars_rename_dict[[orig_col]]
}

# Add location names ====
message("Adding location names.")
enoe_w_geog_names <- enoe_renamed %>% 
  left_join(geog_cat, by = c("cve_ent", "cve_mun"))

stopifnot(mean(!is.na(enoe_w_geog_names$nom_mun)) > 0.9)

# Export data ====
# Save local CSV
write_csv(enoe_w_geog_names, paste0(paths$output, "enoe_cleaned.csv"))

# Upload to Drive
drive_upload(paste0(paths$output, "enoe_cleaned.csv"), path = drive_out, overwrite = TRUE)

# Write hash
message("Writing hash...")
hash <- digest(enoe_w_geog_names, algo = "sha1")
writeLines(hash, paste0(paths$output, "enoe_cleaned.txt"))

# Delete local CSVs (ARCHIVED) ====
# Clean output
file.remove(paste0(paths$output, "enoe_cleaned.csv"))

# Join output
file.remove(paste0(paths$input))

# Import output
enoe_base_path <- substr(paths$output, 1, 40)
import_output_dir <- list.files(paste0(enoe_base_path, "import-enoe/output/"), pattern = ".csv")

for (file in import_output_dir){
 file.remove(paste0(enoe_base_path, "import-enoe/output/", file))
}

# done.
