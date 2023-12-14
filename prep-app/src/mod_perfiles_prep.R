#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/prep-app/src/mod_perfiles_prep.R

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(stringr, dplyr, here, ggplot2, spatstat, googledrive)

# Files
paths <- list(perfiles_output = here("prep-app/output/perfiles.csv"),
              indigenous_output = here("prep-app/output/indigenous.csv"),
              income_dist_output = here("prep-app/output/income_dist.csv"),
              law_preval_age = here("prep-app/output/law_preval_age.csv"),
              law_preval_state = here("prep-app/output/law_preval_state.csv"),
              mxhexbin_map = here("prep-app/output/mxhexbin.csv"))

# Read México hexbin map
hexbin_drive <- drive_ls(as_id("1HHJfnggxdxxjcK7Ud7-n4zIfHdpJvlJa"))

mxhexbin.map <- drive_read_string(hexbin_drive$id) %>%
  textConnection() %>%
  read.csv()

# Drive folder
enoe_drive <- drive_ls(as_id("1fQDbnBduTrth9RroQUhJemw9nB5Wp6RP"))

# Read ENOE
enoe <- drive_read_string(enoe_drive$id) %>%
  textConnection() %>%
  read.csv() %>%
  filter(edad >= 21 &
           # filter for data from past 5 years
           year >= (as.numeric(format(Sys.Date(), "%Y")) - 5)) %>%
  # Condense public sector categories
  mutate(sector = case_when(str_detect(sector, "Gobierno") | sector == "Poder judicial o legislativo" ~ "Gobierno",
                            T ~ sector))

message("ENOE read.")

# SECTION 1: Distribution of sociodemographic characteristics ====
enoe_group_ocup <- enoe %>%
  mutate(
    # Define lawyers by occupation
    group = case_when(sexo == "mujer" & ocup_der ~ "Mujeres abogadas",
                      sexo == "mujer" & !ocup_der ~ "Mujeres no abogadas",
                      sexo == "hombre" & ocup_der ~ "Hombres abogados")) %>%
  filter(!is.na(group) & escolaridad_aprobado %in% c("Licenciatura", "Maestría", "Doctorado") &
           pnea == "pea")

# GROUP BY OCCUPATION

ocup_edad <- enoe_group_ocup %>%
  filter(!is.na(edad_categ_10) & !is.na(group)) %>%
  group_by(group, edad_categ_10) %>%
  summarize(weights = sum(fac)) %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(categ_perc = round(weights * 100 / sum(weights), 1),
         categ_type = "edad") %>%
  rename(categ_name = edad_categ_10) %>%
  select(group, categ_type, categ_name, categ_perc)

ocup_escolaridad <- enoe_group_ocup %>%
  filter(!is.na(escolaridad_aprobado) & !is.na(group)) %>%
  group_by(group, escolaridad_aprobado) %>%
  summarize(weights = sum(fac)) %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(categ_perc = round(weights * 100 / sum(weights), 1),
         categ_type = "escolaridad") %>%
  rename(categ_name = escolaridad_aprobado) %>%
  select(group, categ_type, categ_name, categ_perc)

ocup_sector <- enoe_group_ocup %>%
  filter(!is.na(sector) & !is.na(group)) %>%
  group_by(group, sector) %>%
  summarize(weights = sum(fac)) %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(categ_perc = round(weights * 100 / sum(weights), 1),
         categ_type = "sector") %>%
  rename(categ_name = sector) %>%
  select(group, categ_type, categ_name, categ_perc)

ocup_mat_e_con <- enoe_group_ocup %>%
  filter(!is.na(maternidad_e_con) & !is.na(group)) %>%
  group_by(group, maternidad_e_con) %>%
  summarize(weights = sum(fac)) %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(categ_perc = round(weights * 100 / sum(weights), 1),
         categ_type = "maternidad_e_con") %>%
  rename(categ_name = maternidad_e_con) %>%
  select(group, categ_type, categ_name, categ_perc)

# Join all occupation dfs
ocup_all <- bind_rows(ocup_edad, ocup_escolaridad, ocup_sector, ocup_mat_e_con) %>%
  mutate(def_type = "ocup")

# GROUP BY EDUCATION

enoe_group_educ <- enoe %>%
  filter(escolaridad_aprobado %in% c("Licenciatura", "Maestría", "Doctorado")) %>%
  mutate(
  # Define lawyers by education (licenciatura en derecho)
  group = case_when(sexo == "mujer" & educ_der ~ "Mujeres abogadas",
                    sexo == "mujer" & !educ_der ~ "Mujeres no abogadas",
                    sexo == "hombre" & educ_der ~ "Hombres abogados")) %>%
  filter(!is.na(group))

educ_edad <- enoe_group_educ %>%
  filter(!is.na(edad_categ_10) & !is.na(group)) %>%
  group_by(group, edad_categ_10) %>%
  summarize(weights = sum(fac)) %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(categ_perc = round(weights * 100 / sum(weights), 1),
         categ_type = "edad") %>%
  rename(categ_name = edad_categ_10) %>%
  select(group, categ_type, categ_name, categ_perc)

educ_escolaridad <- enoe_group_educ %>%
  filter(!is.na(escolaridad_aprobado) & !is.na(group)) %>%
  group_by(group, escolaridad_aprobado) %>%
  summarize(weights = sum(fac)) %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(categ_perc = round(weights * 100 / sum(weights), 1),
         categ_type = "escolaridad") %>%
  rename(categ_name = escolaridad_aprobado) %>%
  select(group, categ_type, categ_name, categ_perc)

educ_sector <- enoe_group_educ %>%
  filter(!is.na(sector) & !is.na(group)) %>%
  group_by(group, sector) %>%
  summarize(weights = sum(fac)) %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(categ_perc = round(weights * 100 / sum(weights), 1),
         categ_type = "sector") %>%
  rename(categ_name = sector) %>%
  select(group, categ_type, categ_name, categ_perc)

educ_mat_e_con <- enoe_group_educ %>%
  filter(!is.na(maternidad_e_con) & !is.na(group)) %>%
  group_by(group, maternidad_e_con) %>%
  summarize(weights = sum(fac)) %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(categ_perc = round(weights * 100 / sum(weights), 1),
         categ_type = "maternidad_e_con") %>%
  rename(categ_name = maternidad_e_con) %>%
  select(group, categ_type, categ_name, categ_perc)

# Join all education dfs
educ_all <- bind_rows(educ_edad, educ_sector, educ_mat_e_con) %>%
  mutate(def_type = "educ")

# Join both education and ocupation dfs
ocup_educ_all <- bind_rows(ocup_all, educ_all) %>%
  mutate(min_year = min(enoe$year),
         max_year = max(enoe$year))

# Assert that there are no NAs in ocup_educ_all
stopifnot(!any(is.na(ocup_educ_all$categ_perc)))

# Output data
write_csv(ocup_educ_all, paths$perfiles_output)

# SECTION 2: Income distribution ====
income_dist <- enoe_group_ocup %>%
  filter(ingocup > 0 & escolaridad_aprobado %in% c("Licenciatura", "Maestría", "Doctorado")) %>%
  group_by(group, ingocup) %>%
  summarize(weights = sum(fac)) %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(income_perc = round(weights * 100 / sum(weights), 1),
         median = round(weighted.median(ingocup, weights), 0))

write_csv(income_dist, paths$income_dist_output)

# SECTION 3: Prevalence of law as profession ====
# By age
law_preval_lic <- enoe %>%
  mutate(lic = escolaridad_aprobado %in% c("Licenciatura", "Maestría", "Doctorado")) %>%
  filter(lic) %>%
  group_by(sexo, edad) %>%
  summarize(lic_law = round(weighted.mean(educ_der, fac), 3),
            lic_non_law = round(weighted.mean(!educ_der, fac), 3),
            weights = sum(fac)) %>%
  pivot_longer(cols = c("lic_law", "lic_non_law"), names_to = "educ", values_to = "perc") %>%
  mutate(type = "just_lic")

law_preval_all <- enoe %>%
  mutate(lic = escolaridad_aprobado %in% c("Licenciatura", "Maestría", "Doctorado")) %>%
  group_by(sexo, edad) %>%
  summarize(lic_law = round(weighted.mean(educ_der, fac), 3),
            lic_non_law = round(weighted.mean(lic & !educ_der, fac), 3),
            no_lic = round(weighted.mean(!lic, fac), 3),
            weights = sum(fac)) %>%
  pivot_longer(cols = c("lic_law", "lic_non_law", "no_lic"), names_to = "educ", values_to = "perc") %>%
  mutate(type = "all")

law_preval <- bind_rows(law_preval_lic, law_preval_all)

write_csv(law_preval, paths$law_preval_age)

# By state
ocup_by_state_hom <- enoe %>%
  filter(sexo == "hombre") %>%
  group_by(cve_ent) %>%
  summarize(perc = round(100 * weighted.mean(ocup_der, fac), 1)) %>%
  mutate(sexo = "hombre",
         def_type = "ocup")

ocup_by_state_muj <- enoe %>%
  filter(sexo == "mujer") %>%
  group_by(cve_ent) %>%
  summarize(perc = round(100 * weighted.mean(ocup_der, fac), 1)) %>%
  mutate(sexo = "mujer",
         def_type = "ocup")

educ_by_state_hom <- enoe %>%
  filter(sexo == "hombre") %>%
  group_by(cve_ent) %>%
  summarize(perc = round(100 * weighted.mean(educ_der, fac), 1)) %>%
  mutate(sexo = "hombre",
         def_type = "educ")

educ_by_state_muj <- enoe %>%
  filter(sexo == "mujer") %>%
  group_by(cve_ent) %>%
  summarize(perc = round(100 * weighted.mean(educ_der, fac), 1)) %>%
  mutate(sexo = "mujer",
         def_type = "educ")

ocup_by_state_gen <- enoe %>%
  group_by(cve_ent) %>%
  summarize(perc = round(100 * weighted.mean(ocup_der, fac), 1)) %>%
  mutate(sexo = "general",
         def_type = "ocup")

educ_by_state_gen <- enoe %>%
  group_by(cve_ent) %>%
  summarize(perc = round(100 * weighted.mean(educ_der, fac), 1)) %>%
  mutate(sexo = "general",
         def_type = "educ")

prev_by_state <- bind_rows(ocup_by_state_hom, ocup_by_state_muj,
                           educ_by_state_hom, educ_by_state_muj,
                           ocup_by_state_gen, educ_by_state_gen)

write_csv(prev_by_state, paths$law_preval_state)
write_csv(mxhexbin.map, paths$mxhexbin_map)

# done.
