#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/prep-app/src/mod_genero_prep.R

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(stringr, dplyr, here, ggplot2, spatstat, googledrive, readr)

# Files
paths <- list(paridad_genero_sector = here("prep-app/output/paridad_genero_sector.csv"),
              paridad_genero_edad = here("prep-app/output/paridad_genero_edad.csv"),
              paridad_genero_geog = here("prep-app/output/paridad_genero_geog.csv"),
              brecha_sal_por_ocup = here("prep-app/output/brecha_sal_por_ocup.csv"),
              brecha_sal_abogadxs = here("prep-app/output/brecha_sal_abogadxs.csv"),
              participacion_carrera = here("prep-app/output/particip-carrera.csv"),
              brechas_participacion = here("prep-app/output/brechas-participacion.csv"),
              ejercicio_derecho = here("prep-app/output/ejercicio-derecho.csv"))

# Drive folder
enoe_drive <- drive_ls(as_id("1fQDbnBduTrth9RroQUhJemw9nB5Wp6RP"))

# Read ENOE
enoe <- drive_read_string(enoe_drive$id) %>%
  textConnection() %>%
  read.csv() %>%
  filter(edad >= 21 &
           # filter for data from past 5 years
            year >= (as.numeric(format(Sys.Date(), "%Y")) - 5))

# SECTION 1: Gender parity ====
# By sector
paridad_genero_sector <- enoe %>%
  filter(ocup_der & !is.na(sector)) %>%
  group_by(sector, sexo) %>%
  summarize(fac_sum_sexo = sum(fac)) %>%
  group_by(sector) %>%
  mutate(perc = round(fac_sum_sexo * 100 / sum(fac_sum_sexo), 1),
         fac_sum = sum(fac_sum_sexo)) %>%
  select(-fac_sum_sexo)

paridad_genero_general <- enoe %>%
  filter(ocup_der) %>%
  group_by(sexo) %>%
  summarize(fac_sum_sexo = sum(fac)) %>%
  ungroup() %>%
  mutate(perc = round(fac_sum_sexo * 100 / sum(fac_sum_sexo), 1),
         fac_sum = sum(fac_sum_sexo),
         sector = "General") %>%
  select(-fac_sum_sexo)

paridad_genero_sector <- bind_rows(paridad_genero_sector, paridad_genero_general)

write_csv(paridad_genero_sector, paths$paridad_genero_sector)

# By age
paridad_genero_edad <- enoe %>%
  filter(educ_der & edad < 80) %>%
  mutate(grupo_edad = cut(edad, breaks = seq(20, 80, by = 5), right = FALSE),
         sexo = ifelse(sexo == "mujer", "Mujeres", "Hombres")) %>%
  group_by(grupo_edad, sexo) %>%
  summarize(sum_fac = sum(fac),
            min_year = min(year),
            max_year = max(year)) %>%
  group_by(grupo_edad) %>%
  mutate(perc = round(sum_fac / sum(sum_fac), 3),
         edad_midpoint = as.numeric(substr(grupo_edad, 2, 3)) + 2) %>%
  select(-grupo_edad)

write_csv(paridad_genero_edad, paths$paridad_genero_edad)

# By state
paridad_genero_geog_ocup <- enoe %>%
  filter(ocup_der) %>%
  group_by(cve_ent) %>%
  summarize(perc_muj = round(100 * weighted.mean(sexo == "mujer", fac), 1)) %>%
  mutate(def_type = "ocup")

paridad_genero_geog_educ <- enoe %>%
  filter(educ_der) %>%
  group_by(cve_ent) %>%
  summarize(perc_muj = round(100 * weighted.mean(sexo == "mujer", fac), 1)) %>%
  mutate(def_type = "educ")

paridad_genero_geog <- bind_rows(paridad_genero_geog_ocup, paridad_genero_geog_educ)

write_csv(paridad_genero_geog, paths$paridad_genero_geog)

# SECTION 2: Gender wage gap ====
# By occupation
brecha_salarial_ocup <- enoe %>%
  filter(!is.na(ocups_de_interes) & ingocup > 0) %>%
  group_by(ocups_de_interes, sexo) %>%
  summarize(median = weighted.median(ingocup, fac)) %>%
  pivot_wider(names_from = sexo, values_from = median) %>%
  mutate(brecha_centavos_menos = round(100 * (1 - (mujer/hombre)), 1)) %>%
  select(-c(hombre, mujer))

write_csv(brecha_salarial_ocup, paths$brecha_sal_por_ocup)

# ONLY LAWYERS

# By age
brecha_abogadxs_edad <- enoe %>%
  filter(edad < 60 & ocup_der & ingocup > 0) %>%
  group_by(edad_categ_5, sexo) %>%
  summarize(ingresos_med_sex = weighted.median(ingocup, fac),
            fac_sum = sum(fac)) %>%
  group_by(edad_categ_5) %>%
  mutate(ingresos_med = round(weighted.mean(ingresos_med_sex, fac_sum), -2)) %>%
  select(-fac_sum) %>%
  pivot_wider(names_from = sexo, values_from = c(ingresos_med_sex, ingresos_med)) %>%
  mutate(brecha_centavos_menos =
           round(100 * (1 - (ingresos_med_sex_mujer/ingresos_med_sex_hombre)), 1),
         categ_type = "edad") %>%
  rename(categ_name = edad_categ_5,
         ingresos_med = ingresos_med_mujer) %>%
  select(-c(contains("med_sex"), ingresos_med_hombre))

# By sector
brecha_abogadxs_sector <- enoe %>%
  filter(!is.na(sector) & ocup_der & ingocup > 0 &
           # Remove ONG & escuela/hospital b/c too small sample size
           sector %in% c("ONG", "Escuela u hospital") == F) %>%
  # Condense public sector categories
  #mutate(sector = case_when(str_detect(sector, "Gobierno") | sector == "Poder judicial o legislativo" ~ "Gobierno",
   #                         T ~ sector)) %>%
  group_by(sector, sexo) %>%
  summarize(ingresos_med_sex = weighted.median(ingocup, fac),
            fac_sum = sum(fac)) %>%
  group_by(sector) %>%
  mutate(ingresos_med = round(weighted.mean(ingresos_med_sex, fac_sum), -2)) %>%
  select(-fac_sum) %>%
  pivot_wider(names_from = sexo, values_from = c(ingresos_med_sex, ingresos_med)) %>%
  mutate(brecha_centavos_menos =
           round(100 * (1 - (ingresos_med_sex_mujer/ingresos_med_sex_hombre)), 1),
         categ_type = "sector") %>%
  rename(categ_name = sector,
         ingresos_med = ingresos_med_mujer) %>%
  select(-c(contains("med_sex"), ingresos_med_hombre))

# By motherhood & marital status
brecha_abogadxs_e_con <- enoe %>%
  filter(!is.na(e_con) & ocup_der & ingocup > 0 & sexo == "mujer") %>%
  mutate(maternidad = case_when(str_detect(maternidad_e_con, "con hijes") ~ "Mujer con hijes",
                                str_detect(maternidad_e_con, "sin hijes") ~ "Mujer sin hijes")) %>%
  group_by(e_con, maternidad) %>%
  summarize(ingresos_muj = weighted.median(ingocup, fac),
            sum_fac_muj = sum(fac)) %>%
  left_join(enoe %>%
              filter(!is.na(e_con) & ocup_der & ingocup > 0 & sexo == "hombre") %>%
              group_by(e_con) %>%
              summarize(ingresos_hom = weighted.median(ingocup, fac),
                        sum_fac_hom = sum(fac)),
              by = "e_con") %>%
  mutate(brecha_centavos_menos =
           round(100 * (1 - (ingresos_muj/ingresos_hom)), 1),
         categ_type = "e_con") %>%
  group_by(e_con) %>%
  mutate(ingresos_med = (ingresos_muj * sum_fac_muj / (sum_fac_muj + sum_fac_hom)) +
           (ingresos_hom * sum_fac_hom / (sum_fac_muj + sum_fac_hom)),
         e_con = case_when(e_con == "casado o unión libre" ~ "Casada o en unión libre",
                           e_con == "soltero o divorciado" ~ "Soltera o divorciada")) %>%
  rename(categ_name = e_con) %>%
  select(-c(contains("muj"), contains("hom")))

# Join dfs for lawyers
brecha_abogadxs <- bind_rows(brecha_abogadxs_edad, brecha_abogadxs_sector, brecha_abogadxs_e_con)

write_csv(brecha_abogadxs, paths$brecha_sal_abogadxs)

# SECTION 3: labor market participation ====
# Participation by major
participacion_carrera <- enoe %>%
  filter(!is.na(carreras_de_interes)) %>%
  group_by(carreras_de_interes, sexo) %>%
  summarize(perc_pea = round(weighted.mean(pnea == "pea", fac), 3),
            count = n())

participacion_general <- enoe %>%
  group_by(sexo) %>%
  summarize(perc_pea = round(weighted.mean(pnea == "pea", fac), 3),
            count = n()) %>%
  mutate(carreras_de_interes = "General")

test <- enoe %>% 
  filter(educ_der & pnea == "pea") %>% 
  summarize(perc_mujer = round(weighted.mean(sexo == "mujer", fac), 3))
  
participacion_carrera <- bind_rows(participacion_carrera, participacion_general) %>%
  mutate(sexo = case_when(sexo == "hombre" ~ "Hombres",
                          sexo == "mujer" ~ "Mujeres"),
         margin_error =  1.645 * sqrt((perc_pea) *
                                              ( 1 - perc_pea) / count))

write_csv(participacion_carrera, paths$participacion_carrera)

# Gaps by major
brechas_participacion <- participacion_carrera %>%
  select(-c(margin_error, count)) %>%
  filter(carreras_de_interes != "General") %>%
  pivot_wider(names_from = sexo, values_from = perc_pea) %>%
  janitor::clean_names() %>%
  mutate(brecha = (hombres - mujeres)) %>%
  arrange(desc(brecha))

write_csv(brechas_participacion, paths$brechas_participacion)

# Exercise law major
ejercicio_general <- enoe %>%
  filter(educ_der & pnea == "pea") %>%
  group_by(sexo) %>%
  summarize(perc_ejercicio =  round(weighted.mean(ocup_der, fac), 3),
            count = n()) %>%
  mutate(categ_type = "solo_sexo")

ejercicio_e_con <- enoe %>%
  mutate(maternidad = case_when(
    n_hij > 0 ~ "Mujeres con hijes",
    sexo == "mujer" & n_hij == 0 ~ "Mujeres sin hijes",
    sexo == "hombre" ~ "Hombres")) %>%
  filter(educ_der & pnea == "pea" &
           !is.na(e_con) & !is.na(maternidad)) %>%
  group_by(sexo, e_con, maternidad) %>%
  summarize(perc_ejercicio = round(weighted.mean(ocup_der, fac), 3),
            count = n()) %>% 
  mutate(categ_type = "maternidad_e_con")

ejercicio_der <- bind_rows(ejercicio_general, ejercicio_e_con) %>%
  mutate(sexo = case_when(sexo == "hombre" ~ "Hombres",
                          sexo == "mujer" ~ "Mujeres"),
         e_con = case_when(e_con == "casado o unión libre" ~ "Casade o en unión libre",
                           e_con == "soltero o divorciado" ~ "Soltere o divorciade"),
         margin_error =  1.645 * sqrt((perc_ejercicio) *
                                              ( 1 - perc_ejercicio) / count))

write_csv(ejercicio_der, paths$ejercicio_derecho)

# done.
