#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/prep-app/src/mod_abogadas_prep.R

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(stringr, ggplot2, dplyr, here, spatstat, googledrive, janitor)

# Files
paths <- list(
  enoe_input = here("prep-app/input/enoe_cleaned.csv"),
  sit_fam_joined = here("prep-app/output/sit_fam_joined.csv"),
  indig_joined = here("prep-app/output/indig_subrep.csv"),
  brecha_indig = here("prep-app/output/brecha_indig.csv"),
  discap_types = here("prep-app/output/discap_types.csv"),
  discap_subrep = here("prep-app/output/discap_subrep.csv"),
  univ_ingresos_gap = here("prep-app/output/univ_ingresos_gap.csv"),
  univ_sector = here("prep-app/output/univ_sector.csv"),
  univ_dist = here("prep-app/output/univ_dist.csv"))

# Drive folders
enoe_drive <- drive_ls(as_id("1fQDbnBduTrth9RroQUhJemw9nB5Wp6RP"))
endireh_drive <- drive_ls(as_id("1iVZscdnkjFbPOFgsIoXxwi5idNyyyRsQ"))
censo_drive <- drive_ls(as_id("1TuZc3R2Bt5eX7owFfS8-Qa6r0darYBMC"))

# Assert only one file in each Drive
stopifnot(nrow(enoe_drive) == 1,
          nrow(endireh_drive) == 1,
          nrow(censo_drive) == 1)

# Read ENOE
enoe <- drive_read_string(enoe_drive$id) %>%
  textConnection() %>%
  read.csv() %>%
  filter(edad >= 21 & sexo == "mujer" &
           # filter for data from past 5 years
           year >= (as.numeric(format(Sys.Date(), "%Y")) - 5)) %>%
  # Condense public sector categories
  mutate(sector = case_when(str_detect(sector, "Gobierno") | sector == "Poder judicial o legislativo" ~ "Gobierno",
                            sector %in% c("Gobierno", "Empresa del sector privado", "Empresa personal") == F ~ "Otro",
                            T ~ sector))

# Read ENDIREH
endireh <- drive_read_string(endireh_drive$id) %>%
  textConnection() %>%
  read.csv() %>%
  filter(edad >= 21 & sexo == "mujer") %>%
  mutate(ingreso_mensual = ifelse(ingreso_mensual >= 999997, NA_integer_, ingreso_mensual),
         sector_trabajo = ifelse(sector_trabajo %in%
                                   c("Empresa del sector privado", "Gobierno estatal o municipal",
                                     "Gobierno federal", "Empresa personal"),
                                 sector_trabajo, "Otro"),
         ocups_de_interes = str_replace_all(ocups_de_interes, "x", "a"))

# Read Censo
censo <- drive_read_string(censo_drive$id) %>%
  textConnection() %>%
  read.csv() %>%
  filter(sexo == "mujer") %>%
  mutate(perte_indigena = case_when(perte_indigena ~ "indig",
                                    perte_indigena == F ~ "no_indig",
                                    T ~ NA_character_),
         hlengua = case_when(hlengua ~ "indig",
                             hlengua == F ~ "no_indig",
                             T ~ NA_character_),
         edad_categ_10 = case_when(edad %in% 21:29 ~ "21 a 29 años",
                                   edad %in% 30:39 ~ "30 a 39 años",
                                   edad %in% 40:49 ~ "40 a 49 años",
                                   edad %in% 50:59 ~ "50 a 59 años",
                                   edad >= 60 ~ "60 años o más"))

# SECTION 1: Situación familiar
ocup_der_base <- enoe %>%
  filter(ocup_der & edad < 60 & !is.na(e_con)) %>%
  mutate(e_con = ifelse(e_con == "casado o unión libre", "Casada o en unión libre", "Soltera o divorciada"),
         maternidad = ifelse(n_hij > 0, "Con hijos", "Sin hijos"),
         edad_categ_10 = str_remove_all(edad_categ_10, " años"))

sit_fam_ingresos <- ocup_der_base %>%
  filter(ingocup > 0) %>%
  group_by(e_con, maternidad, edad_categ_10) %>%
  summarize(var_value = weighted.median(ingocup, fac)) %>%
  mutate(var_type = "ingresos")

sit_fam_informal <- ocup_der_base %>%
  group_by(e_con, maternidad, edad_categ_10) %>%
  summarize(var_value = round(100 * weighted.mean(emp_ppal == "informal", fac), 1)) %>%
  mutate(var_type = "informal")

sit_fam_particip <- enoe %>%
  filter(educ_der & edad < 60 & !is.na(e_con)) %>%
  mutate(e_con = ifelse(e_con == "casado o unión libre", "Casada o en unión libre", "Soltera o divorciada"),
         maternidad = ifelse(n_hij > 0, "Con hijos", "Sin hijos"),
         edad_categ_10 = str_remove_all(edad_categ_10, " años")) %>%
  group_by(e_con, maternidad, edad_categ_10) %>%
  summarize(var_value = round(100 * weighted.mean(pnea == "pea", fac), 1)) %>%
  mutate(var_type = "particip")

sit_fam_ejercicio <- enoe %>%
  filter(educ_der & edad < 60 & !is.na(e_con)) %>%
  mutate(e_con = ifelse(e_con == "casado o unión libre", "Casada o en unión libre", "Soltera o divorciada"),
         maternidad = ifelse(n_hij > 0, "Con hijos", "Sin hijos"),
         edad_categ_10 = str_remove_all(edad_categ_10, " años")) %>%
  group_by(e_con, maternidad, edad_categ_10) %>%
  summarize(var_value = round(100 * weighted.mean(ocup_der, fac), 1)) %>%
  mutate(var_type = "ejercicio")

sit_fam_sector <- enoe %>%
  filter(ocup_der & edad < 60 & !is.na(e_con) & !is.na(sector)) %>%
  mutate(e_con = ifelse(e_con == "casado o unión libre", "Casada o en unión libre", "Soltera o divorciada"),
         maternidad = ifelse(n_hij > 0, "Con hijos", "Sin hijos")) %>%
  group_by(e_con, maternidad, sector) %>%
  reframe(fac_sum = sum(fac)) %>%
  group_by(e_con, maternidad) %>%
  mutate(var_value = round(fac_sum / sum(fac_sum), 3),
         var_type = "sector") %>%
  mutate(sum = sum(var_value))

sit_fam_joined <- bind_rows(sit_fam_ingresos, sit_fam_informal, sit_fam_particip,
                            sit_fam_ejercicio, sit_fam_sector) %>%
  # Insufficient sample size (14) for married women 50-59 without children
  mutate(var_value =
           ifelse(var_type != "sector" & e_con == "Casada o en unión libre" & maternidad == "Sin hijos" & edad_categ_10 == "50 a 59",
                  "Muestra insuficiente", as.character(var_value)),
         min_year = min(enoe$year),
         max_year = max(enoe$year))

write_csv(sit_fam_joined, paths$sit_fam_joined)

# SECTION 2: Pertenencia étnica
# Subrepresentación (ENDIREH)
indig_subrep <- endireh %>%
  filter(!is.na(ocups_de_interes)) %>%
  group_by(ocups_de_interes) %>%
  summarize(perc_indig = round(weighted.mean(indigena, fac_muj, na.rm = T), 3),
            count = n())

indig_general <- endireh %>%
  summarize(perc_indig = round(weighted.mean(indigena, fac_muj, na.rm = T), 3),
            count = n()) %>%
  mutate(ocups_de_interes = "General")

indig_subrep <- bind_rows(indig_subrep, indig_general) %>%
  mutate(margin_error = 1.645 * sqrt(perc_indig * (1 - perc_indig) / count))

write_csv(indig_subrep, paths$indig_joined)

# Brecha salarial (Censo)
brecha_indig <- censo %>%
  filter(!is.na(hlengua)) %>%
  group_by(hlengua) %>%
  summarize(ingresos_med_indig = weighted.median(ingtrmen, factor, na.rm = T)) %>%
  pivot_wider(names_from = hlengua, values_from = ingresos_med_indig) %>%
  mutate(brecha_centavos_menos = round(100 * (1 - (indig/no_indig)), 1),
         categ_type = "general")

write_csv(brecha_indig, paths$brecha_indig)

# SECTION 3: Discapacidad
discap_base <- endireh %>%
  mutate(disc_fisica = case_when(disc_caminar | disc_brazos | disc_banar_comer ~ T,
                                 !disc_caminar & !disc_brazos & !disc_banar_comer ~ F,
                                 T ~ NA),
         disc_auditiva = case_when(disc_escuchar | disc_hablar ~ T,
                                   !disc_escuchar & !disc_hablar ~ F,
                                   T ~ NA),
         any_disc = case_when(disc_caminar | disc_brazos | disc_banar_comer |
                                disc_escuchar | disc_hablar | disc_ver |
                                disc_emocional | disc_aprender ~ T,
                              !disc_caminar & !disc_brazos & !disc_banar_comer |
                                !disc_escuchar & !disc_hablar & !disc_ver &
                                !disc_emocional & !disc_aprender ~ F,
                              T ~ NA)) %>%
  filter(!is.na(any_disc)) %>%
  mutate(year = max(year))

# Tipos de discapacidad entre las abogadas
discap_type_percs <- discap_base %>%
  select(-c(disc_caminar, disc_brazos, disc_banar_comer, disc_escuchar, disc_hablar)) %>%
  filter(ocup_der) %>%
  summarize(disc_fisica = round(weighted.mean(disc_fisica, fac_muj, na.rm = T), 3),
            disc_auditiva = round(weighted.mean(disc_auditiva, fac_muj, na.rm = T), 3),
            disc_visual = round(weighted.mean(disc_ver, fac_muj, na.rm = T), 3),
            disc_psicosocial = round(weighted.mean(disc_emocional, fac_muj, na.rm = T), 3),
            disc_aprender = round(weighted.mean(disc_aprender, fac_muj, na.rm = T), 3)) %>%
  pivot_longer(cols = starts_with("disc"), names_to = "disc_type", values_to = "perc") %>%
  mutate(x_val = c(1, 3, 5, 1.5, 4.5),
         y_val = c(rep(1, 3), rep(0, 2)),
         disc_desc = c("física", "auditiva", "visual", "psicosocial", "de aprendizaje"))

write_csv(discap_type_percs, paths$discap_types)

# Subrepresentación por ocupación
discap_subrep_ocups <- discap_base %>%
  filter(!is.na(ocups_de_interes)) %>%
  group_by(ocups_de_interes) %>%
  summarize(perc_disc = round(weighted.mean(any_disc, fac_muj, na.rm = T), 3),
         count = n())

discap_subrep_general <- discap_base %>%
  summarize(perc_disc = round(weighted.mean(any_disc, fac_muj, na.rm = T), 3),
            count = n(),
            ocups_de_interes = "General")

discap_subrep <- bind_rows(discap_subrep_ocups, discap_subrep_general) %>%
  mutate(margin_error = 1.645 * sqrt(perc_disc * (1 - perc_disc) / count))

write_csv(discap_subrep, paths$discap_subrep)

# SECTION 4: Universidad pública vs. privada
univ_dist_law <- endireh %>%
  filter(ocup_der & !is.na(escuela_tipo)) %>%
  group_by(escuela_tipo) %>%
  reframe(fac_sum = sum(fac_muj)) %>%
  ungroup() %>%
  mutate(perc = round(100 * fac_sum / sum(fac_sum), 1)) %>%
  select(-fac_sum) %>%
  mutate(dist_type = "law")

univ_dist_gen <- endireh %>%
  filter(!is.na(escuela_tipo) & escuela_nivel == "universidad") %>%
  group_by(escuela_tipo) %>%
  reframe(fac_sum = sum(fac_muj)) %>%
  ungroup() %>%
  mutate(perc = round(100 * fac_sum / sum(fac_sum), 1)) %>%
  select(-fac_sum) %>%
  mutate(dist_type = "general")

univ_dist <- bind_rows(univ_dist_law, univ_dist_gen)

write_csv(univ_dist, paths$univ_dist)

univ_ingresos_gap_law <- endireh %>%
  filter(ocup_der & !is.na(escuela_tipo)) %>%
  mutate(escuela_tipo = ifelse(escuela_tipo == "pública", "publica", "privada")) %>%
  group_by(escuela_tipo) %>%
  summarize(ingresos = weighted.median(ingreso_mensual, fac_muj, na.rm = T)) %>%
  pivot_wider(names_from = escuela_tipo, values_from = ingresos) %>%
  summarize(gap = round(100 * (1 - publica/privada), 1)) %>%
  mutate(gap_type = "law")

univ_ingresos_gap_gen <- endireh %>%
  filter(!is.na(escuela_tipo) & escuela_nivel == "universidad") %>%
  mutate(escuela_tipo = ifelse(escuela_tipo == "pública", "publica", "privada")) %>%
  group_by(escuela_tipo) %>%
  summarize(ingresos = weighted.median(ingreso_mensual, fac_muj, na.rm = T)) %>%
  pivot_wider(names_from = escuela_tipo, values_from = ingresos) %>%
  summarize(gap = round(100 * (1 - publica/privada), 1)) %>%
  mutate(gap_type = "general")

univ_ingresos_gap <- bind_rows(univ_ingresos_gap_law, univ_ingresos_gap_gen)

write_csv(univ_ingresos_gap, paths$univ_ingresos_gap)

univ_sector <- endireh %>%
  filter(ocup_der & !is.na(escuela_tipo)) %>%
  mutate(escuela_tipo = ifelse(escuela_tipo == "pública", "Universidad pública", "Universidad privada"),
         sector_trabajo = ifelse(sector_trabajo == "Empresa personal", "Otro", sector_trabajo)) %>%
  group_by(escuela_tipo, sector_trabajo) %>%
  reframe(fac_sum = sum(fac_muj)) %>%
  group_by(escuela_tipo) %>%
  mutate(perc = round(fac_sum / sum(fac_sum), 3)) %>%
  select(-fac_sum)

write_csv(univ_sector, paths$univ_sector)

# done.