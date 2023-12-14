#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/prep-app/src/mod_violencia_prep.R

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(stringr, ggplot2, dplyr, here, broom, googledrive)

# Files
paths <- list(cifras_resumen = here("prep-app/output/violencia_abogadas.csv"),
              violencia_abgas_desag = here("prep-app/output/violencia_abgas_desag.csv"),
              violencia_ocups = here("prep-app/output/violencia_ocups.csv"))

# Drive folder
endireh_drive <- drive_ls(as_id("1iVZscdnkjFbPOFgsIoXxwi5idNyyyRsQ"))

# Read ENDIREH
endireh <- drive_read_string(endireh_drive$id) %>%
  textConnection() %>%
  read.csv() %>%
  filter(edad >= 21 & sexo == "mujer")

# Define indicators of interest
indicadores <- c("lab_piropos", "lab_tocar", "lab_ignorar_mujer", "anio_pagar_menos",
                 "anio_menos_oport_asc", "anio_limiar_des_prof", "embzo_prueba_trab", "embzo_desp")

# SECTION 1: Cifras de resumen
violencia_abogadas <- endireh %>% filter(ocup_der) %>%
  summarize(across(.cols = all_of(indicadores),
            .fns = ~round(100 * weighted.mean(., na.rm = T, fac_muj), 1))) %>%
  pivot_longer(cols = everything(),
               names_to = "indicador", values_to = "valor") %>%
  mutate(indicador = factor(indicador, levels = c("anio_pagar_menos", "anio_menos_oport_asc",
                                                  "anio_limiar_des_prof", "lab_ignorar_mujer",
                                                  "lab_piropos", "lab_tocar",
                                                  "embzo_prueba_trab", "embzo_desp"))) %>%
  arrange(indicador) %>%
  mutate(y = c(rep(4, 2), rep(3, 2), rep(2, 2), rep(1, 2)),
         x = rep(1:2, 4))

write_csv(violencia_abogadas, paths$cifras_resumen)

# SECTION 2: Comparaciones entre las abogadas
by_age <- endireh %>% filter(ocup_der) %>%
  filter(edad < 60) %>%
  group_by(edad_categ_10) %>%
  summarize(across(indicadores,
                   ~ round(100 * weighted.mean(., na.rm = T, fac_muj), 1)),
            count = n()) %>%
  mutate(grouped_by = "edad") %>%
  rename(group = edad_categ_10)

by_e_con <- endireh %>%
  filter(ocup_der & !is.na(e_con)) %>%
  group_by(e_con) %>%
  summarize(across(indicadores,
                   ~ round(100 * weighted.mean(., na.rm = T, fac_muj), 1)),
            count = n()) %>%
  mutate(grouped_by = "e_con") %>%
  rename(group = e_con)

by_sector <- endireh %>%
  filter(ocup_der & !is.na(sector_trabajo)) %>%
  group_by(sector_trabajo) %>%
  mutate(sector_trabajo = ifelse(n() < 30, "Otro", sector_trabajo)) %>%
  group_by(sector_trabajo) %>%
  summarize(across(indicadores,
                   ~ round(100 * weighted.mean(., na.rm = T, fac_muj), 1)),
            count = n()) %>%
  mutate(grouped_by = "sector") %>%
  rename(group = sector_trabajo)

violencia_abogadas_desag <- bind_rows(by_age, by_e_con, by_sector) %>%
  pivot_longer(cols = -c("group", "grouped_by", "count"),
               names_to = "indicador", values_to = "valor") %>%
  mutate(indicador_desc = case_when(indicador == "anio_pagar_menos" ~ "Le han pagado menos que a un hombre en el mismo puesto",
                                    indicador == "anio_menos_oport_asc" ~ "Ha tenido menos oportunidad que un hombre para ascender",
                                    indicador == "anio_limiar_des_prof" ~ "Le han limitado su desarrollo profesional para favorecer a un hombre",
                                    indicador == "lab_ignorar_mujer" ~ "La han ignorado por ser mujer",
                                    indicador == "lab_piropos" ~ "Le han dicho piropos groseros u ofensivos",
                                    indicador == "lab_tocar" ~ "La han manoseado, besado o tocado sin su consentimiento",
                                    indicador == "embzo_prueba_trab" ~ "Le han pedido una prueba de embarazo como requisito para trabajar",
                                    indicador == "embzo_desp" ~ "La despidieron o no le renovaron el contrato por embarazarse"),
         margin_error = 1.645 * 100 * sqrt((valor/100) * ( 1 - valor/100) / count),
         max_year = max(endireh$year),
         min_year = min(endireh$year))

write_csv(violencia_abogadas_desag, paths$violencia_abgas_desag)

# SECTION 3: Comparaciones entre ocupaciones
violencia_ocup <- endireh %>%
  filter(!is.na(ocups_de_interes)) %>%
  group_by(ocups_de_interes) %>%
  summarize(across(indicadores,
                   ~ round(100 * weighted.mean(., na.rm = T, fac_muj), 1)),
            count = n()) %>%
  pivot_longer(cols = -c("ocups_de_interes", "count"),
               names_to = "indicador", values_to = "valor") %>%
  mutate(indicador_desc = case_when(indicador == "anio_pagar_menos" ~ "Le han pagado menos que a un hombre en el mismo puesto",
                                    indicador == "anio_menos_oport_asc" ~ "Ha tenido menos oportunidad que un hombre para ascender",
                                    indicador == "anio_limiar_des_prof" ~ "Le han limitado su desarrollo profesional para favorecer a un hombre",
                                    indicador == "lab_ignorar_mujer" ~ "La han ignorado por ser mujer",
                                    indicador == "lab_piropos" ~ "Le han dicho piropos groseros u ofensivos",
                                    indicador == "lab_tocar" ~ "La han manoseado, besado o tocado sin su consentimiento",
                                    indicador == "embzo_prueba_trab" ~ "Le han pedido una prueba de embarazo como requisito para trabajar",
                                    indicador == "embzo_desp" ~ "La despidieron o no le renovaron el contrato por embarazarse"),
         margin_error = 1.645 * 100 * sqrt((valor/100) * ( 1 - valor/100) / count),
         ocups_de_interes = str_replace_all(ocups_de_interes, "x", "a"))

write_csv(violencia_ocup, paths$violencia_ocups)

# done.
