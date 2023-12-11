#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/prep-app/src/mod_cuidados_prep.R

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(stringr, dplyr, here, ggplot2, spatstat, googledrive)

# Files
paths <- list(cifras_resumen = here("prep-app/output/cuidados_resumen.csv"),
              cuidados_ingresos = here("prep-app/output/cuidados_ingresos.csv"),
              cuidados_particip = here("prep-app/output/cuidados_particip.csv"))

# Drive folder
enoe_drive <- drive_ls(as_id("1fQDbnBduTrth9RroQUhJemw9nB5Wp6RP"))

# Read ENOE
enoe <- drive_read_string(enoe_drive$id) %>% 
  textConnection() %>%
  read.csv() %>% 
  filter(edad >= 18 &
           # filter for data from past 5 years
           year >= (as.numeric(format(Sys.Date(), "%Y")) - 5))

# SECTION 1: Cifras de resumen
cifras_resumen <- enoe %>% 
  mutate(group = case_when(sexo == "mujer" & ocup_der ~ "Abogadas\n", 
                           sexo == "mujer" & !ocup_der  ~ "Mujeres no abogadas con estudios universitarios",
                           sexo == "hombre" & ocup_der ~ "Abogados hombres\n"),
         # Replace NAs with F
         across(.cols = c(acceso_guarderia, acceso_tiempo_cuid_crianza),
                .fns = ~ifelse(is.na(.), F, .))) %>% 
  filter(!is.na(group) & pnea == "pea" &
           escolaridad_aprobado %in% c("Licenciatura", "Maestría", "Doctorado")) %>% 
  group_by(group) %>% 
  reframe(guarderia = round(weighted.mean(acceso_guarderia, fac) * 100, 1), 
          tiempo_cuid =  round(weighted.mean(acceso_tiempo_cuid_crianza, fac) * 100, 1),
          trab_dom = round(weighted.mean(hog_contrata_trab_dom, fac) * 100, 1),
          cuidados_thrs = weighted.median(cuidados_thrs, fac, na.rm = T)) %>% 
  mutate(
    across(.cols = c(guarderia, tiempo_cuid, trab_dom, cuidados_thrs),
           .fns = ~as.character(.)),
    across(.cols = c(guarderia, tiempo_cuid, trab_dom),
                .fns = ~paste0(., "%"))) %>%
  pivot_longer(cols = guarderia:cuidados_thrs, names_to = "var_name", values_to = "var_val")

write_csv(cifras_resumen, paths$cifras_resumen)

# SECTION 2: cuidados vs. labor market outcomes
horas_cuidado_hombres <- enoe %>% 
  filter(sexo == "hombre") %>% 
  group_by(hogar_hash) %>% 
  summarize(cuidados_thrs_hombres = weighted.mean(cuidados_thrs, fac, na.rm = T)) %>% 
  select(hogar_hash, cuidados_thrs_hombres)

cuidados_vs_ingresos <- enoe %>% 
  # Filter to include only women lawyers (by occupation)
  filter(sexo == "mujer" & edad >= 21 & ocup_der & ingocup > 0) %>% 
  left_join(horas_cuidado_hombres, by = "hogar_hash") %>% 
  select(cuidados_thrs, cuidados_thrs_hombres, fac, ingocup) %>% 
  mutate(min_year_enoe = min(enoe$year),
         max_year_enoe = max(enoe$year))

cuidados_vs_particip_abga <- enoe %>% 
  filter(sexo == "mujer" & edad >= 21 & educ_der) %>% 
  mutate(pea = ifelse(pnea == "pea", "Participan en el mercado laboral", "No participan en el mercado laboral")) %>% 
  group_by(pea, cuidados_thrs) %>% 
  summarize(weights = sum(fac)) %>% 
  mutate(cuidados_var_name = "cuidados_thrs") %>% 
  rename(cuidados_val = cuidados_thrs)

cuidados_vs_particip_hombres <- enoe %>% 
  filter(sexo == "mujer" & edad >= 21 & educ_der) %>% 
  left_join(horas_cuidado_hombres, by = "hogar_hash") %>% 
  mutate(pea = ifelse(pnea == "pea", "Participan en el mercado laboral", "No participan en el mercado laboral")) %>% 
  group_by(pea, cuidados_thrs_hombres) %>% 
  summarize(weights = sum(fac)) %>% 
  mutate(cuidados_var_name = "cuidados_thrs_hombres") %>% 
  rename(cuidados_val = cuidados_thrs_hombres)

cuidados_vs_particip <- bind_rows(cuidados_vs_particip_abga,
                                  cuidados_vs_particip_hombres)

write_csv(cuidados_vs_ingresos, paths$cuidados_ingresos)

write_csv(cuidados_vs_particip, paths$cuidados_particip)
  
# done.
