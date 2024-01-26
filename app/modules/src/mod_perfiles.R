#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/app/modules/src/mod_perfiles.R

# TODO

rm(list = ls())

# Load required libraries
pacman::p_load(shiny, stringr, ggplot2, dplyr, tidyr, here, data.table, scales, ggrepel)

# Define paths
paths <- list(perfiles = here("prep-app/output/perfiles.csv"),
              income_dist = here("prep-app/output/income_dist.csv"),
              law_preval_age = here("prep-app/output/law_preval_age.csv"),
              law_preval_state = here("prep-app/output/law_preval_state.csv"),
              mxhexbin_map = here("prep-app/output/mxhexbin.csv"),
              theme = here("app/modules/src/grafs-theme-fns.R"))

# Read data ====
perfiles <- fread(paths$perfiles)

income_dist <- fread(paths$income_dist)

law_preval_age <- fread(paths$law_preval_age) %>%
  mutate(educ = factor(educ, levels = c("no_lic", "lic_non_law", "lic_law")))

law_preval_state <- fread(paths$law_preval_state) %>%
  mutate(cve_ent = as.character(sprintf("%02d", cve_ent)))

mxhexbin_map <- fread(paths$mxhexbin_map) %>%
  mutate(id = str_pad(id, 2, pad = "0"))

# Load theme & functions for graphs
source(paths$theme)

# Define constants ====
min_year_enoe <- unique(perfiles$min_year)

max_year_enoe <- unique(perfiles$max_year)

perfil_factor_order <- c(
  # Escolaridad
  "Doctorado",
  "Maestría",
  "Licenciatura",
  "Carrera técnica",
  "Preparatoria",
  "Secundaria",
  "Primaria",
  "Preescolar o menos",
  # Maternidad/estado conyugal
  "Casado o unión libre",
  "Soltero o divorciado",
  "Casada o unión libre, con hijes",
  "Casada o unión libre, sin hijes",
  "Soltera o divorciada, con hijes",
  "Soltera o divorciada, sin hijes",
  # Sector
  "ONG",
  "Escuela u hospital",
  "Gobierno",
  "Empresa del sector privado",
  "Empresa personal",
  # Edad
  "21 a 29 años",
  "30 a 39 años",
  "40 a 49 años",
  "50 a 59 años",
  "60 años o más")

most_common_perfil <- perfiles %>%
  filter(group == "Mujeres abogadas" & def_type == "ocup") %>%
  group_by(categ_type) %>%
  filter(categ_perc == max(categ_perc)) %>%
  select(categ_type, categ_name)

most_common_edad <- most_common_perfil %>%
  filter(categ_type == "edad") %>%
  pull(categ_name) %>%
  str_replace(" a ", " y ")

most_common_sector <- most_common_perfil %>%
  filter(categ_type == "sector") %>%
  pull(categ_name) %>%
  as.character()

most_common_sector <- case_when(most_common_sector == "Empresa personal" ~ "una empresa personal",
                                most_common_sector == "Empresa del sector privado" ~ "una empresa privada",
                                most_common_sector == "Gobierno" ~ "el gobierno",
                                most_common_sector == "Escuela u hospital" ~ "una escuela u hospital",
                                most_common_sector == "ONG" ~ "una ONG")

most_common_mat_e_con <- most_common_perfil %>%
  filter(categ_type == "maternidad_e_con") %>%
  pull(categ_name) %>%
  as.character() %>%
  tolower() %>%
  str_replace(",", " y")

ingresos_promedio <- income_dist %>%
  filter(group == "Mujeres abogadas") %>%
  pull(median) %>%
  unique() %>%
  scales::dollar()

ingresos_abga_vs_otras_muj <- ((ingresos_promedio %>%
                                  # Remove any non-digit characters
                                  str_remove_all("\\D") %>%
                                  as.numeric()) /
                                 (income_dist %>%
                                    filter(group == "Mujeres no abogadas") %>%
                                    pull(median) %>%
                                    unique())) %>%
  round(1)

perc_muj_gen <- income_dist %>%
  filter(group %in% c("Mujeres abogadas", "Hombres abogados")) %>%
  group_by(group) %>%
  summarize(sum_fac = sum(weights)) %>%
  pivot_wider(names_from = group, values_from = sum_fac) %>%
  summarize(perc = `Mujeres abogadas` / (`Mujeres abogadas` + `Hombres abogados`)) %>%
  pull(perc) %>%
  round(3) * 100

perc_muj_menos_13k <- income_dist %>%
  filter(ingocup < 13000 & group %in% c("Mujeres abogadas", "Hombres abogados")) %>%
  group_by(group) %>%
  summarize(sum_fac = sum(weights)) %>%
  pivot_wider(names_from = group, values_from = sum_fac) %>%
  summarize(perc = `Mujeres abogadas` / (`Mujeres abogadas` + `Hombres abogados`)) %>%
  pull(perc) %>%
  round(3) * 100

perc_muj_mas_13k <- income_dist %>%
  filter(ingocup >= 13000 & group %in% c("Mujeres abogadas", "Hombres abogados")) %>%
  group_by(group) %>%
  summarize(sum_fac = sum(weights)) %>%
  pivot_wider(names_from = group, values_from = sum_fac) %>%
  summarize(perc = `Mujeres abogadas` / (`Mujeres abogadas` + `Hombres abogados`)) %>%
  pull(perc) %>%
  round(3) * 100

# 1 in how many 30-35 y.o. women are lawyers?
law_preval_muj_general <- (1 / (law_preval_age %>%
                                  filter(sexo == "mujer" & type == "all" &
                                           edad %in% 30:35 & educ == "lic_law") %>%
                                  summarize(weighted_mean = weighted.mean(perc, weights))) %>% pull()) %>%
  round(-1)

# 1 in how many 30-35 y.o. men are lawyers?
law_preval_hom_general <- (1 / (law_preval_age %>%
                                  filter(sexo == "hombre" & type == "all" &
                                           edad %in% 30:35 & educ == "lic_law") %>%
                                  summarize(weighted_mean = weighted.mean(perc, weights))) %>% pull()) %>%
  round(-1)

# 1 in how many 30-35 y.o. women *with college education* are lawyers?
law_preval_muj_lic <- (1 / ((law_preval_age %>%
                               filter(sexo == "mujer" & type == "just_lic" &
                                        edad %in% 30:35 & educ == "lic_law") %>%
                               summarize(weighted_mean = weighted.mean(perc, weights)) %>% pull()) /
                              (law_preval_age %>%
                                 filter(sexo == "mujer" & type == "just_lic" & edad %in% 30:35) %>%
                                 group_by(edad) %>%
                                 mutate(perc_w_lic = sum(perc)) %>%
                                 ungroup() %>%
                                 summarize(weighted_mean = weighted.mean(perc_w_lic, weights)) %>% pull()))) %>%
  round(0)

# 1 in how many 30-35 y.o. men *with college education* are lawyers?
law_preval_hom_lic <- (1 / ((law_preval_age %>%
                               filter(sexo == "hombre" & type == "just_lic" &
                                        edad %in% 30:35 & educ == "lic_law") %>%
                               summarize(weighted_mean = weighted.mean(perc, weights)) %>% pull()) /
                              (law_preval_age %>%
                                 filter(sexo == "hombre" & type == "just_lic" & edad %in% 30:35) %>%
                                 group_by(edad) %>%
                                 mutate(perc_w_lic = sum(perc)) %>%
                                 ungroup() %>%
                                 summarize(weighted_mean = weighted.mean(perc_w_lic, weights)) %>% pull()))) %>%
  round(0)

map_centroids <- mxhexbin_map %>%
  group_by(id) %>%
  summarize(max_long = max(long),
            max_lat = max(lat),
            min_long = min(long),
            min_lat = min(lat),
            centroid_long = (max_long + min_long) / 2,
            centroid_lat = (max_lat + min_lat) / 2)

caption_enoe_default <- paste0("Fuente: Elaboración propia con datos de la ENOE ", min_year_enoe, " - ", max_year_enoe, ".")

caption_enoe_mat_e_econ <- paste0(caption_enoe_default, "\n",
                                  "Nota: La ENOE no incluye preguntas sobre la paternidad de los hombres.")

# Define functions ====
plot_perfiles <- function(def_choice, categ_choice, group_choice) {
  caption_text <- ifelse(categ_choice == "maternidad_e_con", caption_enoe_mat_e_econ, caption_enoe_default)

  fill_text <- case_when(categ_choice == "maternidad_e_con" ~ "Maternidad y estado conyugal",
                         categ_choice == "sector" ~ "Sector de trabajo",
                         categ_choice == "escolaridad" ~ "Escolaridad",
                         categ_choice == "edad" ~ "Edad")

  plot <- perfiles %>%
    filter(def_type == def_choice & categ_type == categ_choice & group %in% group_choice) %>%
    mutate(categ_name = factor(categ_name, levels = perfil_factor_order),
           group = case_when(group == "Mujeres no abogadas"
                             ~ "Mujeres no abogadas\ncon estudios universitarios",
                             T ~ group)) %>%
    ggplot(data = ., aes(x = group, y = categ_perc, fill = categ_name)) +
    geom_bar(position="fill", stat="identity") +
    labs(
      title = paste0("Distribución de ", tolower(fill_text)),
      y = "", x = "", fill = fill_text, caption = caption_text) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = percent) +
    tema_abogadas

  if(categ_choice == "maternidad_e_con"){
    plot <- plot + scale_fill_manual(values = pal_mat_e_con)
  } else {
    plot <- plot + scale_fill_manual(values = pal_5)
  }

  return(plot)
}

plot_incomes <- function(group_choice){
  income_dist %>%
    filter(group %in% group_choice) %>%
    mutate(group = case_when(group == "Mujeres no abogadas"
                             ~ "Mujeres no abogadas\ncon estudios universitarios",
                             T ~ group)) %>%
    ggplot(data = ., aes(x = ingocup, fill = group)) +
    geom_density(aes(weight = weights),
                 alpha = 0.5, bw = 2000, bounds = c(0, 75000), color = NA) +
    geom_vline(
      data = . %>% distinct(group, median),
      aes(xintercept = median, color = group),
      linetype = "dashed", size = 1, show.legend = FALSE) +
    geom_label_repel(
      data = . %>% distinct(group, median),
      aes(x = median, y = 0.000055, color = group,
          label = paste0("Promedio: $", comma(median))),
      label.size = 0.5, hjust = 0.5, vjust = 0.5, fill = "#faf4e8",
      show.legend = FALSE, fontface = "bold") +
    labs(title = "Distribución de ingresos mensuales",
         x = "Ingresos mensuales (pesos)", y = "Frecuencia",
         fill = "Grupo", caption = caption_enoe_default) +
    coord_cartesian(xlim = c(0, 65000)) +
    scale_x_continuous(labels = function(x) paste0("$", scales::comma(x)),
                       breaks = seq(0, 65000, by = 10000)) +
    tema_abogadas +
    scale_fill_manual(values = pal_abogadas_ocup) +
    scale_color_manual(values = pal_abogadas_ocup)
}

map_preval <- function(def_choice, sexo_choice){

  df_map <- law_preval_state %>%
    filter(def_type == def_choice & sexo == sexo_choice) %>%
    left_join(mxhexbin_map, by = c("cve_ent" = "id"), multiple = "all") %>%
    left_join(map_centroids, by = c("cve_ent" = "id"))

  stopifnot(nrow(df_map) == nrow(mxhexbin_map),
            all(!is.na(df_map$centroid_long)),
            all(!is.na(df_map$centroid_lat)))

  plot_title <- case_when(def_choice == "ocup" & sexo_choice == "mujer"
                          ~ "¿Qué porcentaje de las mujeres ejercen como abogadas?",
                          def_choice == "ocup" & sexo_choice == "hombre"
                          ~ "¿Qué porcentaje de los hombres ejercen como abogados?",
                          def_choice == "ocup" & sexo_choice == "general"
                          ~ "¿Qué porcentaje de la población ejerce como abogades?",
                          def_choice == "educ" & sexo_choice == "mujer"
                          ~ "¿Qué porcentaje de las mujeres estudiaron Derecho?",
                          def_choice == "educ" & sexo_choice == "hombre"
                          ~ "¿Qué porcentaje de los hombres estudiaron Derecho?",
                          def_choice == "educ" & sexo_choice == "general"
                          ~ "¿Qué porcentaje de la población estudió Derecho?")


  ggplot(df_map, aes(x = long, y = lat, group=group)) +
    geom_polygon(aes(fill = perc), color = "#242223") +
    labs(
      title = plot_title,
      fill = "", caption = paste0(caption_enoe_default, "\n",
                                  "Se considera únicamente la población con 21 años o más.")) +
    tema_abogadas +
    scale_fill_gradient(low = "#fcf2e3", high = "#e39017") +
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none")
}

# UI ====
# screen_size_test_ui <- function(id){
#   ns <- NS(id)
#
#   verbatimTextOutput(ns("screen_size_test"))
# }

# SECTION 1: PROFILE DISTRIBUTIONS

most_common_profile_text_ui <- function(id){
  ns <- NS(id)

  htmlOutput(ns("most_common_profile_text"))
}

select_lawyer_def_ui <- function(id){
  ns <- NS(id)

  selectInput(ns("def_type"), "Definición:", choices = c("Por ocupación" = "ocup", "Por formación" = "educ"))
}

select_var_interest_ui <- function(id){
  ns <- NS(id)

  selectInput(ns("var_interest"), "Variable de interés:", choices = c("Edad" = "edad", "Maternidad y estado conyugal" = "maternidad_e_con", "Sector de trabajo" = "sector"))
}

select_comp_groups_perfiles_ui <- function(id){
  ns <- NS(id)

  checkboxGroupInput(ns("groups_perfiles"), "Grupos de comparación:",
                     choices = c("Mujeres abogadas",
                                 "Mujeres no abogadas",
                                 "Hombres abogados"),
                     selected = c("Mujeres abogadas"))
}

perfiles_dist_plot_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("perfiles_dist_plot"))
}

# SECTION 2: INCOME
income_intro_text_ui <- function(id){
  ns <- NS(id)

  htmlOutput(ns("income_intro_text"))
}

select_comp_groups_income_ui <- function(id){
  ns <- NS(id)

  checkboxGroupInput(ns("groups_income"),
                     label = "Grupos de comparación:",
                     choices = c("Mujeres abogadas",
                                 "Mujeres no abogadas",
                                 "Hombres abogados"),
                     selected = c("Mujeres abogadas"))
}

income_dist_plot_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("income_dist_plot"))
}

women_income_text_ui <- function(id){
  ns <- NS(id)

  htmlOutput(ns("women_income_text"))
}

income_13k_text_ui <- function(id){
  ns <- NS(id)

  htmlOutput(ns("income_13k_text"))
}

# SECTION 3: PREVALENCE OF LAW AS MAJOR
# By age
law_preval_age_plot_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("law_preval_age"))
}

law_preval_text_ui <- function(id){
  ns <- NS(id)

  htmlOutput(ns("law_preval_text"))
}

# By state
select_lawyer_def_map_ui <- function(id){
  ns <- NS(id)

  selectInput(ns("def_type_map"), "Definición:", choices = c("Por ocupación" = "ocup", "Por formación" = "educ"))
}

select_sexo_map_ui <- function(id){
  ns <- NS(id)

  selectInput(ns("sexo_map"), "Población de interés:", choices = c("Sólo mujeres" = "mujer",
                                                                   "Sólo hombres" = "hombre",
                                                                   "Población en general" = "general"))
}

law_preval_map_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("law_preval_map"), height = "100%")
}

# Server ====
perfiles_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns(id)

    plotWidth <- reactive({session$clientData[["output_perfiles-perfiles_dist_plot_width"]]})

    text_size_to_use <- reactive({
      if (plotWidth() > 586) {
        laptop_text_sizes
      } else {
        mobile_text_sizes
      }
    })

    # output$screen_size_test <- renderPrint({
    #   paste0("El ancho de la pantalla es: ", plotWidth(),
    #          text_size_to_use())
    # })

    # SECTION 1: PROFILE DISTRIBUTIONS
    output$most_common_profile_text <- renderUI({
      paste0("Si quisiéramos describir a la abogada 'promedio' en México, ",
             "estaríamos hablando de una mujer entre ", most_common_edad, ", ", most_common_mat_e_con, ", ",
             "que cuenta con una escolaridad de licenciatura y trabaja en ", most_common_sector, ".")
    })

    output$perfiles_dist_plot <- renderPlot({
      plot <- plot_perfiles(input$def_type, input$var_interest, input$groups_perfiles) +
        text_size_to_use()

      if (plotWidth() <= 586) {
        plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        if (input$var_interest == "edad"){
          plot <- plot +
            guides(fill=guide_legend(ncol=3, byrow=TRUE))
        }
        if (input$var_interest == "sector"){
          plot <- plot +
            guides(fill=guide_legend(ncol=2, byrow=TRUE))
        }
        if (input$var_interest == "maternidad_e_con"){
          legend_vals <- perfiles %>%
            filter(categ_type == input$var_interest & group %in% input$groups_perfiles) %>%
            pull(categ_name) %>%
            unique() %>%
            str_replace_all(", ", ",\n") %>%
            str_replace_all(" o ", " o\n")

          pal_mat_e_con_split <- pal_mat_e_con
          names(pal_mat_e_con_split) <- names(pal_mat_e_con_split) %>%
            str_replace_all(", ", ",\n") %>%
            str_replace_all(" o ", " o\n")
          pal_mat_e_con_split <- pal_mat_e_con_split[names(pal_mat_e_con_split) %in% legend_vals]
          names(pal_mat_e_con_split) <- NULL

          plot <- plot +
            guides(fill=guide_legend(ncol=3, bycol=TRUE)) +
            scale_fill_manual(labels = legend_vals, values = pal_mat_e_con_split) +
            theme(legend.justification = c(0, 0))
        }
      }
      
      return(plot)
      dev.off()
    })

    # SECTION 2: INCOME
    output$income_intro_text <- renderUI({
      HTML(paste0("Las mujeres que se dedican a ser abogadas ganan en promedio ",
                  span(paste0(ingresos_promedio, " al mes. "), style = "font-weight: bold;"),
                  "Abajo se puede explorar la distribución de ingresos de las abogadas, así como los ingresos de las mujeres no abogadas con estudios superiores y los hombres abogados."))
    })

    output$income_dist_plot <- renderPlot({
      plot <- plot_incomes(input$groups_income) +
        text_size_to_use() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

      if (plotWidth() <= 586) {
        plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          guides(fill=guide_legend(ncol=2, byrow=TRUE))
      }
      
      return(plot)
      dev.off()
    })

    output$women_income_text <- renderUI({
      HTML(paste0("En comparación con las mujeres no abogadas, las abogadas se parecen mucho más a los abogados en términos de sus ingresos mensuales: las abogadas ganan en promedio ",
                  span(paste0(ingresos_abga_vs_otras_muj, " veces más "), style = "font-weight: bold;"),
                  "que las mujeres con estudios universitarios en otras profesiones."))
    })

    output$income_13k_text <- renderUI({
      HTML(paste0("Para ingresos menores a $13,000 al mes (aproximadamente el promedio de los ingresos de los abogados hombres), ",
                  "las mujeres están sobrerrepresentadas, mientras que para salarios mayores a esta cantidad, se observa lo contrario. ",
                  "Las mujeres representan ",
                  span(paste0(perc_muj_menos_13k, "% "), style = "font-weight: bold;"),
                  "de les abogades que ganan menos de $13,000 al mes y ",
                  span(paste0(perc_muj_mas_13k, "% "), style = "font-weight: bold;"),
                  "de les abogades que ganan igual o más que esa cantidad."))
    })

    # SECTION 3: PREVALENCE OF LAW
    output$law_preval_age <- renderPlot({
      plot_title_unwrapped <- "Porcentaje de personas con estudios superiores que estudiaron Derecho"

      plot_title <- ifelse(plotWidth() > 586, plot_title_unwrapped, str_wrap(plot_title_unwrapped, 48))
      
      plot <- law_preval_age %>%
        filter(type == "just_lic") %>%
        ggplot(data = ., aes(x = edad, y = perc, fill = educ)) +
        geom_area() +
        facet_wrap(~sexo, nrow = 1,
                   labeller = labeller(sexo = c("hombre" = "Hombres",
                                                "mujer" = "Mujeres"))) +
        labs(
          title = plot_title,
          subtitle = "Por edad",
          x = paste0("Edad\n", "(Mayores --> jóvenes)"), y = "",
          caption = caption_enoe_default) +
        coord_cartesian(xlim = c(80, 25)) +
        scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = percent) +
        scale_x_reverse() +
        tema_abogadas +
        text_size_to_use() +
        scale_fill_manual(labels = c("lic_law" = "Estudió Derecho",
                                     "lic_non_law" = "Estudió otra carrera"), name = NULL,
                          values = c("lic_law" = pal_2[1],
                                     "lic_non_law" = pal_2[2]))
      
      return(plot)
      dev.off()
    })

    output$law_preval_text <- renderUI({
      HTML(paste0("Hoy en día aproximadamente ",
                  span(paste0("1 de cada ", law_preval_muj_general, " mujeres "), style = "font-weight: bold;"),
                  "entre 30 y 35 años (",
                  span(paste0("1 de cada ", law_preval_muj_lic), style = "font-weight: bold;"),
                  " con licenciatura) y ",
                  span(paste0("1 de cada ", law_preval_hom_general, " hombres "), style = "font-weight: bold;"),
                  " de este rango de edad (",
                  span(paste0("1 de cada ",  law_preval_hom_lic), style = "font-weight: bold;"),
                  " con licenciatura) son abogades por formación."))
    })

    output$law_preval_map <- renderPlot({
      text_size <- ifelse(plotWidth() > 586, 6, 3)

      font_face <- ifelse(plotWidth() > 586, "bold", "plain")
      
      plot <- map_preval(def_choice = input$def_type_map, sexo_choice = input$sexo_map) +
        geom_text(aes(x = centroid_long, y = centroid_lat,
                      label = paste0(state_abbr, "\n",
                                     perc, "%")),
                  color = "#242223", fontface = font_face,
                  family = "Roboto", size = text_size) +
        text_size_to_use() +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              legend.position = "none")
      
      return(plot)
      dev.off()
    })

  })
}

# Demo ====
app_demo <- function(){
  ui <- fluidPage(
    # screen_size_test_ui("perfiles"),
    select_lawyer_def_ui("perfiles"),
    select_var_interest_ui("perfiles"),
    select_comp_groups_perfiles_ui("perfiles"),
    perfiles_dist_plot_ui("perfiles"),
    select_comp_groups_income_ui("perfiles"),
    income_intro_text_ui("perfiles"),
    income_dist_plot_ui("perfiles"),
    income_13k_text_ui("perfiles"),
    select_lawyer_def_map_ui("perfiles"),
    select_sexo_map_ui("perfiles"),
    law_preval_map_ui("perfiles"),
    law_preval_age_plot_ui("perfiles")
  )

  server <- function(input, output, session) {
    perfiles_server("perfiles")
  }

  shinyApp(ui, server)
}

# done.
