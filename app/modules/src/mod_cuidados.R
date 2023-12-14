#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/app/modules/src/mod_cuidados.R

# TODO

# Packages
pacman::p_load(shiny, stringr, dplyr, here, data.table, ggplot2, spatstat)

# Files
paths <- list(cifras_resumen = here("prep-app/output/cuidados_resumen.csv"),
              cuidados_ingresos = here("prep-app/output/cuidados_ingresos.csv"),
              cuidados_particip = here("prep-app/output/cuidados_particip.csv"),
              theme = here("app/modules/src/grafs-theme-fns.R"))

# Read data
cifras_resumen <- fread(paths$cifras_resumen) %>%
  mutate(var_name = factor(var_name, levels = c("cuidados_thrs",
                                                "trab_dom",
                                                "tiempo_cuid",
                                                "guarderia")))

cuidados_ingresos <- fread(paths$cuidados_ingresos)

cuidados_particip <- fread(paths$cuidados_particip)

# Source theme
source(paths$theme)

# Define constants
min_year_enoe <- cuidados_ingresos %>% select(min_year_enoe) %>% unique() %>% pull()

max_year_enoe <- cuidados_ingresos %>% select(max_year_enoe) %>% unique() %>% pull()

thrs_particip <- cuidados_particip %>%
  filter(pea == "Participan en el mercado laboral" & cuidados_var_name == "cuidados_thrs") %>%
  summarize(weighted_mean = weighted.mean(cuidados_val, weights)) %>%
  pull %>%
  round(1)

thrs_no_particip <- cuidados_particip %>%
  filter(pea == "No participan en el mercado laboral" & cuidados_var_name == "cuidados_thrs") %>%
  summarize(weighted_mean = weighted.mean(cuidados_val, weights)) %>%
  pull %>%
  round(1)

caption_enoe_default <- paste0("Fuente: Elaboración propia con datos de la ENOE ", min_year_enoe, " - ", max_year_enoe, ".")

# Define functions ====
plot_cuidados_vs_outcomes <- function(resultado_var_choice, cuidados_var_choice, mobile){
  # Set cuidados axes titles
  if(cuidados_var_choice == "cuidados_thrs"){
    x_title <- "Promedio de horas semanales dedicadas al trabajo de cuidados por las abogadas"

    particip_x_limit <- 62
  }

  if(cuidados_var_choice == "cuidados_thrs_hombres"){
    x_title <- "Promedio de horas semanales dedicadas al trabajo de cuidados por cada hombre adulto en los hogares de las abogadas"

    particip_x_limit <- 57
  }

  x_title <- ifelse(mobile, str_wrap(x_title, 50), x_title)

  # Create plots according to response var
  if (resultado_var_choice == "ingresos"){
    plot <- ggplot(data = cuidados_ingresos, aes_string(x = cuidados_var_choice, y = "ingocup")) +
      geom_point(aes(size = fac), color = "#242223") +
      geom_smooth(aes(weight = fac), method = "lm", color = "#F1A0DD") +
      labs(x = x_title,
           y = "Ingresos mensuales de las abogadas",
           title = ifelse(mobile,
                          "Ingresos de las abogadas según su carga de\ntrabajo de cuidados",
                          "Ingresos de las abogadas según su carga de trabajo de cuidados"),
           caption = paste0(caption_enoe_default,
                            "\nEl tamaño de los puntos corresponde al valor del factor de ponderación.",
                            "\nSe consideran únicamente abogadas por profesión que trabajen por remuneración.")) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 100000)) +
      tema_abogadas +
      theme(legend.position = "none",
            axis.text.y = element_text(angle = ifelse(mobile, 90, 0)))
    return(plot)
  }

  if (resultado_var_choice == "particip"){
    plot <- cuidados_particip %>%
      filter(cuidados_var_name == cuidados_var_choice) %>%
      ggplot(data = ., aes(x = cuidados_val, fill = pea)) +
      geom_density(aes(weight = weights),
                   alpha = 0.5, bw = 5, bounds = c(0, 70), color = NA) +
      labs(x = x_title, y = "Frecuencia",
           title = ifelse(mobile,
                          "Distribución de horas dedicadas a los cuidados\npor las abogadas",
                          "Distribución de horas dedicadas a los cuidados por las abogadas"),
           fill = "Abogadas por formación que...",
           caption = caption_enoe_default) +
      scale_x_continuous(breaks = seq(0, 70, 10)) +
      scale_fill_manual(values = pal_2) +
      tema_abogadas

    return(plot)
  }

}

# UI ====
screen_size_test_ui <- function(id){
  ns <- NS(id)

  verbatimTextOutput(ns("screen_size_test"))
}

infographic_cuidados_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("plot_resumen_cuidados"))
}

select_cuidados_resultado_ui <- function(id){
  ns <- NS(id)

  selectInput(ns("resultado_var"), "Selecciona un resultado laboral de las abogadas:",
              choices = c("Ingresos" = "ingresos",
                          "Participación en el mercado laboral" = "particip"),
              width = 450)
}

select_cuidados_indicador_ui <- function(id){
  ns <- NS(id)

  selectInput(ns("cuidados_var"), "Selecciona un indicador de cuidados:",
              choices = c("Horas dedicadas a los cuidados por las abogadas" = "cuidados_thrs",
                          "Horas dedicadas a los cuidados por los hombres en sus hogares" = "cuidados_thrs_hombres"),
              width = 450)
}

plot_cuidados_vs_outcomes_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("plot_cuidados_vs_outcomes"))
}

text_thrs_particip_ui <- function(id){
  ns <- NS(id)

  htmlOutput(ns("hallazgos_thrs_particip"))
}

# Server ====
cuidados_server <- function(id){
  moduleServer(id, function(input, output, session) {
    plotWidth <- reactive({session$clientData[["output_cuidados-plot_cuidados_vs_outcomes_width"]]})

    text_size_to_use <- reactive({
      if (plotWidth() > 586) {
        laptop_text_sizes
      } else {
        mobile_text_sizes
      }
    })

    output$screen_size_test <- renderPrint({
      paste0("El ancho de la pantalla es: ", plotWidth(),
             text_size_to_use())
    })

    output$plot_resumen_cuidados <- renderPlot({
      mobile <- ifelse(plotWidth() > 586, FALSE, TRUE)

      muj_no_abg_title <- ifelse(mobile,
                                 "Mujeres no abogadas\ncon estudios\nuniversitarios",
                                 "Mujeres no abogadas\ncon estudios universitarios")

      df <- cifras_resumen %>%
        mutate(group = case_when(group == "Mujeres no abogadas con estudios universitarios" ~ muj_no_abg_title,
                                 TRUE ~ group),
               group = factor(group, levels = c("Abogados hombres\n",
                                                "Abogadas\n",
                                                muj_no_abg_title)))

      ggplot(data = df, aes(x = group, y = as.numeric(var_name))) +
        geom_text(aes(label = var_val), size = ifelse(mobile, 8, 20), fontface = "bold", color = "#242223", family = "Roboto Slab") +
        geom_text(aes(label = "tienen acceso a guarderías a través de su empleo",
                      x = "Abogadas\n", y = 4),
                  size = ifelse(mobile, 3, 5), vjust = 4, family = "Roboto", color = "#242223") +
        geom_text(aes(label = ifelse(mobile,
                                     "tienen acceso a tiempo para cuidados maternos o paternos\na través de su empleo",
                                     "tienen acceso a tiempo para cuidados maternos o paternos a través de su empleo"),
                      x = 2, y = 3),
                  size = ifelse(mobile, 3, 5), vjust = ifelse(mobile, 2, 4), family = "Roboto", color = "#242223") +
        geom_text(aes(label = "viven en hogares que contratan trabajadoras domésticas",
                      x = "Abogadas\n", y = 2),
                  size = ifelse(mobile, 3, 5), vjust = 4, family = "Roboto", color = "#242223") +
        geom_text(aes(label = "promedio de horas semanales dedicadas al trabajo de cuidados",
                      x = "Abogadas\n", y = 1),
                  size = ifelse(mobile, 3, 5), vjust = 4, family = "Roboto", color = "#242223") +
        geom_hline(yintercept = c(1.4, 2.4, 3.4), linewidth = 1, color = "#242223") +
        labs(caption = paste0(caption_enoe_default,
                              "\nSe consideran únicamente personas que participan en el mercado laboral.",
                              "\nAbogades son les que declaran ser abogades por ocupación.")) +
        coord_cartesian(ylim = c(0.5, 4.25)) +
        scale_x_discrete(position = "top") +
        tema_abogadas +
        text_size_to_use() +
        theme(axis.title = element_blank(),
              axis.text.x = element_text(size = ifelse(mobile, 8, 20), face = "bold",
                                         family = "Roboto Slab", color = "#242223"),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_blank())
    })

    output$plot_cuidados_vs_outcomes <- renderPlot({
      mobile <- ifelse(plotWidth() > 586, F, T)

      plot <- plot_cuidados_vs_outcomes(resultado_var_choice = input$resultado_var,
                                cuidados_var_choice = input$cuidados_var, mobile) +
        text_size_to_use()

      if(input$resultado_var == "particip"){
        plot <- plot +
          theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
      }

      return(plot)
    })

    output$hallazgos_thrs_particip <- renderUI({
      HTML(paste0("Con respecto a la participación en el mercado laboral, ",
             "las mujeres que estudiaron Derecho que sí trabajan por una remuneración (pero no necesariamente como abogadas) dedican en promedio ",
             span(paste0(thrs_particip, " horas semanales "), style = "font-weight: bold;"),
             "al trabajo de cuidados, mientras que sus contrapartes que no participan en el mercado laboral dedican en promedio ",
             span(paste0(thrs_no_particip, " horas por semana "), style = "font-weight: bold;"),
             "a estas tareas."))
    })

  })
}

# Demo ====
app_demo <- function(){
  ui <- fluidPage(
    screen_size_test_ui("cuidados"),
    infographic_cuidados_ui("cuidados"),
    select_cuidados_resultado_ui("cuidados"),
    select_cuidados_indicador_ui("cuidados"),
    plot_cuidados_vs_outcomes_ui("cuidados"),
    text_thrs_particip_ui("cuidados")
  )

  server <- function(input, output, session) {
    cuidados_server("cuidados")
  }

  shinyApp(ui, server)
}

# done.
