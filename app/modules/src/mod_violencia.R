#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/app/modules/src/mod_perfiles.R

# TODO

# Packages
pacman::p_load(shiny, stringr, ggplot2, dplyr, here, data.table, scales, ggrepel)

# Files
paths <- list(cifras_resumen_violencia = here("prep-app/output/violencia_abogadas.csv"),
              violencia_abgas_desag = here("prep-app/output/violencia_abgas_desag.csv"),
              violencia_ocups = here("prep-app/output/violencia_ocups.csv"),
              theme = here("app/modules/src/grafs-theme-fns.R"))

# Source theme
source(paths$theme)

# Read data
cifras_resumen_violencia <- fread(paths$cifras_resumen_violencia)

violencia_abgas_desag <- fread(paths$violencia_abgas_desag) %>%
  mutate(indicador_desc = factor(indicador_desc, levels = c("Le han pedido una prueba de embarazo como requisito para trabajar",
                                                            "La despidieron o no le renovaron el contrato por embarazarse",

                                                            "Ha tenido menos oportunidad que un hombre para ascender",
                                                            "Le han limitado su desarrollo profesional para favorecer a un hombre",
                                                            "Le han pagado menos que a un hombre en el mismo puesto",

                                                            "La han ignorado por ser mujer",
                                                            "La han manoseado, besado o tocado sin su consentimiento",
                                                            "Le han dicho piropos groseros u ofensivos"))) %>%
  arrange(indicador_desc) %>%
  mutate(indicador_color = pal_9[match(indicador, unique(indicador))])

violencia_ocups <- fread(paths$violencia_ocups)

# Define constants
endireh_min_year <- unique(violencia_abgas_desag$min_year)

endireh_max_year <- unique(violencia_abgas_desag$max_year)

caption_endireh_default <- paste0("Fuente: Elaboración propia con datos de la ENDIREH ", endireh_min_year, "-", endireh_max_year, ".",
                                  "\n'Abogadas' se consideran las que declararon su última ocupación\ncomo abogada, juez, o coordinadora en una área jurídica")

# Define functions
plot_violencia_abgas <- function(desag_choice, indicadores_graf, plot_title,
                                 mobile, tablet){
  var_title <- case_when(desag_choice == "e_con" ~ "Estado conyugal",
                          desag_choice == "edad" ~ "Edad",
                          desag_choice == "sector" ~ "Sector de trabajo")

  title_chars <- case_when(mobile ~ 50,
                           tablet ~ 80,
                           TRUE ~ 120)

  tempo <- violencia_abgas_desag %>%
    filter(grouped_by == desag_choice &
             indicador %in% indicadores_graf)

    ggplot(data = tempo,
           aes(x = 0, y = valor,
               fill = str_wrap(indicador_desc, ifelse(mobile, 20, 40)))) +
    geom_col(position = "dodge") +
    geom_text(aes(label = case_when(mobile ~ "", T ~ paste0(valor, "%")),
                        y = valor + margin_error),
              position = position_dodge(0.9), vjust = -1,
              fontface = "bold", family = "Roboto", color = "#242223") +
    facet_wrap(~group, nrow = 1) +
    geom_errorbar(aes(ymin = ifelse(valor - margin_error < 0, 0, valor - margin_error),
                      ymax = valor + margin_error), position = position_dodge(0.9),
                  color = "gray", width = 0.25) +
    labs(title = str_wrap(plot_title, title_chars),
         subtitle = paste0("Por ", tolower(var_title)), y = "", x = var_title,
         caption = paste0(caption_endireh_default, "\nIntervalos representan intervalos de confianza de 90%")) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 50),
                       breaks = seq(0, 50, 10)) +
    scale_fill_manual(values = unique(tempo$indicador_color)) +
    tema_abogadas +
    guides(fill=guide_legend(ncol=ifelse(mobile | tablet, 2, 3), byrow=TRUE))

}

plot_violencia_ocup <- function(indic_choice, mobile, tablet){
  title_chars <- case_when(mobile ~ 50,
                           tablet ~ 80,
                           TRUE ~ 120)

  violencia_ocups %>%
    filter(indicador_desc == indic_choice) %>%
    ggplot(data = ., aes(x = ocups_de_interes, y = valor)) +
    geom_col(fill = "#D79438") +
    geom_errorbar(aes(ymin = valor - margin_error, ymax = valor + margin_error),
                  position = position_dodge(0.9), color = "gray", width = 0.25) +
    labs(title = str_wrap(paste0("¿", indic_choice, "?"), title_chars),
         subtitle = "Porcentaje que respondió que sí, por ocupación", x = "", y = "",
         caption = paste0(caption_endireh_default, "\nIntervalos representan intervalos de confianza de 90%")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 50),
                       breaks = seq(0, 50, 10)) +
    tema_abogadas
}

# UI ====
infographic_violencia_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("cifras_resumen_violencia"))
}

select_violencia_var_ui <- function(id){
  ns <- NS(id)

  selectInput(ns("desag_var"), "Explora cómo estas cifras varían por:",
              choices = c("Edad" = "edad",
                          "Estado conyugal" = "e_con",
                          "Sector de trabajo" = "sector"))
}

plot_violencia_embzo_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("violencia_abgas_embzo"))
}

plot_violencia_anio_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("violencia_abgas_anio"))
}

plot_violencia_lab_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("violencia_abgas_lab"))
}

select_tipo_violencia_ui <- function(id){
  ns <- NS(id)

  selectInput(ns("indic_var"), "Tipo de violencia o discriminación laboral",
              choices = levels(violencia_abgas_desag$indicador_desc),
              width = 600)
}

plot_violencia_ocup_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("violencia_por_ocup"))
}

# Server ====
violencia_server <- function(id){
  moduleServer(id, function(input, output, session) {
    plotWidth <- reactive({session$clientData[["output_violencia-violencia_abgas_embzo_width"]]})

    text_size_to_use <- reactive({
      if (plotWidth() > 586) {
        laptop_text_sizes
      } else {
        mobile_text_sizes
      }
    })

    # 1. OVERVIEW DE VIOLENCIA
    output$cifras_resumen_violencia <- renderPlot({
      mobile <- ifelse(plotWidth() > 586, FALSE, TRUE)

      if(mobile){
        x_limits <- c(0.5, 2.5)
      } else{
        x_limits <- c(0.75, 2.25)
      }

      plot <- ggplot(data = cifras_resumen_violencia, aes(x = x, y = y)) +
        geom_text(aes(label = paste0(valor, "%")), size = ifelse(mobile, 8, 20),
                  fontface = "bold", color = "#242223", family = "Roboto Slab") +
        geom_text(aes(label = ifelse(mobile,
                                     str_wrap("le han pagado menos que a un hombre en el mismo puesto", 25),
                                     "le han pagado menos que a un hombre en el mismo puesto"),
                      x = 1, y = 4), vjust = ifelse(mobile, 1.5, 6), family = "Roboto", color = "#242223",
                  size = ifelse(mobile, 3, 4)) +
        geom_text(aes(label = ifelse(mobile,
                                     str_wrap("ha tenido menos oportunidad que un hombre para ascender", 25),
                                     "ha tenido menos oportunidad que un hombre para ascender"),
                      x = 2, y = 4), vjust = ifelse(mobile, 1.5, 6), family = "Roboto", color = "#242223",
                  size = ifelse(mobile, 3, 4)) +
        geom_text(aes(label = ifelse(mobile,
                                     str_wrap("le han limitado su desarrollo profesional para favorecer a un hombre", 30),
                                     "le han limitado su desarrollo profesional para favorecer a un hombre"),
                      x = 1, y = 3), vjust = ifelse(mobile, 1.5, 6), family = "Roboto", color = "#242223",
                  size = ifelse(mobile, 3, 4)) +
        geom_text(aes(label = ifelse(mobile,
                                     str_wrap("la han ignorado por ser mujer", 25),
                                     "la han ignorado por ser mujer"),
                      x = 2, y = 3), vjust = ifelse(mobile, 2, 6), family = "Roboto", color = "#242223",
                  size = ifelse(mobile, 3, 4)) +
        geom_text(aes(label = ifelse(mobile,
                                     str_wrap("le han dicho piropos groseros u ofensivos", 30),
                                     "le han dicho piropos groseros u ofensivos"),
                      x = 1, y = 2), vjust = ifelse(mobile, 2, 6), family = "Roboto", color = "#242223",
                  size = ifelse(mobile, 3, 4)) +
        geom_text(aes(label = ifelse(mobile,
                                     str_wrap("la han manoseado, besado o tocado sin su consentimiento", 25),
                                     "la han manoseado, besado o tocado sin su consentimiento"),
                      x = 2, y = 2), vjust = ifelse(mobile, 1.5, 6), family = "Roboto", color = "#242223",
                  size = ifelse(mobile, 3, 4)) +
        geom_text(aes(label = ifelse(mobile,
                                     str_wrap("la han amenazado con despedirla o no renovarle el contrato", 30),
                                     "le han pedido una prueba de embarazo como requisito para trabajar"),
                      x = 1, y = 1), vjust = ifelse(mobile, 1.5, 6), family = "Roboto", color = "#242223",
                  size = ifelse(mobile, 3, 4)) +
        geom_text(aes(label = ifelse(mobile,
                                     str_wrap("la han amenazado con despedirla o no renovarle el contrato", 30),
                                     "la despidieron o no le renovaron el contrato por embarazarse"),
                      x = 2, y = 1), vjust = ifelse(mobile, 1.5, 6), family = "Roboto", color = "#242223",
                  size = ifelse(mobile, 3, 4)) +
        coord_cartesian(xlim = x_limits, ylim = c(0.5, 4.25)) +
        labs(caption = paste0(caption_endireh_default,
                              "\nTodas las cifras se refieren a eventos ocurridos en el ámbito laboral")) +
        tema_abogadas +
        text_size_to_use() +
        theme(axis.title = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_blank())
      
      return(plot)
      dev.off()
    })

    # 2. BAR CHARTS INTERACTIVOS DE VIOLENCIA HACIA LAS ABOGADAS
    output$violencia_abgas_embzo <- renderPlot({
      mobile <- ifelse(plotWidth() > 586, FALSE, TRUE)

      tablet <- ifelse(plotWidth() > 768, FALSE, TRUE)

      plot <- plot_violencia_abgas(input$desag_var, c("embzo_prueba_trab", "embzo_desp"),
                           "¿Qué porcentaje de las abogadas han vivido discriminación por embarazo en su trabajo durante los últimos 5 años?",
                           mobile, tablet) +
        text_size_to_use() +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_blank())

      if(!mobile){
        plot <- plot + theme(axis.text.y = element_blank())
      }

      return(plot)
      dev.off()
    })

    output$violencia_abgas_anio <- renderPlot({
      mobile <- ifelse(plotWidth() > 586, FALSE, TRUE)

      tablet <- ifelse(plotWidth() > 768, FALSE, TRUE)

      plot <- plot_violencia_abgas(input$desag_var, c("anio_menos_oport_asc", "anio_limiar_des_prof", "anio_pagar_menos"),
                           "¿Qué porcentaje de las abogadas han vivido discriminación de género en su trabajo durante el último año?",
                           mobile, tablet) +
        text_size_to_use() +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_blank())

      if(!mobile){
        plot <- plot + theme(axis.text.y = element_blank())
      }

      return(plot)
      dev.off()
    })

    output$violencia_abgas_lab <- renderPlot({
      mobile <- ifelse(plotWidth() > 586, FALSE, TRUE)

      tablet <- ifelse(plotWidth() > 768, FALSE, TRUE)

      plot <- plot_violencia_abgas(input$desag_var, c("lab_piropos", "lab_tocar", "lab_ignorar_mujer"),
                           "¿Qué porcentaje de las abogadas han vivido discriminación o violencia de género en alguno de sus trabajos?",
                           mobile, tablet) +
        text_size_to_use() +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_blank())

      if(!mobile){
        plot <- plot + theme(axis.text.y = element_blank())
      }

      return(plot)
      dev.off()
    })


    # 3. BAR CHART DE VIOLENCIA POR OCUPACIÓN
    output$violencia_por_ocup <- renderPlot({
      mobile <- ifelse(plotWidth() > 586, FALSE, TRUE)

      tablet <- ifelse(plotWidth() > 768, FALSE, TRUE)

      plot <- plot_violencia_ocup(input$indic_var, mobile, tablet) +
        text_size_to_use()

      if(mobile){
        plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }

      return(plot)
      dev.off()
    })

  })
}

# Demo ====
app_demo <- function(){
  ui <- fluidPage(
    infographic_violencia_ui("violencia"),
    select_violencia_var_ui("violencia"),
    plot_violencia_embzo_ui("violencia"),
    plot_violencia_anio_ui("violencia"),
    plot_violencia_lab_ui("violencia"),
    select_tipo_violencia_ui("violencia"),
    plot_violencia_ocup_ui("violencia")
  )

  server <- function(input, output, session) {
    violencia_server("violencia")
  }

  shinyApp(ui, server)
}

# done.
