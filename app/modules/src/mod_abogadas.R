#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/app/modules/src/mod_genero.R

# TODO

# Load required libraries
pacman::p_load(shiny, stringr, ggplot2, dplyr, here, data.table, forcats)

# Files
paths <- list(sit_fam_joined = here("prep-app/output/sit_fam_joined.csv"),
              indig_joined = here("prep-app/output/indig_subrep.csv"),
              brecha_indig = here("prep-app/output/brecha_indig.csv"),
              discap_types = here("prep-app/output/discap_types.csv"),
              discap_subrep = here("prep-app/output/discap_subrep.csv"),
              univ_ingresos_gap = here("prep-app/output/univ_ingresos_gap.csv"),
              univ_sector = here("prep-app/output/univ_sector.csv"),
              univ_dist = here("prep-app/output/univ_dist.csv"),
              perfiles = here("prep-app/output/perfiles.csv"))

# Read data
sit_fam <- fread(paths$sit_fam_joined) %>%
  mutate(edad_categ_10 = ifelse(edad_categ_10 == "21 a 29", "21 a 29 años", edad_categ_10),
         sector = factor(sector, levels = c("Otro",
                                            "Gobierno",
                                            "Empresa del sector privado",
                                            "Empresa personal")))

indig_subrep <- fread(paths$indig_joined)

indig_brecha <- fread(paths$brecha_indig)

discap_types <- fread(paths$discap_types)

discap_subrep <- fread(paths$discap_subrep)

univ_brecha <- fread(paths$univ_ingresos_gap)

univ_sector <- fread(paths$univ_sector)

univ_dist <- fread(paths$univ_dist)

perfiles <- fread(paths$perfiles)

# Run theme
source(here("app/modules/src/grafs-theme-fns.R"))

# Define constants
min_year_enoe <- sit_fam %>% pull(min_year) %>% unique()

max_year_enoe <- sit_fam %>% pull(max_year) %>% unique()

perc_indig_pop <- indig_subrep %>%
  filter(ocups_de_interes == "General") %>%
  pull(perc_indig) * 100

perc_indig_abogada <- indig_subrep %>%
  filter(ocups_de_interes == "Abogadas") %>%
  pull(perc_indig) * 100

brecha_indig_abogadas <- indig_brecha %>%
  pull(brecha_centavos_menos)

perc_discap_pop <- discap_subrep %>%
  filter(ocups_de_interes == "General") %>%
  pull(perc_disc)

perc_discap_abogada <- discap_subrep %>%
  filter(ocups_de_interes == "Abogadas") %>%
  pull(perc_disc)

univ_pub_abga <- univ_dist %>%
  filter(dist_type == "law" & escuela_tipo == "pública") %>%
  pull(perc)

univ_pub_gen <- univ_dist %>%
  filter(dist_type == "general" & escuela_tipo == "pública") %>%
  pull(perc)

univ_priv_abga <- univ_dist %>%
  filter(dist_type == "law" & escuela_tipo == "privada") %>%
  pull(perc)

univ_priv_gen <- univ_dist %>%
  filter(dist_type == "general" & escuela_tipo == "privada") %>%
  pull(perc)

univ_brecha_law <- univ_brecha %>%
  filter(gap_type == "law") %>%
  pull(gap)

abgas_madres <- (perfiles %>%
  filter(group == "Mujeres abogadas" & categ_type == "maternidad_e_con" &
           def_type == "ocup" & str_detect(categ_name, "con hijes")) %>%
  summarize(sum(categ_perc) / 10)) %>% pull() %>% round(0)

abgas_casadas <- (perfiles %>%
                          filter(group == "Mujeres abogadas" & categ_type == "maternidad_e_con" &
                                   def_type == "ocup" & str_detect(tolower(categ_name), "casada")) %>%
                    summarize(sum(categ_perc) / 10)) %>% pull() %>% round(0)

ingresos_title <- "Ingresos mensuales de las abogadas"

informal_title <- "Porcentaje de abogadas que trabajan en el sector informal"

particip_title <- "Porcentaje de egresadas de la carrera de Derecho que participan en el mercado laboral"

ejercicio_title <- "Porcentaje* de egresadas de la carrera de Derecho que ejercen como abogadas"

sector_title <- "Sector de trabajo de las abogadas"

enoe_caption_default <- paste0("Fuente: Elaboración propia con datos de la ENOE ", min_year_enoe, " - ", max_year_enoe, ".")

endireh_caption_default <- paste0("Fuente: Elaboración propia con datos de la ENDIREH 2016 - 2021.",
                                  "\n'Abogadas' se consideran las que declararon su última ocupación\ncomo abogada, juez, o coordinadora en una área jurídica")

endireh_caption_indig <-  paste0("Fuente: Elaboración propia con datos de la ENDIREH 2021.",
                                 "\n'Abogadas' se consideran las que declararon su última ocupación\ncomo abogada, juez, o coordinadora en una área jurídica")

censo_caption_default <- "Fuente: Elaboración propia con datos del Censo de Población y Vivienda 2020."

# Define functions ====
plot_sit_fam <- function(var_choice, mobile){
  df <- sit_fam %>%
    filter(var_type == var_choice)

  subtitle_var <- "Desagregado por edad, maternidad y estado conyugal"
  muestra_insuficiente <- "Muestra insuficiente"
  label_size <- 4

  if(mobile){
    subtitle_var <- str_wrap(subtitle_var, 33)
    muestra_insuficiente <- str_wrap(muestra_insuficiente, 10)
    label_size <- 3
  }

  if(var_choice == "ingresos"){
    df$var_label <- paste0("$", df$var_value)
  }

  if(var_choice %in% c("informal", "particip", "ejercicio")){
    df$var_label <- paste0(df$var_value, "%")
  }

  if(var_choice %in% c("ingresos", "informal", "particip", "ejercicio")){
    df <- df %>%
      mutate(var_label = ifelse(str_detect(var_label, "insuficiente"),
                                muestra_insuficiente, var_label))

    plot <- ggplot(data = df, aes(x = 0, y = fct_rev(edad_categ_10))) +
      geom_tile(aes(fill = as.numeric(var_value))) +
      geom_label(aes(label = var_label), fill = "#faf4e8", color = "#242223",
                 fontface = "bold", family = "Roboto", size = label_size, label.size = NA) +
      facet_grid(maternidad~e_con,
                 labeller = labeller(e_con = label_wrap_gen(
                   ifelse(mobile, 15, 30)))) +
      labs(title = get(paste0(var_choice, "_title")),
           subtitle = subtitle_var,
           caption = ifelse(var_choice == "ejercicio",
                            paste0(enoe_caption_default,
                                   "\n*Del total que participa en el mercado laboral"),
                            enoe_caption_default),
           y = "", x = "") +
      tema_abogadas +
      scale_fill_gradient(low = pal_gradient_purple["low"], high = pal_gradient_purple["high"])
  }

  if(var_choice == "sector"){
    plot <- ggplot(data = df %>% select(-edad_categ_10), aes(x = factor(1), y = as.numeric(var_value), fill = sector)) +
      geom_bar(position = "stack", stat = "identity") +
      facet_grid(maternidad ~ e_con) +
      labs(y = "", x = "", title = sector_title, fill = "",
           caption = enoe_caption_default) +
      tema_abogadas +
      scale_fill_manual(values = pal_5) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1.01),
                         breaks = seq(0, 1, by = 0.2)) +
      theme(axis.line.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title = element_blank())

  }

  return(plot)
}

# UI ====
# SECTION 1: MOTHERHOOD/MARRIAGE
screen_size_test_ui <- function(id){
  ns <- NS(id)

  verbatimTextOutput(ns("screen_size_test"))
}

test_ui <- function(id){
  ns <- NS(id)

  textOutput(ns("clientdataText"))
}

sit_fam_intro_text_ui <- function(id){
  ns <- NS(id)

  htmlOutput(ns("sit_fam_intro_text"))
}

select_sit_fam_var_ui <- function(id){
  ns <- NS(id)

  selectInput(ns("sit_fam_var"), "Variable de interés:",
              choices = c("Ingresos" = "ingresos",
                          "Informalidad de empleo" = "informal",
                          "Participación laboral" = "particip",
                          "Ejercicio de carrera" = "ejercicio",
                          "Sector de trabajo" = "sector"))
}

sit_fam_plot_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("sit_fam_plot"))
}

# SECTION 2: INDIGENEITY
indig_percent_text_ui <- function(id){
  ns <- NS(id)

  htmlOutput(ns("indig_percent_text"))
}

indig_subrep_text_ui <- function(id){
  ns <- NS(id)

  htmlOutput(ns("indig_subrep_text"))
}

indig_subrep_plot_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("indig_subrep_plot"))
}

indig_brecha_text_ui <- function(id){
  ns <- NS(id)

  htmlOutput(ns("indig_brecha_text"))
}

# SECTION 3: DISABILITY
discap_infograf_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("discap_infograf"))
}

discap_subrep_plot_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("discap_subrep_plot"))
}

discap_text_ui <- function(id){
  ns <- NS(id)

  htmlOutput(ns("discap_text"))
}

# SECTION 4: PUBLIC VS PRIVATE UNIVERSITY
univ_dist_text_ui <- function(id){
  ns <- NS(id)

  htmlOutput(ns("univ_dist_text"))
}

univ_sector_plot_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("univ_sector_plot"))
}

univ_brecha_text_ui <- function(id){
  ns <- NS(id)

  htmlOutput(ns("univ_brecha_text"))
}

# Server ====
abogadas_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns(id)

    ns <- session$ns(id)

    plotWidth <- reactive({session$clientData[["output_abogadas-sit_fam_plot_width"]]})

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

    # SECTION 1: MOTHERHOOD/MARRIAGE
    output$sit_fam_intro_text <- renderUI({
      HTML(paste0("De cada 10 mujeres que ejercen como abogadas, ",
      span(abgas_madres, style = "font-weight: bold;"),
      " son madres y ",
      span(abgas_casadas, style = "font-weight: bold;"),
      " están casadas o viven en unión libre. ",
      "Abajo se puede ver cómo consideraciones como la maternidad y el estado civil de las abogadas afectan las realidades laborales que enfrentan."))
    })

    output$sit_fam_plot <- renderPlot({
      if (plotWidth() > 586) {
        plot <- plot_sit_fam(input$sit_fam_var, mobile = F) +
          text_size_to_use()
      } else {
        plot <- plot_sit_fam(input$sit_fam_var, mobile = T) +
          text_size_to_use()
      }

      if(input$sit_fam_var == "sector"){
        plot <- plot +
          theme(axis.line.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title = element_blank())
      } else {
        plot <- plot +
          theme(axis.ticks = element_blank(),
                axis.text.x = element_blank(),
                axis.line = element_blank(),
                legend.position = "none")
      }

      return(plot)
      dev.off()
    })

    # SECTION 2: INDIGENEITY
    output$indig_percent_text <- renderUI({
      HTML(paste0("En términos de identificación étnica, ",
                  span(paste0(perc_indig_abogada, "%"),  style = "font-weight: bold;"),
                  " de abogadas en México se reconocen como indígenas, mientras que ",
                  span(paste0(perc_indig_pop, "%"), style = "font-weight: bold;"),
                  " de todas las mujeres del país se identifican así."))
    })

    output$indig_subrep_text <- renderUI({
      HTML(paste0("Esto quiere decir que para que la población de abogadas ",
                  "fuera étnicamente representativa de México en general, tendría que haber aproximadamente ",
                  span(round(perc_indig_pop / perc_indig_abogada, 1), style = "font-weight: bold;"),
                  " veces más abogadas indígenas de las que actualmente hay."))
    })

    output$indig_subrep_plot <- renderPlot({
      mobile <- ifelse(plotWidth() > 586, F, T)

      plot <- ggplot(data = indig_subrep %>% filter(ocups_de_interes != "General"),
                     aes(x = ocups_de_interes, y = perc_indig)) +
        geom_col(fill = pal_2[1]) +
        geom_errorbar(aes(ymin = perc_indig - margin_error, ymax = perc_indig + margin_error),
                      position = position_dodge(0.9), color = "#bcbcbc", width = 0.25) +
        geom_hline(yintercept = as.numeric(perc_indig_pop)/100, linetype = "dashed", color = "#242223") +
        geom_text(aes(label = paste0(perc_indig*100, "%"), y = perc_indig - margin_error - 0.03),
                  color = "#faf4e8", fontface = "bold", family = "Roboto", size = ifelse(mobile, 3, 4)) +
        geom_text(aes(label = "Mexicanas en general", x = 4,
                      y = (as.numeric(perc_indig_pop)/100) + 0.03),
                  color = "#242223", family = "Roboto Slab",
                  fontface = "bold", size = ifelse(mobile, 4, 5)) +
        labs(title = ifelse(mobile,
                            "Porcentaje de mujeres que se identifican\ncomo indígenas",
                            "Porcentaje de mujeres que se identifican como indígenas"),
             subtitle = "Desagregado por ocupación",
             caption = paste0(endireh_caption_indig, "\n",
                              "Intervalos representan intervalos de confianza del 90%"),
             x = "", y = "") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        tema_abogadas +
        text_size_to_use()
      
      if(plotWidth() < 800){
        plot <- plot +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      return(plot)
      dev.off()
    })

    output$indig_brecha_text <- renderUI({
      HTML(paste0("La desigualdad étnica en el derecho no es solo una cuestión de representatividad sino también de disparidades salariales. ",
                  "Por cada peso que gana una abogada no indígena, una abogada indígena gana en promedio ",
                  span(paste0(brecha_indig_abogadas, " centavos menos."), style = "font-weight: bold;"),
                  "\n(Fuente: Censo de Población y Vivienda 2020)"))
    })

    # SECTION 3: DISABILITY
    output$discap_infograf <- renderPlot({
      mobile <- ifelse(plotWidth() > 586, F, T)

      if(mobile){
        x_limits <- c(0, 6)
      } else{
        x_limits <- c(0.5, 5.5)
      }

      plot <- ggplot(data = discap_types, aes(x = x_val, y = y_val)) +
        geom_text(aes(label = paste0(disc_desc, "?")),
                  color = "#242223", fontface = "bold", family = "Roboto",
                  size = ifelse(mobile, 5, 8)) +
        geom_text(aes(label = paste0(perc * 100, "%"), y = y_val), vjust = 1.5,
                  color = "#242223", fontface = "bold", family = "Roboto",
                  size = ifelse(mobile, 10, 30)) +
        labs(title = ifelse(mobile,
                            "De la población de abogadas,\n¿qué porcentaje tiene una discapacidad...",
                            "De la población de abogadas, ¿qué porcentaje tiene una discapacidad..."),
             caption = paste0(endireh_caption_indig)) +
        coord_cartesian(ylim = c(-0.75, 1.25), xlim = x_limits) +
        tema_abogadas +
        text_size_to_use() +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, size = ifelse(mobile, 14, 30)))
      
      return(plot)
      dev.off()
    })

    output$discap_subrep_plot <- renderPlot({
      mobile <- ifelse(plotWidth() > 586, F, T)

      df <- discap_subrep %>% filter(ocups_de_interes != "General")

      plot <- ggplot(data = df,
             aes(x = ocups_de_interes, y = perc_disc)) +
        geom_col(fill = pal_2[1]) +
        geom_errorbar(aes(ymin = perc_disc - margin_error, ymax = perc_disc + margin_error),
                      position = position_dodge(0.9), color = "#bcbcbc", width = 0.25) +
        geom_hline(yintercept = as.numeric(perc_discap_pop), linetype = "dashed", color = "#242223") +
        geom_text(aes(label = "Mexicanas en general", x = 4,
                      y = as.numeric(perc_discap_pop + 0.03)),
                  color = "#242223", family = "Roboto Slab",
                  fontface = "bold", size = ifelse(mobile, 4, 5)) +
        labs(title = ifelse(mobile,
                    "Porcentaje de mujeres que tienen\nalguna discapacidad",
                    "Porcentaje de mujeres que tienen alguna discapacidad"),
             subtitle = "Desagregado por ocupación",
             caption = paste0(endireh_caption_indig, "\n",
                              "Intervalos representan intervalos de confianza del 90%"),
             x = "", y = "") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        tema_abogadas +
        text_size_to_use()
      
      if(plotWidth() < 800){
        plot <- plot +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      return(plot)
      dev.off()
    })

    output$discap_text <- renderUI({
      HTML(paste0(
        span(paste0("Una de cada ", round(1/perc_discap_abogada, 0)), style = "font-weight: bold;"),
        " abogadas tiene alguna discapacidad, lo cual significa que hay aproximadamente ",
        span(paste0(round(perc_discap_pop / perc_discap_abogada, 1)," veces menos"), style = "font-weight: bold;"),
        " mujeres con discapacidad entre las abogadas que entre la población en general. "))
    })


    # SECTION 4: PUBLIC VS. PRIVATE UNIVERSITY
    output$univ_dist_text <- renderUI({
      HTML(paste0("Mientras que ", span(paste0(univ_pub_abga,"%"), style = "font-weight: bold;"),
                  " de las abogadas estudiaron en una universidad pública, ",
                  span(paste0(univ_priv_abga,"%"), style = "font-weight: bold;"),
                  " estudiaron en una privada. ",
                  "En cambio, de las mujeres con estudios superiores (de cualquier disciplina), ",
                  span(paste0(univ_pub_gen,"%"), style = "font-weight: bold;"),
                  " estudiaron en una universidad pública y ",
                  span(paste0(univ_priv_gen,"%"), style = "font-weight: bold;"),
                  " en una privada. "))
    })

    output$univ_sector_plot <- renderPlot({
      mobile <- ifelse(plotWidth() > 586, F, T)

      plot_title <- ifelse(mobile,
                           "Sector de trabajo de las abogadas según tipo de\nuniversidad donde estudiaron",
                           "Sector de trabajo de las abogadas según tipo de universidad donde estudiaron")
      
      df <- univ_sector %>% 
        mutate(sector_trabajo = case_when(mobile ~ str_wrap(sector_trabajo, 15), 
                                          T ~ sector_trabajo))

      plot <- ggplot(data = df, aes(x = fct_rev(escuela_tipo), y = perc,
                                     fill = sector_trabajo)) +
        geom_bar(position = "stack", stat = "identity") +
        geom_text(aes(label = paste0(perc*100, "%"), y = perc),
                  position = position_stack(0.5),
                  color = "#faf4e8", fontface = "bold", family = "Roboto", size = 4) +
        labs(y = "", x = "", title = plot_title,
             fill = "Sector de trabajo", caption = endireh_caption_default) +
        scale_y_continuous(aes(labels = perc), breaks = seq(0, 1, by = 0.2)) +
        tema_abogadas +
        text_size_to_use() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank()) +
        scale_fill_manual(values = pal_5) +
        guides(fill=guide_legend(ncol=ifelse(mobile, 2, 4), byrow=TRUE))
      
      return(plot)
      dev.off()
    })

    output$univ_brecha_text <- renderUI({
      HTML(paste0("Así como existen brechas salariales por sexo e identidad étnica, también existe una brecha salarial por el tipo de universidad donde una abogada estudió. ",
                  "Las abogadas que estudiaron en una universidad pública ganan en promedio ",
                  span(paste0(univ_brecha_law, "% menos"), style = "font-weight: bold;"),
                  " que las que estudiaron en una universidad privada."))
    })

  })
}

# Run Shiny app
app_demo <- function(){
  ui <- fluidPage(
    screen_size_test_ui("abogadas"),
    # sit_fam_intro_text_ui("abogadas"),
    select_sit_fam_var_ui("abogadas"),
    sit_fam_plot_ui("abogadas"),
    indig_percent_text_ui("abogadas"),
    indig_subrep_text_ui("abogadas"),
    indig_subrep_plot_ui("abogadas"),
    indig_brecha_text_ui("abogadas"),
    discap_infograf_ui("abogadas"),
    discap_subrep_plot_ui("abogadas"),
    discap_text_ui("abogadas"),
    univ_dist_text_ui("abogadas"),
    univ_sector_plot_ui("abogadas"),
    univ_brecha_text_ui("abogadas")
  )

  server <- function(input, output, session) {
    abogadas_server("abogadas")
    }

shinyApp(ui, server)
}

# done
