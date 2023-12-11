#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/app/modules/src/mod_genero.R

# TODO

# Load required libraries
pacman::p_load(shiny, stringr, dplyr, here, data.table, ggplot2, spatstat)

# Define paths
paths <- list(paridad_genero_sector = here("prep-app/output/paridad_genero_sector.csv"),
              paridad_genero_edad = here("prep-app/output/paridad_genero_edad.csv"),
              paridad_genero_geog = here("prep-app/output/paridad_genero_geog.csv"),
              brecha_sal_por_ocup = here("prep-app/output/brecha_sal_por_ocup.csv"),
              brecha_sal_abogadxs = here("prep-app/output/brecha_sal_abogadxs.csv"),
              participacion_carrera = here("prep-app/output/particip-carrera.csv"),
              brechas_participacion = here("prep-app/output/brechas-participacion.csv"),
              ejercicio_derecho = here("prep-app/output/ejercicio-derecho.csv"),
              mxhexbin_map = here("prep-app/output/mxhexbin.csv"),
              theme = here("app/modules/src/grafs-theme-fns.R"))

# Read data ====
paridad_genero_sector <- fread(paths$paridad_genero_sector) %>% 
  mutate(sexo = ifelse(sexo == "mujer", "Mujeres", "Hombres"),
         sector = case_when(sector == "Escuela u hospital" ~ "Escuela u\nhospital",
                            sector == "Gobierno (otro)" ~ "Gobierno\n(otro)",
                            sector == "Empresa del sector privado" ~ "Empresa del\nsector privado",
                            sector == "Gobierno estatal o municipal" ~ "Gobierno estatal\no municipal",
                            sector == "Poder judicial o legislativo" ~ "Poder judicial\no legislativo",
                            T ~ sector))
         
paridad_genero_edad <- fread(paths$paridad_genero_edad)

paridad_genero_geog <- fread(paths$paridad_genero_geog) %>% 
  mutate(cve_ent = as.character(sprintf("%02d", cve_ent)))

brecha_sal_por_ocup <- fread(paths$brecha_sal_por_ocup)

brecha_sal_abogadxs <- fread(paths$brecha_sal_abogadxs)

brechas_participacion <- fread(paths$brechas_participacion)

participacion_carrera <- fread(paths$participacion_carrera) %>% 
  # Order majors by gaps in participation
  mutate(carreras_de_interes = factor(carreras_de_interes,
                                      levels = c("General",
                                                 unique(brechas_participacion$carreras_de_interes))))

ejercicio_derecho <- fread(paths$ejercicio_derecho)

mxhexbin_map <- fread(paths$mxhexbin_map) %>% 
  mutate(id = str_pad(id, 2, pad = "0"))

# Load theme & functions for graphs
source(paths$theme)

# Define constants ====
min_year_enoe <- unique(paridad_genero_edad$min_year)

max_year_enoe <- unique(paridad_genero_edad$max_year)

sector_order <- c("General",
                  "Empresa del\nsector privado",
                  "Empresa personal",
                  "Gobierno federal",
                  "Gobierno estatal\no municipal",
                  "Poder judicial\no legislativo",
                  "Gobierno\n(otro)",
                  "Escuela u\nhospital",
                  "ONG")

# Factor sectors
paridad_genero_sector$sector <- factor(paridad_genero_sector$sector, levels = sector_order)

perc_muj_educ <- paridad_genero_edad %>% 
  filter(sexo == "Mujeres") %>% 
  group_by(sexo) %>% 
  summarize(weighted_mean = weighted.mean(perc, sum_fac)) %>%
  pull() %>% 
  round(3) * 100

perc_muj_ocup <- paridad_genero_sector %>% 
  filter(sexo == "Mujeres" & sector == "General") %>% 
  pull(perc)

perc_muj_25_29 <- paridad_genero_edad %>% 
  filter(grupo_edad == "[25,30)" & sexo == "Mujeres") %>% 
  pull(perc) * 100

perc_muj_55_59 <- paridad_genero_edad %>% 
  filter(grupo_edad == "[55,60)" & sexo == "Mujeres") %>% 
  pull(perc) * 100

brecha_general <- brecha_sal_por_ocup %>% 
  filter(ocups_de_interes == "Abogadxs") %>% 
  pull(brecha_centavos_menos)

participacion_gen_hombres <- participacion_carrera %>% 
  filter(carreras_de_interes == "General" & sexo == "Hombres") %>% 
  pull(perc_pea) %>% round(3) * 100

participacion_gen_mujeres <- participacion_carrera %>% 
  filter(carreras_de_interes == "General" & sexo == "Mujeres") %>% 
  pull(perc_pea) %>% round(3) * 100

participacion_der_hombres <- participacion_carrera %>% 
  filter(carreras_de_interes == "Derecho" & sexo == "Hombres") %>% 
  pull(perc_pea) %>% round(1) * 10

participacion_der_mujeres <- participacion_carrera %>% 
  filter(carreras_de_interes == "Derecho" & sexo == "Mujeres") %>% 
  pull(perc_pea) %>% round(1) * 10

pnea_abogadas <- 100 - (participacion_carrera %>% 
                        filter(carreras_de_interes == "Derecho" & sexo == "Mujeres") %>% 
                        pull(perc_pea)) * 100

ejercicio_mujeres <- ejercicio_derecho %>% 
  filter(sexo == "Mujeres" & categ_type == "solo_sexo") %>% 
  pull(perc_ejercicio) * 100

ejercicio_hombres <- ejercicio_derecho %>% 
  filter(sexo == "Hombres" & categ_type == "solo_sexo") %>% 
  pull(perc_ejercicio) * 100

map_centroids <- mxhexbin_map %>% 
  group_by(id) %>% 
  summarize(max_long = max(long),
            max_lat = max(lat),
            min_long = min(long),
            min_lat = min(lat),
            centroid_long = (max_long + min_long) / 2,
            centroid_lat = (max_lat + min_lat) / 2)

brecha_title_default <- paste0("Por cada peso que gana un abogado hombre,\n¿cuántos centavos menos gana una abogada de")

caption_enoe_default <- paste0("Fuente: Elaboración propia con datos de la ENOE ", min_year_enoe, " - ", max_year_enoe, ".")

# Define functions ====
plot_brecha_heat_map <- function(var_choice, mobile){
  
  filtered_df <- brecha_sal_abogadxs %>% 
    filter(categ_type == var_choice)
  
  nchar_wrap <- ifelse(mobile, 35, 200)
  
  # Estado conyugal
  unwrapped_title <- paste0(brecha_title_default, "l mismo estado conyugal?") %>%
    str_replace_all("\n", " ")
  
  unwrapped_subtitle <- "Valores negativos representan casos en los cuales las mujeres en promedio ganan más que los hombres"
  
  plot_title <- str_wrap(unwrapped_title, nchar_wrap)
  plot_subtitle <- str_wrap(unwrapped_subtitle, nchar_wrap)
  
  if(var_choice == "e_con"){
    plot <- ggplot(data = filtered_df, aes(x = categ_name, y = maternidad)) +
      geom_tile(aes(fill = brecha_centavos_menos), color = "#242223") +
      geom_text(aes(label = paste0(brecha_centavos_menos, " ¢")),
                color = ifelse(filtered_df$brecha_centavos_menos > 0, "#faf4e8", "#242223"),
                fontface = "bold", size = 6, family="Roboto") +
      scale_fill_gradient2(high = pal_gradient_gold["high"], low = pal_gradient_gold["low"], midpoint = 0,
                           breaks = c(0, max(filtered_df$brecha_centavos_menos) - 5),
                           labels = c("Mujeres ganan más", "Mujeres ganan menos")) +
      labs(x = "", y = "", fill = "Brecha salarial",
           title = plot_title,
           subtitle = plot_subtitle,
           caption = paste0(caption_enoe_default,"\n", 
                             "No se toma en cuenta la paternidad de los hombres ya que la ENOE no incluye ninguna pregunta sobre la paternidad de los hombres encuestados.")) +
      tema_abogadas +
      theme(axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line = element_blank(),
            legend.position = "none")
  }
  
  # Other variables
  else{
    # Define titles
    var_title <- case_when(var_choice == "edad" ~ paste0(brecha_title_default, " la misma edad?"),
                       var_choice == "sector" ~ paste0(brecha_title_default, "l mismo sector?")) %>% 
      str_replace_all("\n", " ")
    
    plot_title <- str_wrap(var_title, nchar_wrap)
    
    # Order sectors when selected
    if(var_choice == "sector") {
      filtered_df$categ_name <- factor(filtered_df$categ_name,
                                       levels = sector_order %>% str_replace("\n", " "))
      
      if(mobile){
        filtered_df$categ_name <- str_wrap(filtered_df$categ_name, 16)
      }
    }
    
    plot <- ggplot(data = filtered_df, aes(x = categ_name, y = 0)) +
      geom_tile(aes(fill = brecha_centavos_menos), color = "#242223") +
      geom_text(aes(label = paste0(brecha_centavos_menos, " ¢")),
                color = ifelse(filtered_df$brecha_centavos_menos > 6.5, "#faf4e8", "#242223"),
                fontface = "bold", size = 6, family="Roboto") +
      scale_fill_gradient2(high = pal_gradient_gold["high"], low = pal_gradient_gold["low"], midpoint = 0,
                           breaks = c(0, max(filtered_df$brecha_centavos_menos) - 5),
                           labels = c("Mujeres ganan más", "Mujeres ganan menos")) +
      labs(x = "", y = "", fill = "Brecha salarial", title = plot_title,
           subtitle = plot_subtitle,
           caption = caption_enoe_default) +
      tema_abogadas +
      theme(axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none")
  }
  return(plot)
}

plot_ejercicio <- function(var_choice){
  filtered_df <- ejercicio_derecho %>% 
    filter(categ_type == var_choice) %>% 
    mutate(maternidad = factor(maternidad, levels = c("Hombres",
                                                      "Mujeres sin hijes",
                                                      "Mujeres con hijes")))
  
  caption_plot_ejercicio <- paste0(caption_enoe_default, "\n",
                                   "Intervalos representan intervalos de confianza del 90%.")
  
  if(var_choice == "solo_sexo"){
    plot <- ggplot(data = filtered_df, aes(x = sexo, y = perc_ejercicio, fill = sexo)) +
      geom_col() +
      geom_errorbar(aes(ymin = ifelse(perc_ejercicio - margin_error >= 0, perc_ejercicio - margin_error, 0),
                        ymax = perc_ejercicio + margin_error),
                    position = position_dodge(0.9), color = "#bcbcbc", width = 0.25) +
      labs(title = "Del total de personas que estudiaron Derecho,\n¿qué porcentaje ejerce la profesión?",
           y = "", x = "", fill = "Sexo", caption = caption_plot_ejercicio) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_fill_manual(values = pal_hom_muj) +
      tema_abogadas +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank())
    
    return(plot)
  }
  
  if(var_choice == "maternidad_e_con"){
    plot <- ggplot(data = filtered_df, aes(x = e_con, y = perc_ejercicio, fill = maternidad)) +
      geom_col(position = position_dodge()) +
      geom_errorbar(aes(ymin = ifelse(perc_ejercicio - margin_error >= 0, perc_ejercicio - margin_error, 0),
                        ymax = perc_ejercicio + margin_error),
                    position = position_dodge(0.9), color = "#bcbcbc", width = 0.25) +
      labs(title = "Del total de personas que estudiaron Derecho,\n¿qué porcentaje ejerce la profesión?",
           y = "", x = "", fill = "Situación familiar", caption = caption_plot_ejercicio) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_fill_manual(values = pal_maternidad) +
      tema_abogadas +
      theme(
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
    
    return(plot)
  }
}

map_paridad <- function(def_choice, mobile){
  
  df_map <- paridad_genero_geog %>%
    filter(def_type == def_choice) %>% 
    left_join(mxhexbin_map, by = c("cve_ent" = "id"), multiple = "all") %>% 
    left_join(map_centroids, by = c("cve_ent" = "id")) %>% 
    mutate(color_label = ifelse(scale(perc_muj) > 2, 
                                        "#faf4e8", 
                                        "#242223"))
  
  stopifnot(nrow(df_map) == nrow(mxhexbin_map),
            all(!is.na(df_map$centroid_long)),
            all(!is.na(df_map$centroid_lat)))
  
  title_unwrapped <- case_when(def_choice == "educ" ~ "¿Qué porcentaje de las personas que estudariaron Derecho son mujeres?",
                          def_choice == "ocup" ~ "¿Qué porcentaje de las personas que ejercen como abogades son mujeres?")
  
  plot_title <- ifelse(mobile, str_wrap(title_unwrapped, 35), title_unwrapped)
  
  plot <- ggplot(df_map, aes(x = long, y = lat, group=group)) +
    geom_polygon(aes(fill = perc_muj), color = "#242223") +
    # coord_map() +
    labs(
      title = plot_title,
      fill = "", caption = caption_enoe_default) +
    tema_abogadas +
    scale_fill_gradient(low = "#ded9f7", high = "#5135d3") +
    scale_color_manual(values = c("#242223","#faf4e8"))
  
  return(plot)
}

# UI ====
genero_intro_text_ui <- function(id){
  ns <- NS(id)
  
  htmlOutput(ns("genero_intro"))
}

paridad_sector_plot_ui <- function(id){
  ns <- NS(id)
  
  plotOutput(ns("plot_paridad_sector"))
}

paridad_edad_plot_ui <- function(id){
  ns <- NS(id)
  
  plotOutput(ns("plot_paridad_edad"))
}

paridad_edad_text_ui <- function(id){
  ns <- NS(id)
  
  htmlOutput(ns("paridad_edad_text"))
}

select_lawyer_def_map_ui <- function(id){
  ns <- NS(id) 
  
  selectInput(ns("def_type_map"), "Definición:", choices = c("Por ocupación" = "ocup", "Por formación" = "educ"))
}

paridad_map_ui <- function(id){
  ns <- NS(id)
  
  plotOutput(ns("paridad_map"), height = "100%")
}

brecha_salarial_intro_text_ui <- function(id){
  ns <- NS(id)
  
  htmlOutput(ns("brecha_salarial_intro"))
}

brecha_salarial_ocup_plot_ui <- function(id){
  ns <- NS(id)
  
  plotOutput(ns("plot_brecha_ocup"))
}

select_heatmap_var_ui <- function(id){
  ns <- NS(id)
  
  selectInput(ns("heatmap_control"), "Variable de control:", choices = c("Edad" = "edad",
                                                                         "Sector" = "sector",
                                                                         "Estado conyugal" = "e_con"))
}

plot_brecha_interactive_ui <- function(id){
  ns <- NS(id)
  
  plotOutput(ns("plot_brecha_interactive"))
}

participacion_intro_text_ui <- function(id){
  ns <- NS(id)
  
  htmlOutput(ns("participacion_intro"))
}

participacion_hallazgos_ui <- function(id){
  ns <- NS(id)
  
  htmlOutput(ns("participacion_hallazgos"))
}

participacion_compare_ui <- function(id){
  ns <- NS(id)
  
  htmlOutput(ns("participacion_compare"))
}

plot_participacion_carrera_ui <- function(id){
  ns <- NS(id)
  
  plotOutput(ns("plot_participacion_carrera"))
}

select_ejercicio_var_ui <- function(id){
  ns <- NS(id)
  
  selectInput(ns("ejercicio_var"), "Variable de control:",
              choices = c("Sólo sexo" = "solo_sexo",
                          "Sexo, estado conyugal y maternidad" = "maternidad_e_con"))
}

plot_ejercicio_interactive_ui <- function(id){
  ns <- NS(id)
  
  plotOutput(ns("plot_ejercicio_interactive"))
}

text_ejercicio_summary_ui <- function(id){
  ns <- NS(id)
  
  htmlOutput(ns("plot_ejercicio_summary"))
}

# Server ====
genero_server <- function(id){
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

    # SECTION 1: GENDER PARITY
    output$genero_intro <- renderUI({
      HTML(paste0("Actualmente en México, aproximadamente ",
             span(paste0(perc_muj_educ, "% "), style = "font-weight: bold;"),
             "de las personas que son abogades de formación y ",
             span(paste0(perc_muj_ocup, "% "), style = "font-weight: bold;"),
             "de las personas que ejercen como abogades son mujeres."))
    })
    
    output$plot_paridad_sector <- renderPlot({
      plot <-
        ggplot(data = paridad_genero_sector %>% filter(sector != "General"),
               aes(x = sector, y = perc)) +
        geom_col(aes(fill = sexo, width = sqrt(fac_sum))) +
        geom_text(
          aes(label = paste0(round(perc, 0), "%"), group = sexo),
          position = position_stack(vjust = 0.5),
          color = "#faf4e8",
          fontface = "bold",
          size = 4,
          family = "Roboto"
        ) +
        geom_hline(yintercept = 50,
                   linetype = "dashed",
                   color = "#faf4e8") +
        facet_grid( ~ sector, scales = "free_x", space = "free_x") +
      labs(
        title = "Paridad de género entre las personas abogadas por formación",
        fill = "Sexo",
        caption = paste0(
          caption_enoe_default,
          "\nEl ancho de las columnas corresponde al número total de personas abogadas empleadas en ese sector."
        )
      ) +
        scale_fill_manual(values = pal_hom_muj) +
        tema_abogadas +
        text_size_to_use() + 
        theme(
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_blank()
        )
      
      if (plotWidth() <= 586) {
        
        plot <-
          ggplot(data = paridad_genero_sector %>% filter(sector != "General"),
                 aes(y = sector, x = perc)) +
          geom_col(aes(fill = sexo, width = sqrt(fac_sum))) +
          geom_text(
            aes(label = paste0(round(perc, 0), "%"), group = sexo),
            position = position_stack(vjust = 0.5),
            color = "#faf4e8",
            fontface = "bold",
            size = 4,
            family = "Roboto"
          ) +
          geom_vline(xintercept = 50,
                     linetype = "dashed",
                     color = "#faf4e8") +
          facet_wrap( ~ sector,
                      scales = "free_y",
                      ncol = 1)  +
        labs(
          title = "Paridad de género entre las personas\nabogadas por formación",
          fill = "Sexo",
          caption = paste0(
            caption_enoe_default
          )
        ) +
          scale_fill_manual(values = pal_hom_muj) +
          tema_abogadas +
          text_size_to_use() +
          theme(
            strip.background = element_blank(),
            strip.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(face = "bold"),
            axis.line = element_blank()
          )
      }
      else{
        plot <- plot
      }
      
      return(plot)
    })
    
    output$plot_paridad_edad <- renderPlot({
      plot_title_unwrapped <- "Paridad de género entre las personas abogadas por formación"
      plot_title <- ifelse(plotWidth() > 586, plot_title_unwrapped, str_wrap(plot_title_unwrapped, 40))

      paridad_genero_edad %>% 
        ggplot(data = ., aes(x = edad_midpoint, y = perc, fill = sexo)) +
        geom_area() +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "#faf4e8") +
        labs(
          title = plot_title,
          x = paste0("Edad\n", "(Mayores ", "\u2192", " jóvenes)"), y = "", 
          fill = "Sexo",
          caption = caption_enoe_default) +
        coord_cartesian(xlim = c(80, 20)) +
        scale_fill_manual(values = pal_hom_muj) +
        scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = scales::percent) +
        scale_x_reverse(breaks = seq(20, 80, by = 10)) +
        tema_abogadas +
        text_size_to_use()
    })
    
    output$paridad_edad_text <- renderUI({
      HTML(paste0("Mientras que ",
                  span(paste0(perc_muj_55_59, "% de las personas abogadas entre 55 y 59 años "),
                       style = "font-weight: bold;"),
                  "son mujeres, las mujeres representan ",
                  span(paste0(perc_muj_25_29, "% de las personas entre 25 y 29 años "),
                       style = "font-weight: bold;"),
                  "con esta formación."))
    })
    
    # SECTION 2: WAGE GAP
    output$brecha_salarial_intro <- renderUI({
      HTML(paste0("¿Dentro de la abogacía hombres y mujeres ganan lo mismo? ",
      "En general, por cada peso que gana un abogado hombre, una abogada gana ",
      span(paste0(brecha_general, " centavos "), style = "font-weight: bold;"),
      span("menos", style = "font-weight: bold; font-style: italic;"), "."))
    })
    
    output$plot_brecha_ocup <- renderPlot({
      mobile <- plotWidth() <= 586
      
      plot_title_unwrapped <- "Por cada peso que gana un hombre, ¿cuántos centavos MENOS gana una mujer en la misma profesión?"
      plot_title <- ifelse(plotWidth() > 586, plot_title_unwrapped, str_wrap(plot_title_unwrapped, 35))
      plot_subtitle_unwrapped <- "Valores negativos representan ocupaciones en las cuales las mujeres en promedio ganan más que los hombres"
      plot_subtitle <- ifelse(plotWidth() > 586, plot_subtitle_unwrapped, str_wrap(plot_subtitle_unwrapped, 35))
      
      df <- brecha_sal_por_ocup %>% 
        mutate(ocups_de_interes = case_when(mobile ~ str_wrap(ocups_de_interes, 20),
                                            T ~ ocups_de_interes))
      
      plot <- df %>% 
        ggplot(aes(x = ocups_de_interes, y = 0)) +
        geom_point(aes(size = brecha_centavos_menos,
                       stroke = ifelse(brecha_centavos_menos > 0, 
                                       brecha_centavos_menos/3.5, 
                                       0)), 
                       shape = 21,
                   color = ifelse(mobile, "#B59410", "#bcbcbc"), fill = "#B59410") +
        geom_text(aes(label = paste0(round(brecha_centavos_menos), "¢")),
                  color = ifelse(brecha_sal_por_ocup$brecha_centavos_menos > 0, "#faf4e8", "#242223"),
                  fontface = "bold", size = 4, family="Roboto") +
        scale_x_discrete(limits = case_when(mobile ~ sort(df$ocups_de_interes, decreasing = T),
                                            T ~ df$ocups_de_interes)) + 
        labs(title = plot_title,
             subtitle = plot_subtitle,
             y = "", x = "", caption = caption_enoe_default) +
        tema_abogadas +
        text_size_to_use() +
        theme(legend.position = "none",
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              plot.margin=unit(c(0,0,0,0), "cm"),
              panel.margin=unit(c(0,0,0,0), "cm"),
              plot.title = element_text(face = "bold")
              )
      
      if(plotWidth() > 630){
       plot <- plot + 
         scale_size(range = c(0, 50))  +
         theme(axis.text.y = element_blank(),
)
      }else{
        plot <- plot + 
          coord_flip() +
          scale_size(range = c(0, 9))  +
          theme(axis.text.y = element_text(face = "bold"),
                axis.text.x = element_blank()
                )
      }
      
      return(plot)
    })
    
    output$plot_brecha_interactive <- renderPlot({
      mobile <- plotWidth() <= 586
      
      plot <- plot_brecha_heat_map(input$heatmap_control, mobile) +
        text_size_to_use()
      
      if (plotWidth() <= 586 & input$heatmap_control != "e_con") {
        plot <- plot + 
          coord_flip() +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_text(family = "Barlow",
                                           color = "#242223"))
      }
      
      if(plotWidth() > 586 & input$heatmap_control != "e_con"){
        plot <- plot +  theme(axis.text.y = element_blank())
      } else{
        plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      if(plotWidth() <= 586 & input$heatmap_control %in% c("edad", "sector")){
        plot <- plot +  theme(axis.text.x = element_blank())
      } 
      
      return(plot)
    })
    
    # SECTION 3: LABOR MARKET PARTICIPATION
    output$participacion_intro <- renderUI({
      HTML(paste0("El análisis de la brecha salarial aplica solamente para las abogadas que ejercen esta profesión como un trabajo remunerado. ", 
                  "Pero también hay muchas mujeres que estudiaron Derecho y no participan en el mercado laboral: ",
                  span(paste0(pnea_abogadas, "% de mujeres "), style = "font-weight: bold;"),
                  "con esta formación."))
    })
    
    output$plot_participacion_carrera <- renderPlot({
      plotWidth <- reactive({session$clientData[["output_perfiles-perfiles_dist_plot_width"]]})
      text_size <- ifelse(plotWidth() <= 630, 3, 4)
      
      ggplot(data = participacion_carrera %>% filter(carreras_de_interes != "General"),
             aes(x = carreras_de_interes, y = perc_pea, fill = sexo)) +
        geom_col(position = position_dodge()) +
        geom_errorbar(aes(ymin = ifelse(perc_pea - margin_error >= 0, perc_pea - margin_error, 0),
                          ymax = ifelse(perc_pea + margin_error <= 1, perc_pea + margin_error, 1)),
                      position = position_dodge(0.9), color = "#bcbcbc", width = 0.25) +
        geom_segment(data = brechas_participacion,
                     aes(x = carreras_de_interes, xend = carreras_de_interes,
                         y = mujeres, yend = mujeres + brecha, fill = NULL),
                     color = "#242223", size = 1) +
        geom_text(data = brechas_participacion,
                  aes(y = mujeres + brecha/2, fill = NULL,
                      label = ifelse(brecha == max(brecha),
                                     paste0("Brecha:\n ", round(brecha * 100), "%"),
                                     paste0(round(brecha * 100), "%")),
                      hjust = ifelse(brecha == max(brecha), -0.1, -0.3)),
                  color = "#242223", fontface = "bold", size = text_size, family="Roboto") +
        labs(title = "Participación laboral por carrera universitaria estudiada",
             x = "", y = "Porcentaje que participa en el mercado laboral", fill = "Sexo",
             caption = paste0(caption_enoe_default, "\n",
                              "Intervalos representan intervalos de confianza del 90%.")) +
        guides(color = "none") +
        scale_fill_manual(values = pal_hom_muj) +
        scale_y_continuous(limits = c(0, 1),
                           breaks = seq(0, 1, by = 0.2), labels = scales::percent) +
        scale_x_discrete(labels = function(x) ifelse(x == "Derecho", parse(text = paste0("bold(\n", x, ")")), paste0("\n",as.character(x)))) +
        tema_abogadas +
        text_size_to_use() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 1))
    })
    
    output$participacion_hallazgos <- renderUI({
      HTML(paste0("Aunque ", 
                  span(paste0(participacion_der_hombres, " de cada 10 hombres "), 
                       style = "font-weight: bold;"),
                  "que estudiaron Derecho trabajan, ",
                  "solo ", 
                  span(paste0(participacion_der_mujeres, " de cada 10 mujeres "), 
                       style = "font-weight: bold;"),
                  "con esta formación lo hacen."))
    })
    
    output$participacion_compare <- renderUI({
      HTML(paste0(
        "Entre la población mexicana en general, ",
        participacion_gen_hombres, "% de los hombres y ",
        participacion_gen_mujeres, "% de las mujeres participan en el mercado laboral, ",
        "lo cual quiere decir que las mujeres con estudios universitarios (de cualquier disciplina) participan significativamente más en el trabajo remunerado. ",
        "Sin embargo, incluso entre la población con estudios superiores, siguen existiendo importantes diferencias en tasas de participación por sexo."))
    })
    
    output$plot_ejercicio_interactive <- renderPlot({
      plot <- plot_ejercicio(var_choice = "maternidad_e_con") + 
        text_size_to_use()
      
      if(plotWidth() <= 586){
        plot <- plot +
          geom_text(aes(label = paste0(perc_ejercicio * 100, "%"), y = perc_ejercicio - 0.1, group = maternidad), 
                    color = "#faf4e8", fontface = "bold", size = 4, family="Roboto",
                    position = position_dodge(width = 0.9)) 
      } else{
        plot <- plot +
          geom_text(aes(label = paste0(perc_ejercicio * 100, "%"), y = perc_ejercicio - 0.1, group = maternidad), 
                    color = "#faf4e8", fontface = "plain", size = 6, family="Roboto",
                    position = position_dodge(width = 0.9)) 
      }
      
      return(plot)

    })
    
    output$plot_ejercicio_summary <- renderUI({
      HTML(paste0("En general, sólo aproximadamente ",
                  span(paste0(ejercicio_mujeres, "% de las mujeres "), 
                       style = "font-weight: bold;"), "y ",
                  span(paste0(ejercicio_hombres, "% de los hombres "), 
                       style = "font-weight: bold;"),
                  "que estudiaron Derecho ejercen actualmente en el área. ",
                  "Les demás ejercen otras profesiones o no participan en el mercado de trabajo."))
    })
    
    output$paridad_map <- renderPlot({
      mobile <- ifelse(plotWidth() <= 586, T, F)
      
      plot <- map_paridad(input$def_type_map, mobile) + 
        text_size_to_use() +
        theme(axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              legend.position = "none")
      
      plotWidth <- reactive({session$clientData[["output_perfiles-perfiles_dist_plot_width"]]})
      
      if(plotWidth() <= 586){
        plot <- plot +
          geom_text(aes(x = centroid_long, y = centroid_lat, 
                        label = paste0(state_abbr, "\n",
                                       perc_muj, "%"), 
                        color = color_label),
                    fontface = "plain", family = "Roboto", size = 2.3) 
      }
      else{
        plot <- plot +
          geom_text(aes(x = centroid_long, y = centroid_lat, 
                        label = paste0(state_abbr, "\n",
                                       perc_muj, "%"),
                        color = color_label),
                    fontface = "bold", family = "Roboto", size = 4) 
      }
      return(plot)
    })
    
  })
}

# Demo ====
app_demo <- function(){
  ui <- fluidPage(
    genero_intro_text_ui("demo"),
    paridad_sector_plot_ui("demo"),
    paridad_edad_plot_ui("demo"),
    select_lawyer_def_map_ui("demo"),
    paridad_map_ui("demo"),
    brecha_salarial_intro_text_ui("demo"),
    select_heatmap_var_ui("demo"),
    plot_brecha_interactive_ui("demo"),
    participacion_hallazgos_ui("demo"),
    participacion_compare_ui("demo"),
    plot_participacion_carrera_ui("demo"),
    select_ejercicio_var_ui("demo"),
    plot_ejercicio_interactive_ui("demo"),
    text_ejercicio_summary_ui("demo")
  )
  
  server <- function(input, output, session) {
  genero_server("demo")
    }

shinyApp(ui, server)
}

# done.
