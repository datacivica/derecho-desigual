#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/app/modules/src/grafs-theme-fns.R

pacman::p_load(stringr, ggplot2, dplyr, shiny)

# Define color palettes ====
pal_2 <- c("#45C0B3", "#E39017")
pal_5 <- c(pal_2, "#FF9BE1", "#5135D3", "#EE6244")
pal_7 <- c(pal_5, "#242223", "#c3bbf9")
pal_9 <- c(pal_7, "#3e7f7b", "#8aaff9")

pal_hom_muj <- c("Mujeres" = "#45C0B3", "Hombres" = "#E39017")
pal_abogadas_ocup <- c("Mujeres abogadas" = "#45C0B3",
                  "Hombres abogados" = "#5135d3",
                  "Mujeres no abogadas\ncon estudios universitarios" = "#FF9BE1")
pal_maternidad <- c("Mujeres con hijes" = "#45C0B3",
                    "Hombres" = "#5135d3",
                    "Mujeres sin hijes" = "#FF9BE1")

pal_mat_e_con <- c("Casado o unión libre" = "#45C0B3",
                   "Soltero o divorciado" = "#E39017",
                   "Casada o unión libre, con hijes" = "#379a8f",
                   "Casada o unión libre, sin hijes" = "#6acdc2",
                   "Soltera o divorciada, con hijes" = "#b67312",
                   "Soltera o divorciada, sin hijes" = "#e9a645")

pal_gradient_gold <- c(high = "#e39017", low = "#fcf2e3")
pal_gradient_purple <- c(high = "#5135D3", low = "#ded9f7")

# Define dynamic text size ====
laptop_text_sizes <- theme(
  plot.title = element_text(size = 20),
  plot.subtitle = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 12),
  plot.caption = element_text(size = 9),
  axis.title = element_text(size = 14),
  legend.text = element_text(size = 12),
  strip.text = element_text(size = 12))

mobile_text_sizes <- theme(
  plot.title = element_text(size = 12),
  plot.subtitle = element_text(size = 9),
  axis.text.x = element_text(size = 10),
  axis.text.y = element_text(size = 10),
  plot.caption = element_text(size = 6),
  axis.title = element_text(size = 10),
  legend.text = element_text(size = 9),
  strip.text = element_text(size = 10))

# Define theme ====
tema_abogadas <- theme(
  plot.title = element_text(family = "Barlow Condensed",
                            face = "bold",
                            color = "#242223"),
  plot.subtitle = element_text(family = "Roboto Slab",
                               color = "#212121"),
  axis.text.x = element_text(family = "Roboto Slab",
                             face = "bold",
                             color = "#242223"),
  axis.text.y = element_text(family = "Barlow",
                             color = "#242223"),
  plot.caption = element_text(family = "Roboto Slab",
                              color = "#474446"),
  plot.caption.position = "plot",
  axis.title = element_text(family = "Barlow",
                            color = "#000000"),
  legend.title = element_blank(),
  legend.text = element_text(family = "Roboto Slab",
                              color = "#242223"),
  legend.background = element_rect(fill = "#faf4e8"),
  legend.position = "top",
  legend.justification='left',
  strip.text = element_text(
    family = "Barlow Condensed",
    face = "bold",
    color = "#3d3d3d"),
  strip.background = element_rect(fill = "#faf4e8"),
  axis.line = element_line(color = "#bcbcbc"),
  axis.ticks = element_line(color = "#bcbcbc"),
  panel.background = element_blank(),
  panel.grid = element_blank(),
  plot.background = element_rect(fill = "#faf4e8", color = "#faf4e8"))

# done.
