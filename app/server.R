server <- function(input, output, session) {
  perfiles_server("perfiles")
  genero_server("genero")
  abogadas_server("abogadas")
  cuidados_server("cuidados")
  violencia_server("violencia")
}