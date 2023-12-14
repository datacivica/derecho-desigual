ui <- function() {
  tagList(
    bootstrapPage(
      htmlTemplate(filename = "www/template.html",
                   # PERFILES
                   textMostCommonProfile = most_common_profile_text_ui("perfiles"),
                   selectLawyerDef = select_lawyer_def_ui("perfiles"),
                   selectVarInterestPerfiles = select_var_interest_ui("perfiles"),
                   selectCompGroupsPerfiles = select_comp_groups_perfiles_ui("perfiles"),
                   plotPerfilesDist = perfiles_dist_plot_ui("perfiles"),
                   textIncomeIntro = income_intro_text_ui("perfiles"),
                   selectCompGroupsIncome = select_comp_groups_income_ui("perfiles"),
                   plotIncomeDist = income_dist_plot_ui("perfiles"),
                   textWomenIncome = women_income_text_ui("perfiles"),
                   text13kIncome = income_13k_text_ui("perfiles"),
                   plotLawPreval = law_preval_age_plot_ui("perfiles"),
                   textLawPreval = law_preval_text_ui("perfiles"),
                   selectLawyerDefMapPrev = select_lawyer_def_map_ui("perfiles"),
                   selectSexoMapPrev = select_sexo_map_ui("perfiles"),
                   mapLawPreval = law_preval_map_ui("perfiles"),

                   # GÉNERO
                   textGeneroIntro = genero_intro_text_ui("genero"),
                   plotParidadSector = paridad_sector_plot_ui("genero"),
                   plotParidadEdad = paridad_edad_plot_ui("genero"),
                   textParidadEdad = paridad_edad_text_ui("genero"),
                   selectLawyerDefMapParidad = select_lawyer_def_map_ui("genero"),
                   mapParidad = paridad_map_ui("genero"),
                   textBrechaGeneroIntro = brecha_salarial_intro_text_ui("genero"),
                   plotBrechaGeneroOcup = brecha_salarial_ocup_plot_ui("genero"),
                   selectBrechaGeneroVar = select_heatmap_var_ui("genero"),
                   plotBrechaHeatmap = plot_brecha_interactive_ui("genero"),
                   textParticipIntro = participacion_intro_text_ui("genero"),
                   plotParticipCarreraGenero = plot_participacion_carrera_ui("genero"),
                   textParticipHallazgos = participacion_hallazgos_ui("genero"),
                   textParticipCompare = participacion_compare_ui("genero"),
                   selectEjercicioGeneroVar = select_ejercicio_var_ui("genero"),
                   plotEjercicioGenero = plot_ejercicio_interactive_ui("genero"),
                   textEjercicioSummary = text_ejercicio_summary_ui("genero"),

                   # ABOGADAS
                   textSitFamIntro = sit_fam_intro_text_ui("abogadas"),
                   selectSitFamAbgasVar = select_sit_fam_var_ui("abogadas"),
                   plotSitFamAbgas = sit_fam_plot_ui("abogadas"),
                   textIndigPercent = indig_percent_text_ui("abogadas"),
                   textIndigSubrep = indig_subrep_text_ui("abogadas"),
                   plotIndigSubrep = indig_subrep_plot_ui("abogadas"),
                   textIndigBrecha = indig_brecha_text_ui("abogadas"),
                   infografDiscap = discap_infograf_ui("abogadas"),
                   plotDiscapSubrep = discap_subrep_plot_ui("abogadas"),
                   textDiscap = discap_text_ui("abogadas"),
                   textUnivDist = univ_dist_text_ui("abogadas"),
                   plotUnivSector = univ_sector_plot_ui("abogadas"),
                   textUnivBrecha = univ_brecha_text_ui("abogadas"),

                   # CUIDADOS
                   infografCuidados = infographic_cuidados_ui("cuidados"),
                   selectCuidadosResultado = select_cuidados_resultado_ui("cuidados"),
                   selectCuidadosIndicador = select_cuidados_indicador_ui("cuidados"),
                   plotCuidadosOutcomes = plot_cuidados_vs_outcomes_ui("cuidados"),
                   textCuidadosParticip = text_thrs_particip_ui("cuidados"),

                   # VIOLENCIA
                   infografViolencia = infographic_violencia_ui("violencia"),
                   selectViolenciaVar = select_violencia_var_ui("violencia"),
                   plotViolenciaEmbzo = plot_violencia_embzo_ui("violencia"),
                   plotViolenciaAnio = plot_violencia_anio_ui("violencia"),
                   plotViolenciaLab = plot_violencia_lab_ui("violencia"),
                   selectTipoViolencia = select_tipo_violencia_ui("violencia"),
                   plotViolenciaOcup = plot_violencia_ocup_ui("violencia")
                   ),
      theme = bs_theme(version = 5)
    )
  )
}
