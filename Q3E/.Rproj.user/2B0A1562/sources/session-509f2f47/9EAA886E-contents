#TRABALHO ECONOMETRIA
#PROFESSOR: VITOR PEREIRA
#GRUPO: MARCUS BOTO, PEDRO LATINI


# ================================================================
# 3.2 - Cálculo de médias de variáveis por região italiana e série
# ================================================================


medias_por_regiao_serie <- Maimonides_italia %>% #Calcula usando pacote dplyr
  group_by(area, grade) %>%
  summarise(
    media_answers_math_std = mean(answers_math_std, na.rm = TRUE),
    media_answers_ital_std = mean(answers_ital_std, na.rm = TRUE),
    media_female = mean(female, na.rm = TRUE),
    media_immigrants_broad = mean(immigrants_broad, na.rm = TRUE),
    media_dad_lowedu = mean(dad_lowedu, na.rm = TRUE),
    media_dad_midedu = mean(dad_midedu, na.rm = TRUE),
    media_mom_unemp = mean(mom_unemp, na.rm = TRUE),
    media_answers_math_pct = mean(answers_math_pct, na.rm = TRUE),
    media_answers_ital_pct = mean(answers_ital_pct, na.rm = TRUE),
    media_our_CHEAT_ital = mean(our_CHEAT_ital, na.rm = TRUE),
    media_our_CHEAT_math = mean(our_CHEAT_math, na.rm = TRUE)
  )

print(medias_por_regiao_serie)

stargazer(medias_por_regiao_serie, #Tabela Stargazer
          type = "html",
          title = "Médias por região e série",
          summary = FALSE,
          rownames = FALSE,
          digits = 2,
          out = "output/tabela_medias.html")  # salva em output
