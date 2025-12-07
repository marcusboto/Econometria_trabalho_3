#TRABALHO ECONOMETRIA
#PROFESSOR: VITOR PEREIRA
#GRUPO: MARCUS BOTO, PEDRO LATINI


# ================================================================
# CÓDIGO DA QUESTÃO 3.7: REGRESSÃO COM INTERAÇÃO REGIONAL
# ================================================================

library(stargazer)
library(dplyr)

# Variável Dependente
VD <- "answers_math_std"

Maimonides_italia <- Maimonides_italia %>%
  mutate(is_sul = ifelse(area == 3, 1, 0)) # 1 se for Sul, 0 caso contrário

reg_interacao_notas <- lm(
  answers_math_std ~ o_math * is_sul + factor(area) + factor(grade),
  data = Maimonides_italia,
  na.action = na.omit
)

# Geração da Tabela Stargazer
stargazer(reg_interacao_notas,
          type = "html", # Altere para "latex" para Overleaf
          title = "Impacto do Monitoramento nas Notas: Efeito Diferencial no Sul",
          dep.var.labels = c("Nota de Matemática (Std)"),
          # Nota: O termo de interação 'o_math:is_sul' é o coeficiente chave (delta 1).
          covariate.labels = c("Monitoramento (Norte Ref.)", 
                               "Sul (Dummy de Nível)", 
                               "Região Central (Dummy)", "Região Sul (Dummy - Base)", # Dummies da área
                               "Série (Grade 2)", "Série (Grade 3)", "Série (Grade 4)",
                               "Interação: Monit. x Sul (Diferença)"),
          out = "notas_regional_interacao.html",
          add.lines = list(c("Referência Regional", "Norte"),
                           c("Referência de Série", "Grade 1")),
          digits = 4)