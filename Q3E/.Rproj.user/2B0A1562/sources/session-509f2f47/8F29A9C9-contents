#TRABALHO ECONOMETRIA
#PROFESSOR: VITOR PEREIRA
#GRUPO: MARCUS BOTO, PEDRO LATINI


# ================================================================
# CÓDIGO DA QUESTÃO 3.6: REGRESSÃO COM INTERAÇÃO REGIONAL
# ================================================================

library(stargazer)
library(dplyr)

# Variável Dependente:
VD <- "our_CHEAT_ital"

reg_interacao_fraude <- lm(
  as.formula(paste(VD, "~ o_math * factor(area) + factor(grade)")),
  data = Maimonides_italia,
  na.action = na.omit
)

# Geração da Tabela Stargazer
stargazer(reg_interacao_fraude,
          type = "html", # Altere para "latex" para Overleaf
          title = "Impacto do Monitoramento sobre a Fraude: Análise de Interação Regional",
          dep.var.labels = c("Fraude em Italiano"),
          # Nota: Os labels para as interações são gerados automaticamente pelo R.
          covariate.labels = c("Monitoramento (Norte Ref.)", 
                               "Região Central (Nível Base)", "Região Sul (Nível Base)",
                               "Série (Grade 2)", "Série (Grade 3)", "Série (Grade 4)",
                               "Interação: Monit. x Centro", "Interação: Monit. x Sul"),
          out = "monitoramento_regional_interacao.html",
          add.lines = list(c("Referência Regional", "Norte"),
                           c("Referência de Série", "Grade 1")),
          digits = 4)