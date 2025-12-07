#TRABALHO ECONOMETRIA
#PROFESSOR: VITOR PEREIRA
#GRUPO: MARCUS BOTO, PEDRO LATINI


# ===============================================================================
# CÓDIGO DA QUESTÃO 3.16: IV COM DOIS INSTRUMENTOS: REGRA DE MAIMÔNIDES + SORTEIO
# ===============================================================================

# 1. Importar bibliotecas
library(AER)
library(dplyr)

# 2. Controles
controles <- "female + immigrants_broad + dad_lowedu + dad_midedu + dad_highedu +
              mom_unemp + mom_housew + mom_employed + north + centre + south"

# 3. Fórmula IV
form_iv <- as.formula(
  paste0(
    "answers_math_std ~ clsize_snv + our_CHEAT_math + ", controles,
    " | ",
    "clsize_hat + o_math + ", controles
  )
)

# 4. Estimação
reg_maimonides2 <- ivreg(
  form_iv,
  data = Maimonides_italia,
  cluster = "cluster_id"
)

summary(reg_maimonides2)

# Stargazer da comparação com o modelo anterior
stargazer(
  reg_maimonides,
  reg_maimonides2,
  type = "html",
  out = "iv.html",
  title = "IV: Interação entre Tamanho da Turma e Manipulação"
)
