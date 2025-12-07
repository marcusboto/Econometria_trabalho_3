#TRABALHO ECONOMETRIA
#PROFESSOR: VITOR PEREIRA
#GRUPO: MARCUS BOTO, PEDRO LATINI


# ================================================================
# CÓDIGO DA QUESTÃO 3.5: IMPACTO DO MONITORAMENTO
# ================================================================



# 1. Regressão simples

reg1 <- lm(mom_unemp ~ o_math, data = Maimonides_italia)
reg1_cluster <- coeftest(reg1,
                         vcov = vcovCL(reg1, cluster = Maimonides_italia$CLUSTER))

# 2. Regressão com controles
reg2 <- lm(mom_unemp ~ o_math + female + immigrants_broad +
             dad_lowedu + dad_midedu,
           data = Maimonides_italia)

reg2_cluster <- coeftest(reg2,
                         vcov = vcovCL(reg2, cluster = Maimonides_italia$CLUSTER))
# 3. Regressão com efeitos fixos
###############################################################
reg3 <- lm(mom_unemp ~ o_math + factor(area),
           data = Maimonides_italia)

reg3_cluster <- coeftest(reg3,
                         vcov = vcovCL(reg3, cluster = Maimonides_italia$CLUSTER))

# 4. Interação

reg4 <- lm(mom_unemp ~ o_math + factor(area) * enrol_ins_snv,
           data = Maimonides_italia)

reg4_cluster <- coeftest(reg4,
                         vcov = vcovCL(reg4, cluster = Maimonides_italia$CLUSTER))

# 5. Tabela HTML
stargazer(reg1, reg2, reg3, reg4,
          type = "html",
          out = "balanceamento_ex35.html",
          title = "output/Exercício 3.5 — Balanceamento")

# Mostrar no console
reg1_cluster
reg2_cluster
reg3_cluster
reg4_cluster
