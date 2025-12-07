#TRABALHO ECONOMETRIA
#PROFESSOR: VITOR PEREIRA
#GRUPO: MARCUS BOTO, PEDRO LATINI

# ================================================================
# 3.11: RDD com DOIS CUTOFFS (22 e 28)
# ================================================================

library(dplyr)
library(ggplot2)

CUTOFF_BAIXO <- 22  # Financia turma adicional
CUTOFF_ALTO <- 28   # Exige nova turma

# 1. Criar variável com duas categorias
Maimonides_italia <- Maimonides_italia %>%
  mutate(
    categoria = case_when(
      clsize_hat < CUTOFF_BAIXO ~ "Abaixo de 22",
      clsize_hat >= CUTOFF_BAIXO & clsize_hat < CUTOFF_ALTO ~ "Entre 22 e 28",
    )
  )

# 2. Scatterplot agrupado
dados_plot <- Maimonides_italia %>%
  group_by(clsize_hat) %>%
  summarise(
    mean_score = mean(answers_math_std, na.rm = TRUE),
    categoria = first(categoria),
    .groups = "drop"
  )

# 3. Regressões para CADA segmento

# Segmento 1: Abaixo de 22
if (sum(Maimonides_italia$categoria == "Abaixo de 22", na.rm = TRUE) > 10) {
  reg_abaixo22 <- lm(answers_math_std ~ clsize_hat,
                     data = Maimonides_italia %>% 
                       filter(categoria == "Abaixo de 22"))
} else {
  reg_abaixo22 <- NULL
}

# Segmento 2: Menor que 28
if (sum(Maimonides_italia$categoria == "Entre 22 e 28", na.rm = TRUE) > 10) {
  reg_entre22_28 <- lm(answers_math_std ~ clsize_hat,
                       data = Maimonides_italia %>% 
                         filter(categoria == "Entre 22 e 28"))
} else {
  reg_entre22_28 <- NULL

}

# 4. LINHAS DE PREDIÇÃO (FALTANDO NO SEU CÓDIGO)
# Abaixo de 22
if (!is.null(reg_abaixo22)) {
  linha_abaixo <- data.frame(
    clsize_hat = seq(
      min(dados_plot$clsize_hat[dados_plot$categoria == "Abaixo de 22"]),
      CUTOFF_BAIXO - 0.1,
      length.out = 100
    )
  )
  linha_abaixo$pred <- predict(reg_abaixo22, newdata = linha_abaixo)
} else {
  linha_abaixo <- NULL
}

# Menor que 28
if (!is.null(reg_entre22_28)) {
  linha_entre <- data.frame(
    clsize_hat = seq(
      CUTOFF_BAIXO,
      CUTOFF_ALTO - 0.1,
      length.out = 100
    )
  )
  linha_entre$pred <- predict(reg_entre22_28, newdata = linha_entre)
} else {
  linha_entre <- NULL
}


# 5. Gráfico com DOIS cutoffs
p <- ggplot() +
  # Pontos coloridos por categoria
  geom_point(data = dados_plot,
             aes(x = clsize_hat, y = mean_score, color = categoria),
             alpha = 0.5, size = 2) +
  
  # Linhas verticais dos DOIS cutoffs
  geom_vline(xintercept = c(CUTOFF_BAIXO, CUTOFF_ALTO),
             linetype = "dashed", size = 1, alpha = 0.7, color = "gray50") +
  
  # Anotações
  annotate("text", x = CUTOFF_BAIXO, y = max(dados_plot$mean_score),
           label = "Cutoff = 22", hjust = 1.1, vjust = 1, 
           size = 4, fontface = "bold") +
  
  annotate("text", x = CUTOFF_ALTO, y = max(dados_plot$mean_score),
           label = "Cutoff = 28", hjust = -0.1, vjust = 1,
           size = 4, fontface = "bold") +
  
  # Estética
  labs(title = "RDD com Dois Cutoffs: Notas vs Tamanho Predito",
       subtitle = "Regra italiana: 22 (financiamento) e 28 (obrigatório)",
       x = "Tamanho Predito da Turma (clsize_hat)",
       y = "Nota média (answers_math_std)",
       color = "Faixa de tamanho") +
  scale_color_manual(values = c("Abaixo de 22" = "#1f77b4",
                                "Entre 22 e 28" = "#ff7f0e",
                                "28 ou mais" = "#2ca02c")) +
  theme_minimal()

# 6. Adicionar linhas de regressão 
if (!is.null(linha_abaixo)) {
  p <- p + geom_line(data = linha_abaixo,
                     aes(x = clsize_hat, y = pred),
                     color = "#1f77b4", size = 1.2)
}

if (!is.null(linha_entre)) {
  p <- p + geom_line(data = linha_entre,
                     aes(x = clsize_hat, y = pred),
                     color = "#ff7f0e", size = 1.2)
}


print(p)

# 7. Salvar gráfico
ggsave("output/rdd_dois_cutoffs.png", p, width = 10, height = 6, dpi = 300)
