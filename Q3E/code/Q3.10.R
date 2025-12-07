

# 1. Pela regra italiana (serão nossos cutoffs)
cutoff_baixo <- 22  # Financia turma adicional
cutoff_alto <- 28   # Exige nova turma

# 2. Criar categorias
Maimonides_italia <- Maimonides_italia %>%
  mutate(
    categoria = case_when(
      clsize_hat < cutoff_baixo ~ "Abaixo de 22",
      clsize_hat >= cutoff_baixo & clsize_hat < cutoff_alto ~ "Entre 22 e 28",
      clsize_hat >= cutoff_alto ~ "28 ou mais"
    ),
    # Dummy
    acima_22 = ifelse(clsize_hat >= cutoff_baixo, 1, 0),
    acima_28 = ifelse(clsize_hat >= cutoff_alto, 1, 0)
  )

# 3. Scatterplot agrupado
dados_plot <- Maimonides_italia %>%
  filter(!is.na(categoria)) %>%
  group_by(clsize_hat, categoria) %>%
  summarise(
    mean_score = mean(answers_math_std, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

# 4. Regressões para cada segmento
# Segmento 1: Abaixo de 22
reg_abaixo22 <- lm(answers_math_std ~ clsize_hat,
                   data = Maimonides_italia %>% 
                     filter(categoria == "Abaixo de 22"))

# Segmento 2: Entre 22 e 28
reg_entre22_28 <- lm(answers_math_std ~ clsize_hat,
                     data = Maimonides_italia %>% 
                       filter(categoria == "Entre 22 e 28"))



# 5. Linhas previstas para cada segmento
# Abaixo de 22
x_abaixo <- seq(min(dados_plot$clsize_hat[dados_plot$categoria == "Abaixo de 22"]),
                cutoff_baixo - 0.1,
                length.out = 100)
pred_abaixo <- data.frame(clsize_hat = x_abaixo)
pred_abaixo$pred <- predict(reg_abaixo22, newdata = pred_abaixo)

# Entre 22 e 28
x_entre <- seq(cutoff_baixo,
               cutoff_alto - 0.1,
               length.out = 100)
pred_entre <- data.frame(clsize_hat = x_entre)
pred_entre$pred <- predict(reg_entre22_28, newdata = pred_entre)

# 28 ou mais (se houver dados)
if (sum(Maimonides_italia$categoria == "28 ou mais", na.rm = TRUE) > 5) {
  x_acima <- seq(cutoff_alto,
                 max(dados_plot$clsize_hat[dados_plot$categoria == "28 ou mais"]),
                 length.out = 100)
  pred_acima <- data.frame(clsize_hat = x_acima)
  pred_acima$pred <- predict(reg_acima28, newdata = pred_acima)
} else {
  pred_acima <- NULL
}

# 6. Gráfico completo
p <- ggplot() +
  # Pontos coloridos por categoria
  geom_point(data = dados_plot,
             aes(x = clsize_hat, y = mean_score, color = categoria, size = n_obs),
             alpha = 0.5) +
  
  # Linhas de regressão para cada segmento
  geom_line(data = pred_abaixo, aes(x = clsize_hat, y = pred),
            color = "#1f77b4", size = 1.2) +  # Azul
  
  geom_line(data = pred_entre, aes(x = clsize_hat, y = pred),
            color = "#ff7f0e", size = 1.2) +  # Laranja
  
  # Linhas verticais dos cutoffs
  geom_vline(xintercept = c(cutoff_baixo, cutoff_alto),
             linetype = "dashed", size = 1, alpha = 0.7) +
  
  # Anotações dos cutoffs
  annotate("text", x = cutoff_baixo, y = max(dados_plot$mean_score),
           label = "Cutoff = 22\n(Financia turma adicional)",
           hjust = 1.1, vjust = 1, size = 3.5, fontface = "bold") +
  
  annotate("text", x = cutoff_alto, y = max(dados_plot$mean_score),
           label = "Cutoff = 28\n(Exige nova turma)",
           hjust = -0.1, vjust = 1, size = 3.5, fontface = "bold") +
  
  # Linha para acima de 28 (se existir)
  if (!is.null(pred_acima)) {
    p <- p + geom_line(data = pred_acima, aes(x = clsize_hat, y = pred),
                       color = "#2ca02c", size = 1.2)  # Verde
  }

# Estética
labs(title = "RDD com Múltiplos Cutoffs: Notas vs Tamanho Predito",
     subtitle = "Regra italiana: cutoff em 22 (financiamento) e 28 (obrigatório)",
     x = "Tamanho Predito da Classe (clsize_hat)",
     y = "Nota média padronizada (answers_math_std)",
     color = "Faixa de tamanho",
     size = "Nº de observações") +
  scale_color_manual(values = c("Abaixo de 22" = "#1f77b4",
                                "Entre 22 e 28" = "#ff7f0e",
                                "28 ou mais" = "#2ca02c")) +
  scale_size_continuous(range = c(1, 4)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11))

print(p)

# 7. Resultados das regressões
cat("\n=== RESULTADOS DAS REGRESSÕES POR SEGMENTO ===\n")

cat("\n1. ABAIXO DE 22:\n")
print(summary(reg_abaixo22))

cat("\n2. ENTRE 22 E 28:\n")
print(summary(reg_entre22_28))

cat("\n3. 28 OU MAIS:\n")
if (exists("reg_acima28") && !is.null(reg_acima28)) {
  print(summary(reg_acima28))
} else {
  cat("Poucas observações para estimar\n")
}

# 8. Estatísticas descritivas por categoria
cat("\n=== ESTATÍSTICAS POR CATEGORIA ===\n")
stats_categorias <- Maimonides_italia %>%
  group_by(categoria) %>%
  summarise(
    n = n(),
    media_nota = mean(answers_math_std, na.rm = TRUE),
    media_tamanho = mean(clsize_hat, na.rm = TRUE),
    .groups = "drop"
  )
print(stats_categorias)

# 9. Salvar gráfico
ggsave("output/rdd_multiplos_cutoffs.png", p, width = 11, height = 7, dpi = 300)

# 10. Tabela para relatório (opcional)
stargazer(reg_abaixo22, reg_entre22_28, 
          type = "html",
          title = "Regressões RDD por Segmento de Tamanho de Turma",
          dep.var.labels = "Nota padronizada em matemática",
          covariate.labels = c("Tamanho predito da turma"),
          column.labels = c("Abaixo de 22", "Entre 22-28"),
          out = "output/regressoes_rdd.html")