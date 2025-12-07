#TRABALHO ECONOMETRIA
#PROFESSOR: VITOR PEREIRA
#GRUPO: MARCUS BOTO, PEDRO LATINI


# ================================================================
# CÓDIGO DA QUESTÃO 3.1: Características da Amostra e Estatísticas Descritivas
# ================================================================



Maimonides_italia <- read.dta13(file.path("input/smallmo.dta")) #Usa read.ddta13 para ober dados a partir do input

variaveis_descritivas <- Maimonides_italia %>% #Escolhe as estatC-sticas designadas
  select(
    answers_math_std,    
    answers_ital_std,    
    female,              
    immigrants_broad,    
    dad_lowedu,          
    dad_midedu,          
    mom_unemp,           
    answers_math_pct,    
    answers_ital_pct,    
    our_CHEAT_ital,      
    our_CHEAT_math       
  )

stargazer(variaveis_descritivas,  #USA PACOTE STARGAZER PARA GERAR A TABELA
          type = "html",
          title = "EstatC-sticas Descritivas - Dados Italianos",
          digits = 3,
          covariate.labels = c(
            "Nota Padronizada - MatemC!tica",
            "Nota Padronizada - Italiano",
            "Sexo Feminino (Dummy)",
            "Imigrante (Dummy)",
            "Pai com Baixa EducaC'C#o",
            "Pai com EducaC'C#o MC)dia", 
            "MC#e Desempregada",
            "Percentil - MatemC!tica",
            "Percentil - Italiano",
            "IndC-cio ManipulaC'C#o - Italiano",
            "IndC-cio ManipulaC'C#o - MatemC!tica"
          ),
          summary.stat = c("n", "mean", "median", "sd", "min", "max"),
          out = file.path(output, "questao3_1_estatisticas_descritivas.html"))