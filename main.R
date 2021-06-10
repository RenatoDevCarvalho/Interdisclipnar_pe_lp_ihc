library(tidyverse)
library(gt)
library(ggplot2)

materias = c("Ciências da Natureza", "Ciências Humanas", "Linguagens e Códigos", "Matemática", "Redação")

enem = read.csv("dadosEnem.csv", TRUE, ";")

enem = as_tibble(enem) %>%
  rename("Ciências da Natureza" = NU_MEDIA_CN, 
         "Ciências Humanas" = NU_MEDIA_CH,
         "Linguagens e Códigos" = NU_MEDIA_LP,
         "Matemática" = NU_MEDIA_MT,
         "Redação" = NU_MEDIA_RED,)

estados = enem %>%
  distinct(CO_UF_ESCOLA, SG_UF_ESCOLA) %>%
  arrange(SG_UF_ESCOLA) %>%
  rename(Estados = SG_UF_ESCOLA)

print(gt(estados))

escolhaEstado = readline(prompt = "Escolha o estado: ")

print("1 - Ciências da Natureza")
print("2 - Ciências Humanas")
print("3 - Linguagens e Códigos")
print("4 - Matemática")
print("5 - Redação")

correlacao = readline(prompt = "Escolha as variaveis para correlação: ")

correlacao = as.list(strsplit(correlacao, " ")[[1]])

for (i in seq(1, length(correlacao), by = 2)) {
  medias = enem %>%
    filter(NU_ANO == 2015 & CO_UF_ESCOLA == strtoi(escolhaEstado)) %>%
    select(NO_ESCOLA_EDUCACENSO, materias[strtoi(correlacao[i])], materias[strtoi(correlacao[i+1])]) %>%
    arrange(NO_ESCOLA_EDUCACENSO) %>%
    rename(Escola = NO_ESCOLA_EDUCACENSO, x = materias[strtoi(correlacao[i])], y = materias[strtoi(correlacao[i+1])]) %>%
    mutate(x.y = x * y, x.x = x^2)
  
  print(
    ggplot(medias, aes(x, y)) +
      geom_point() +
      geom_smooth(method = "lm", col = "red") +
      theme_classic() +
      labs(
        title = paste("Média em", materias[strtoi(correlacao[i])] , "X Média em", materias[strtoi(correlacao[i+1])]),
        x = materias[i],
        y = materias[i+1]
      )
  )
  
  print(paste("Correlação entre", materias[strtoi(correlacao[i])], "e", materias[strtoi(correlacao[i+1])], ":", cor(medias$x, medias$y, method = "pearson")))
  
  readline(prompt = "Próxima correlação(Enter)")
}

print("1 - Ciências da Natureza")
print("2 - Ciências Humanas")
print("3 - Linguagens e Códigos")
print("4 - Matemática")
print("5 - Redação")

correlacao = readline(prompt = "Escolha a variavel resposta e as variaveis independentes: ")

correlacao = as.list(strsplit(correlacao, " ")[[1]])

medias = enem %>%
  filter(NU_ANO == 2015 & CO_UF_ESCOLA == strtoi(escolhaEstado)) %>%
  select(materias[strtoi(correlacao)])

fit = lm(pull(medias[1]) ~ pull(medias[2:length(medias)]), data = medias)