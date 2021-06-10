if (!require("tidyverse")) install.packages("tidyverse")
if (!require("gt")) install.packages("gt")
if (!require("ggplot2")) install.packages("ggplot2")

materias = c("Ciencias da Natureza", "Ciencias Humanas", "Linguagens e Codigos", "Matematica", "Redacao")

enem = read.csv("dadosEnem.csv", TRUE, ";")

enem = as_tibble(enem) %>%
  rename("Ciencias da Natureza" = NU_MEDIA_CN, 
         "Ciencias Humanas" = NU_MEDIA_CH,
         "Linguagens e Codigos" = NU_MEDIA_LP,
         "Matematica" = NU_MEDIA_MT,
         "Redacao" = NU_MEDIA_RED,)

estados = enem %>%
  distinct(CO_UF_ESCOLA, SG_UF_ESCOLA) %>%
  arrange(SG_UF_ESCOLA) %>%
  rename(Estados = SG_UF_ESCOLA, Cod. = CO_UF_ESCOLA)

print(gt(estados))

escolhaEstado = readline(prompt = "Escolha o estado: ")

print("1 - Ciencias da Natureza")
print("2 - Ciencias Humanas")
print("3 - Linguagens e Codigos")
print("4 - Matematica")
print("5 - Redacao")

correlacao = readline(prompt = "Escolha as variaveis para correlacao: ")

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
        title = paste("Media em", materias[strtoi(correlacao[i])] , "X Media em", materias[strtoi(correlacao[i+1])]),
        x = materias[i],
        y = materias[i+1]
      )
  )
  
  print(paste("correlacao entre", materias[strtoi(correlacao[i])], "e", materias[strtoi(correlacao[i+1])], ":", cor(medias$x, medias$y, method = "pearson")))
  
  readline(prompt = "Continuar(Enter)")
}

print("1 - Ciencias da Natureza")
print("2 - Ciencias Humanas")
print("3 - Linguagens e Codigos")
print("4 - Matematica")
print("5 - Redacao")

correlacao = readline(prompt = "Escolha a variavel resposta e as variaveis independentes: ")

correlacao = as.list(strsplit(correlacao, " ")[[1]])

medias = enem %>%
  filter(NU_ANO == 2015 & CO_UF_ESCOLA == strtoi(escolhaEstado)) %>%
  select(materias[strtoi(correlacao)])

fit = lm(pull(medias[1]) ~ pull(medias[2:length(medias)]), data = medias)

print(fit)
