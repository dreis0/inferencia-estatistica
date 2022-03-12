#Intervalo de confiança da média
#Intervalo de confiança do desvio padrão

#Intervalo de confiança da média entre pais mais baixos que mediana
#Intervalo de confiança do desvio padrão entre pais mais baixos que mediana

#Intervalo de confiança da média entre pais mais altos que mediana
#Intervalo de confiança do desvio padrão entre pais mais altos que mediana

#Usar bootstrap

table = read.table(
  "https://drive.google.com/u/1/uc?id=19NU_6wb8ZVg00AJ0C4Vcx3saGlrby4Zv&export=download",
  header = TRUE
)

pais = table$father * 2.54
filhos = table$son * 2.54

pai_menos_filho = pais - filhos #Diferença entra alturas dos pais e dos filhos

bootstrap_mean_all = vector()

for (i in 1:10000) {
  amostra = sample(pai_menos_filho, length(pai_menos_filho), replace = TRUE)
  bootstrap_mean_all[i] = mean(amostra)
}

bootstrap_sd_all = vector()

for (i in 1:10000) {
  amostra = sample(pai_menos_filho, length(pai_menos_filho), replace = TRUE)
  bootstrap_sd_all[i] = sd(amostra)
}

mediana_altura_pais = median(pais)

idx_pais_menor_mediana = pais < mediana_altura_pais

pais_menos_filhos_menor_que_mediana = pais[idx_pais_menor_mediana] - filhos[idx_pais_menor_mediana]

bootstrap_mean_menor_mediana = vector()

for (i in 1:10000) {
  amostra = sample(
    pais_menos_filhos_menor_que_mediana,
    length(pais_menos_filhos_menor_que_mediana),
    replace = TRUE
  )
  bootstrap_mean_menor_mediana[i] = mean(amostra)
}

bootstrap_sd_menor_mediana = vector()

for (i in 1:10000) {
  amostra = sample(
    pais_menos_filhos_menor_que_mediana,
    length(pais_menos_filhos_menor_que_mediana),
    replace = TRUE
  )
  bootstrap_mean_menor_mediana[i] = sd(amostra)
}

idx_pais_maior_mediana = pais > mediana_altura_pais

pais_menos_filhos_maior_que_mediana = pais[idx_pais_maior_mediana] - filhos[idx_pais_maior_mediana]

bootstrap_mean_maior_mediana = vector()

for (i in 1:10000) {
  amostra = sample(
    pais_menos_filhos_maior_que_mediana,
    length(pais_menos_filhos_maior_que_mediana),
    replace = TRUE
  )
  bootstrap_mean_maior_mediana[i] = mean(amostra)
}

bootstrap_sd_maior_mediana = vector()

for (i in 1:10000) {
  amostra = sample(
    pais_menos_filhos_maior_que_mediana,
    length(pais_menos_filhos_maior_que_mediana),
    replace = TRUE
  )
  bootstrap_mean_maior_mediana[i] = sd(amostra)
}

ic_95_mean_all = quantile(bootstrap_mean_all, c(0.025, 0.975))
ic_95_sd_all = quantile(bootstrap_sd_all, c(0.025, 0.975))
ic_95_mean_menor_mediana = quantile(bootstrap_mean_menor_mediana, c(0.025, 0.975))
ic_95_sd_menor_mediana = quantile(bootstrap_sd_menor_mediana, c(0.025, 0.975))
ic_95_mean_maior_mediana = quantile(bootstrap_mean_maior_mediana, c(0.025, 0.975))
ic_95_sd_maior_mediana = quantile(bootstrap_sd_maior_mediana, c(0.025, 0.975))

ic_95_mean_all
ic_95_sd_all
ic_95_mean_menor_mediana
ic_95_sd_menor_mediana
ic_95_mean_maior_mediana
ic_95_sd_maior_mediana
