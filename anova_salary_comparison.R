# QUEST�O 4) A Yumi Company realiza pesquisas de dados salariais e apresenta os resumos em seu site.
# Com base nos dados salariais de 12 de outubro de 2020, a Yumi divulgou que o sal�rio m�dio anual
# dos vice-presidente de vendas era R$ 142111, com uma m�dia de bonifica��o anual de R$ 15832.
# Suponha que os dados seguintes sejam uma amostra do sal�rio anual e das bonifica��es de dez vice-presidentes
# de vendas. Os dados est�o expressos em milhares de reais.
#
# Vice-Presidente: 1, 2, 3, 4, 5, 6, 7, 8, 9 e 10
# Sal�rio: 135, 115, 146, 167, 165, 176, 98, 136, 163 e 119
# Bonifica��es:  12, 14, 16, 19, 22, 24, 7, 17, 18 e 11

vice_presidentes = seq(1, 10)
salarios = c(135, 115, 146, 167, 165, 176, 98, 136, 163, 119)
bonificacoes = c(12, 14, 16, 19, 22, 24, 7, 17, 18, 11)

# (a.1) Desenvolva um diagrama de dispers�o desses dados, sendo o sal�rio a vari�vel independente.

plot(salarios, bonificacoes, xlab = 'Sal�rios', ylab = 'Bonifica��es')

# (b.1) Use o m�todo dos m�nimos quadrados para desenvolver a equa��o de regress�o estimada.

mean_salario = mean(salarios)
mean_bonificacoes = mean(bonificacoes)

numerador = sum((salarios - mean_salario) * (bonificacoes - mean_bonificacoes))
denominador = sum((salarios - mean_salario) ^ 2)

beta = numerador / denominador
alfa = mean_bonificacoes - beta * mean_salario

plot(
  salarios,
  bonificacoes,
  xlim = range(salarios) + c(-1, 1) ,
  ylim = range(boni) + c(-1, 1),
  xlab = 'Sal�rios',
  ylab = 'Bonifica��es',
  lwd = 2
)

abline(alfa, beta, lwd = 2)

# (b.3) Preveja uma bonifica��o para um vice-presidente de vendas que recebe um sal�rio anual de R$ 120 mil.

mean_salario = mean(salarios)
mean_bonificacoes = mean(bonificacoes)

numerador = sum((salarios - mean_salario) * (bonificacoes - mean_bonificacoes))
denominador = sum((salarios - mean_salario) ^ 2)

beta = numerador / denominador
alfa = mean_bonificacoes - beta * mean_salario

x = seq(min(salarios), max(salarios))
y = x * beta + alfa

y[match(120, x)]


# (c) A inclina��o da equa��o � estatisticamente maior que 0? Qual o p-valor?

bootstrap_alfas = vector()
bootstrap_betas = vector()

for (i in 1:10000) {
  idxs_amostra = sample(1:length(salarios), length(salarios), replace = TRUE)
  bootstrap_salarios = salarios[idxs_amostra]
  bootstrap_bonificacoes = bonificacoes[idxs_amostra]
  
  fit = lm(bootstrap_bonificacoes ~ bootstrap_salarios)
  coeficientes = fit[["coefficients"]]
  coeficientes
  bootstrap_alfas[i] = coeficientes[1]
  bootstrap_betas[i] = coeficientes[2]
}

intervalo_confianca_beta = quantile(bootstrap_betas, c(0.025, 0.975))
pvalor_beta = mean(bootstrap_betas > 0) * 2
length(bootstrap_betas > 0)



calculaF = function(x, y) {
  ybar = mean(y)
  n = length(y)
  fit = lm(y ~ x)
  yhat = fit[["coefficients"]][1] + fit[["coefficients"]][2] * x
  MSb = sum((yhat - ybar) ^ 2) / 1
  MSw = sum((y - yhat) ^ 2) / (n - 2)
  Fobs = MSb / MSw
  
  return(Fobs)
}

# (d) A equa��o da regress�o explica estatisticamente melhor do que a m�dia simples? Qual o p-valor?

f = calculaF(salarios, bonificacoes)

permutacao_fs = vector()
permutacao_betas = vector()

for (i in 1:10000) {
  idxs_amostra = sample(1:length(salarios), length(salarios), replace = TRUE)
  permutacao_bonificacoes = bonificacoes[idxs_amostra]
  
  fit = lm(permutacao_bonificacoes ~ salarios)
  coeficientes = fit[["coefficients"]]
  
  permutacao_fs[i] = calculaF(salarios, permutacao_bonificacoes)
  permutacao_betas[i] = coeficientes[2]
}


hist(
  permutacao_fs ,
  prob = TRUE,
  breaks = seq(0, max(permutacao_fs), length = 50) ,
  col = 'gray',
  xlim = c(0, max(permutacao_fs)) ,
  ylim = c(0, 1.5)
)
par(new = TRUE)
plot(
  f * c(1, 1),
  c(0, 1.5),
  'l',
  lwd = 2,
  col = 'red',
  xlim = c(0, max(permutacao_fs)) ,
  ylim = c(0, 1.5) ,
  axes = FALSE,
  xlab = '',
  ylab = ''
)
mean(permutacao_fs >= f)
