# install.packages("sn")
# install.packages("fGarch")
# install.packages("hnp")

# packageDescription("sn")
# packageDescription("fGarch")
# packageDescription("hnp")

rm(list = ls)
require (sn)
require (fGarch)
require (hnp)

setwd("G:/Meu Drive/DOCUMENTOS/ANDRÉ/MESTRADO UFAC PPGCC/DISCIPLINAS/ESTATÍSTICA COMPUTACIONAL/ATIVIDADE ENVELOPE SIMULADO")
dados <- read.table("A_FatCruz_ex1.txt", h = T)
y <- dados[,2]

n = 10
#n = 30
#n = 100
mu = 100
sigma = 40

#y <- rnorm(n, mu, sigma)

y
mean(y)
sd(y)
hist(y)

logvero <- function(param, y) {
  mu = param[1]
  sigma = param[2]
  lv = log((1/(sigma*sqrt(2*pi)))*exp(-0.5*((y - mu)/sigma)^2))
  soma = -sum(lv)
  return(soma)
}

N = 1000
T0 = NULL
T1 = NULL

for (i in 1:N){
  y = rnorm(n, mu, sigma)
  fit1 <- optim(par = c(mean(y),sd(y)),logvero,
                y = y,
                method = "BFGS",
                hessian = TRUE)
  
  T0[i] = fit1$par[1]
  T1[i] = fit1$par[2]
}

vicio_l = mean(T0) - mu; vicio_l
EQM_l = var(T0) + vicio_l^2; EQM_l

vicio_s = mean(T1) - sigma
EQM_s = var(T1) + vicio_s^2

Tabela <- data.frame("Vícios" = c(vicio_l, vicio_s),
                     "EQMs" = c(EQM_l, EQM_s),
                     "Estimativa" = c(mu, sigma),
                     "Verdadeiro" = c(mu, sigma),
                     row.names = c("mu", "Sigma"))

Tabela
