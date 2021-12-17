# install.packages("sn")
# install.packages("fGarch")
# install.packages("hnp")

# packageDescription("sn")
# packageDescription("fGarch")
# packageDescription("hnp")

require (sn)
require (fGarch)
require (hnp)

rm(list=ls(all=TRUE))

setwd("G:/Meu Drive/DOCUMENTOS/ANDRÉ/MESTRADO UFAC PPGCC/DISCIPLINAS/ESTATÍSTICA COMPUTACIONAL/ATIVIDADE ENVELOPE SIMULADO")

dados <- read.table("A_FatCruz_ex1.txt", h = T)
y <- dados[,2]
y

hist(y)

logvero <- function(param, y){
  mu = param[1]
  sigma = param[2]
  log((1/(sigma*sqrt(2*pi)))*exp(-0.5*((y - mu)/sigma)^2))
  soma = -sum(lv)
  return(soma)
}

fit.norm <- function(dados){
  fit1 <- optim(par = c(mean(dados), sd(dados)), logvero,
                y = dados,
                method = "BFGS",
                hessian = TRUE)
                t = dados - fit1$par[1]
          object <- list(parametros = fit1$par, residuos = t)
          return(object)
}

d.fun1 <- function(obj){
  obj$residuos
}

simula.norm <- function(n, mu, sigma) {
  ysim = rnorm(n, mean = mu, sd = sigma)
  return(ysim)
}

s.fun1 <- function(n, obj){
  mu <- obj$parametros[1]
  sigma <- obj$parametros[2]
  simula.norm(n, mu, sigma)
}

my.data <- data.frame(y)
f.fun1 <- function(y.){
  fit.norm(y.)
}

logvero_sn <- function(param, y){
  mu = param[1]
  sigma = param[2]
  lambda = param[3]
  z = (y - mu)/sigma
  f = (2/sigma)*dnorm(z,0,1)*pnorm(lambda*z,0,1)
  lv = log(f)
  soma = -sum(lv)
  return(soma)
}

fit.sn <- function(dados){
  fit1 <- optim(par = c(22.327123, 4.8915887, -9.1860834), logvero,
                y = dados,
                method = "BFGS",
                hessian = TRUE)
  t = dados - fit1$par[1]
  object <- list(parametros = fit1$par, residuos = t)
  return(object)
}

simula.sn <- function(n, mu, sigma, lambda) {
  ysim = rsn(n, mu, sigma, lambda)
  return(ysim)
}

s.fun <- function(n,obj) {
  mu <- obj$parametros[1]
  sigma <- obj$parametros[2]
  lambda <- obj$parametros[3]
  simula.sn(n, mu, sigma, lambda)
}

my.data <- data.frame(y)
f.fun <- function(y.) {
  fit.sn(y.)
}

fit1 <- fit.norm(y)
fit2 <- fit.sn(y)
hnp(fit1, newclass = T, halfnormal = F, diagfun = d.fun1,
    data = my.data, sim = 1000, verb = T, half = TRUE, print = TRUE)

hnp(fit2, newclass = T, halfnormal = F, diagfun = d.fun1,
    data = my.data, sim = 1000, verb = T, half = TRUE, print = TRUE)