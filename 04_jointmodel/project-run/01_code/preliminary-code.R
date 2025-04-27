pacman::p_load(tidyverse, rjags, JMbayes)
rm(list = ls())
data("prothro")
###############
# my code
dat = prothro|> na.omit()|> arrange(id)
id<-
  sapply(1:(dat$id|> unique()|> length()), \(i){
    k = dat$id[dat$id == sort(unique(dat$id))[i]]
    sapply(k,\(j) i)
  })|> unlist()
dat$id<- id

#### preparing data 
dat = filter(dat, id %in% 1:30)|>
  group_by(id)|>
  arrange(time)|>
  slice(1:3)|>
  ungroup()

###

n = dat$id|> unique()|> length()
Tnum = length(unique(dat$Time))+1
time = c(0, sort(unique(dat$Time)), max(dat$Time)+1)
obs.t = sapply(unique(dat$id), \(i) dat$Time[dat$id==i]|> unique())

treat = 1*sapply(unique(dat$id), \(i) dat$treat[dat$id==i]|> {\(i) unique(i)== "prednisone"}())
set.seed(111);fail = rbinom(n, 1,0.9)

# rdN<-  sapply(1:Tnum, \(j){
#   eps = 0.000001
#   sapply(1:n, \(i){
#     
#     risk<- ifelse(obs.t[i] - time[j] + eps>0,1,0)
#     dN<- risk*ifelse(time[j+1] - obs.t[i] - eps>0,1,0)*fail[i]
#     list(risk, dN)
# 
#   })
# })



mod_dat = list(
  obs.t = obs.t, 
  time = time, 
  fail = fail,
  treat = treat,
  n = n,
  Tnum = Tnum, 
  N = dim(dat)[1], 
  id = dat$id,
  eps = 1e-8
)



 
mod_string = textConnection("
data{
for(i in 1:n){for(j in 1:Tnum){
# risk set 
risk[i,j]<- step(obs.t[i] - time[j] + eps)
dN[i,j]<- risk[i,j]*step(time[j+1] - obs.t[i] - eps)*fail[i]
}}
}

model{

for(i in 1:n){for(j in 1:Tnum){
dN[i,j] ~ dpois(Idt[i,j])
Idt[i,j]<- risk[i,j]*exp(alpha[1]*treat[i]+ alpha[2]*b[i])*dL0[j] 
}}

for(j in 1:Tnum){
dL0[j] ~ dgamma(mu0[j], c0)
mu0[j]<- dL0.star[j]*c0
}
c0<- 0.1

for(j in 1:Tnum){dL0.star[j]<- r*(time[j+1] - time[j])}
r <- 5

## longitudinal model
for(i in 1:N){
y[i] ~ dnorm(b[id[i]] + beta*treat[id[i]], tau.y)
}

for(i in 1:n){b[i] ~ dnorm(0, tau.b)}
beta ~ dnorm(0, 1/1000)
tau.y ~ dgamma(0.1, 0.1)
tau.b ~ dgamma(0.1, 0.1)
for(i in 1:2){alpha[i] ~ dnorm(0,1/1000)}
theta ~ dnorm(0, 1/1000)

}")

model <- jags.model(mod_string, data = mod_dat, n.chains=2)
update(model,20000, progress.bar="none")

params_name <- c ( "beta", "alpha")

samples <- coda.samples(model,
                        variable.names=params_name,
                        n.iter=2500, progress.bar="none")

gelman.diag(samples)
summary(samples)










