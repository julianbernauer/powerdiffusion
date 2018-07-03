# Replication code factor analytical item response theoretical (FAIRT) approach to generate scores of power diffusion 
# Chapter 3 of the book 
# Uses R and JAGS 

library(foreign)
library(R2WinBUGS)
library(R2jags)
library(rjags)

dir <- "[...]"

setwd(dir)

load("DPD2018July.Rdata")
DPD <- DPD18
ls(DPD)
attach(DPD)

#Main measurement model 
model <- "model{

#Election-level model for electoral disproportionality and the number of parties
for(k in 1:NE){
elec[k] ~ dnorm(mu.elec[k],tau.elec)
mu.elec[k] <- alpha.elec[idne[k]] + gamma.elec1*ex1[k]
par[k] ~ dnorm(mu.par[k],tau.par)
mu.par[k] <- alpha.par[idne[k]] + gamma.par1*ex1[k]
cab[k] ~ dcat(p.cab[k,1:4])
p.cab[k,1] <- Q[k,1]
p.cab[k,2] <- Q[k,2] - Q[k,1]
p.cab[k,3] <- Q[k,3] - Q[k,2]
p.cab[k,4] <- 1 - Q[k,3]
for (k.cut in 1:3){
logit(Q[k,k.cut]) <- C[idne[k],k.cut] - beta.cab1*ex1[k]
}
}

#Country-level model for electoral disproportionality and the number of parties
for(j in 1:N){
alpha.elec[j] ~ dnorm(mu.ae[j],tau.ae)
mu.ae[j] <- ae + a.elec1*cx1[j]
alpha.par[j] ~ dnorm(mu.ap[j],tau.ap)
mu.ap[j] <- ap + a.par1*cx1[j]
}
ae ~ dnorm(0,.0001)
a.elec1 ~ dnorm(0,.0001)
tau.ae <- pow(sigma.ae, -2)
sigma.ae ~ dunif(0, 50)
ap ~ dnorm(0,.0001)
# Restriction 
a.par1 ~ dnorm(0,.0001) I(0,)
tau.ap <- pow(sigma.ap, -2)
sigma.ap ~ dunif(0, 50)
tau.elec <- pow(sigma.elec, -2)
sigma.elec ~ dunif(0, 50)
tau.par <- pow(sigma.par, -2)
sigma.par ~ dunif(0, 50)
gamma.elec1 ~ dnorm(0, .001)
# Restriction 
gamma.par1 ~ dnorm(0, .001) I(0,)

#Election- and country level ordinal logit model for cabinet type
for (j in 1:N){
C[j,1] ~ dnorm (mu.ac[j], tau.ac)
mu.ac[j] <- acab - (a.cab1*cx1[j] + beta.prescab*pres[j] + beta.dircab*dir[j])
delta[j,1] ~ dexp(2)
delta[j,2] ~ dexp(2)
C[j, 2] <- C[j,1] + delta[j,1]
C[j, 3] <- C[j,2] + delta[j,2]
}
acab ~ dnorm(0,.0001)
tau.ac <- pow(sigma.ac,-2)
sigma.ac ~ dunif(0,50)
a.cab1 ~ dnorm(0, .001)
beta.cab1 ~ dnorm(0, .001)

#Country-year- and country-level model for fiscal decentralization
for(i in 1:NT){
dec[i] ~ dnorm(mu.dec[i],tau.dec)
mu.dec[i] <- alpha.dec[idn[i]] + gamma.dec2*x2[i]
}
for(j in 1:N){
alpha.dec[j] ~ dnorm(mu.ad[j],tau.ad)
mu.ad[j] <- adec + a.dec2*cx2[j]
}
adec ~ dnorm(0,.0001)
a.dec2 ~ dnorm(0,.0001)
tau.ad <- pow(sigma.ad, -2)
sigma.ad ~ dunif(0, 50)
tau.dec <- pow(sigma.dec, -2)
sigma.dec ~ dunif(0, 50)
# Restriction 
gamma.dec2 ~ dnorm(0, .001) I(0,)

#Country-level models for the remaining indicators
for(i in 1:N){
exeleg[i] ~ dnorm(mu.exeleg[i],tau.exeleg)
mu.exeleg[i] <- alpha.exeleg + gamma.exeleg1*cx1[i] + gamma.presexeleg*pres[i]
dir[i] ~ dnorm(mu.dir[i],tau.dir)
mu.dir[i] <- alpha.dir + gamma.dir4*cx4[i]
pres[i] ~ dcat(p.pres[i,1:3])
mu.pres[i] <- beta.pres3*cx3[i]
logit(Q.pres[i,1]) <- tau.pres[1] - mu.pres[i]
p.pres[i,1] <- Q.pres[i,1]
logit(Q.pres[i,2]) <- tau.pres[2] - mu.pres[i]
p.pres[i,2] <- Q.pres[i,2] - Q.pres[i,1]
p.pres[i,3] <- 1 - Q.pres[i,2]
fed[i] ~ dcat(p.fed[i,1:3])
mu.fed[i] <- beta.fed2*cx2[i]
logit(Q.fed[i,1]) <- tau.fed[1] - mu.fed[i]
p.fed[i,1] <- Q.fed[i,1]
logit(Q.fed[i,2]) <- tau.fed[2] - mu.fed[i]
p.fed[i,2] <- Q.fed[i,2] - Q.fed[i,1]
p.fed[i,3] <- 1 - Q.fed[i,2]
bic[i] ~ dcat(p.bic[i,1:4])
mu.bic[i] <- beta.bic2*cx2[i]
logit(Q.bic[i,1]) <- tau.bic[1]-mu.bic[i]
p.bic[i,1] <- Q.bic[i,1]
logit(Q.bic[i,2]) <- tau.bic[2] - mu.bic[i]
p.bic[i,2] <- Q.bic[i,2] - Q.bic[i,1]
logit(Q.bic[i,3]) <- tau.bic[3] - mu.bic[i]
p.bic[i,3] <- Q.bic[i,3] - Q.bic[i,2]
p.bic[i,4] <- 1 - Q.bic[i,3]
const[i] ~ dcat(p.const[i,1:3])
mu.const[i] <- beta.const2*cx2[i]
logit(Q.const[i,1]) <- tau.const[1] - mu.const[i]
p.const[i,1] <- Q.const[i,1]
logit(Q.const[i,2]) <- tau.const[2] - mu.const[i]
p.const[i,2] <- Q.const[i,2] - Q.const[i,1]
p.const[i,3] <- 1 - Q.const[i,2]
jud[i] ~ dcat(p.jud[i,1:3])
mu.jud[i] <- beta.jud2*cx2[i]
logit(Q.jud[i,1]) <- tau.jud[1] - mu.jud[i]
p.jud[i,1] <- Q.jud[i,1]
logit(Q.jud[i,2]) <- tau.jud[2] - mu.jud[i]
p.jud[i,2] <- Q.jud[i,2] - Q.jud[i,1]
p.jud[i,3] <- 1 - Q.jud[i,2]
}
tau.exeleg <- pow(sigma.exeleg, -2)
sigma.exeleg ~ dunif(0, 50)
tau.dir <- pow(sigma.dir, -2)
sigma.dir ~ dunif(0, 50)
alpha.exeleg ~ dnorm(0, .001)
alpha.dir ~ dnorm(0, .001)
gamma.exeleg1 ~ dnorm(0, .001)
gamma.dir4 ~ dnorm(0, .001)
tau.pres[1] ~ dnorm(0,.01)
for (j in 1:1){
delta.pres[j] ~ dexp(2)
tau.pres[j+1] <- tau.pres[j] + delta.pres[j]
}
tau.fed[1] ~ dnorm(0,.01)
for (j in 1:1){
delta.fed[j] ~ dexp(2)
tau.fed[j+1] <- tau.fed[j] + delta.fed[j]
}
tau.bic[1] ~ dnorm(0,.01)
for (j in 1:2){
delta.bic[j] ~ dexp(2)
tau.bic[j+1] <- tau.bic[j] + delta.bic[j]
}
tau.jud[1] ~ dnorm(0,.01)
for (j in 1:1){
delta.jud[j] ~ dexp(2)
tau.jud[j+1] <- tau.jud[j] + delta.jud[j]
}
tau.const[1] ~ dnorm(0,.01)
for (j in 1:1){
delta.const[j] ~ dexp(2)
tau.const[j+1] <- tau.const[j] + delta.const[j]
}
beta.fed2 ~ dnorm(0, .001)
# Restriction
beta.bic2 ~ dnorm(0, .001) I(0,)
beta.const2 ~ dnorm(0, .001)
beta.jud2 ~ dnorm(0, .001)
beta.pres3 ~ dnorm(0, .001)

#Priors for the country scores of power diffusion at the appropriate levels of elections (proportional power diffusion),
for(k in 1:NE){
ex1[k] ~ dnorm(cx1[idne[k]],1)
}
#country-years (decentral power diffusion),
for(i in 1:NT){
x2[i] ~ dnorm(cx2[idn[i]],1)
}
#or countries (all variants)
for(k in 1:55){
cx1[k] ~ dnorm(0,1)
cx2[k] ~ dnorm(0,1)
cx3[k] ~ dnorm(0,1)
cx4[k] ~ dnorm(0,1)
}
#Restriction of country-level proportional power diffusion for Switzerland
cx1[56] ~ dnorm(0,1) I(0,)
cx1[57] ~ dnorm(0,1)
cx1[58] ~ dnorm(0,1)
cx1[59] ~ dnorm(0,1)
cx1[60] ~ dnorm(0,1)
cx1[61] ~ dnorm(0,1)
#Restriction of country-level decentral power diffusion for Switzerland
cx2[56] ~ dnorm(0,1) I(0,)
cx2[57] ~ dnorm(0,1)
cx2[58] ~ dnorm(0,1)
cx2[59] ~ dnorm(0,1)
cx2[60] ~ dnorm(0,1)
cx2[61] ~ dnorm(0,1)
cx3[56] ~ dnorm(0,1)
cx3[57] ~ dnorm(0,1)
cx3[58] ~ dnorm(0,1)
cx3[59] ~ dnorm(0,1)
#Restriction of country-level presidential power diffusion for the United States
cx3[60] ~ dnorm(0,1) I(0,)
cx3[61] ~ dnorm(0,1)
#Restriction of country-level direct power diffusion for Switzerland
cx4[56] ~ dnorm(0,1) I(0,)
cx4[57] ~ dnorm(0,1)
cx4[58] ~ dnorm(0,1)
cx4[59] ~ dnorm(0,1)
cx4[60] ~ dnorm(0,1)
cx4[61] ~ dnorm(0,1)

#Collected cross-indicator effects
beta.dircab ~ dnorm(0, .001)
beta.prescab ~ dnorm(0, .001)
#Restriction of relationship between presidentialism and parliamentary power
gamma.presexeleg ~ dnorm(0, .001) I(,0)
}"
  
write(model, file="avbmodel.jags")

N <- 61
NE <- 427
NT <- length(dec)

DPDc <-aggregate(DPD, by=list(DPD$cname),FUN=mean, na.rm=TRUE)
exelegc <- DPDc$exeleg
exelegc[is.na(exelegc)] <- NA
fedc <- round(DPDc$fed)
bicc <- round(DPDc$bic)
constc <- round(DPDc$const)
judc <- round(DPDc$jud)
dirc <- DPDc$dir
presc <- round(DPDc$pres)

DPD$ides <- paste(DPD$cname, DPD$eper, sep = "")

#Take care of order of countries 

DPDe <-aggregate(DPD, by=list(DPD$ides),FUN=mean, na.rm=TRUE)
elece <- DPDe$elec
elece[is.na(elece)] <- NA
pare <- DPDe$par
pare[is.na(pare)] <- NA

DPD$eper[DPD$eper==0] <- "a"
DPD$eper[DPD$eper==1] <- "b"
DPD$eper[DPD$eper==2] <- "c"
DPD$eper[DPD$eper==3] <- "d"
DPD$eper[DPD$eper==4] <- "e"
DPD$eper[DPD$eper==5] <- "f"
DPD$eper[DPD$eper==6] <- "g"
DPD$eper[DPD$eper==7] <- "h"
DPD$eper[DPD$eper==8] <- "i"
DPD$eper[DPD$eper==9] <- "j"
DPD$eper[DPD$eper==10] <- "k"
DPD$eper[DPD$eper==11] <- "l"
DPD$eper[DPD$eper==12] <- "m"
DPD$eper[DPD$eper==13] <- "n"
DPD$eper[DPD$eper==14] <- "o"
DPD$ides2 <- paste(DPD$cname, DPD$eper, sep = "")
DPD$ide <- as.numeric(factor(DPD$ides2))
#ide <- DPD$ide 

cabe <- round(DPDe$cab)
cabe[is.na(cabe)] <- NA

idne <- DPDe$id

AVBthreel.data <- list(N=N, NT=NT, NE=NE, idn=id, idne=idne, par=pare, elec=elece, exeleg=exelegc, cab=cabe, fed=fedc, dec=dec, bic=bicc, const=constc, jud=judc, dir=dirc, pres=presc)

AVBthreel.parameters <- c("sigma.elec", "sigma.par", "sigma.exeleg", "sigma.dec", "sigma.dir", "alpha.exeleg", "alpha.dir", "gamma.elec1", "gamma.par1", "gamma.exeleg1", "gamma.dec2", "gamma.dir4", "tau.fed", "tau.bic", "tau.const", "tau.jud", "tau.pres", "beta.cab1", "beta.fed2", "beta.bic2", "beta.const2", "beta.jud2", "beta.pres3", "cx1", "cx2", "cx3", "cx4", "beta.prescab", "beta.dircab", "gamma.presexeleg", "acab", "adec", "ae", "ap", "a.elec1",  "a.par1", "a.dec2", "a.cab1", "tau.ac", "sigma.ac", "mu.ac", "mu.ad", "mu.ae", "mu.ap", "ex1")

jags.avb <- jags.model(file="avbmodel.jags", data = AVBthreel.data, n.chains = 3, n.adapt = 100)

sampleshelp <- coda.samples(jags.avb, AVBthreel.parameters, n.iter=100, thin=1)
samplesburn <- coda.samples(jags.avb, AVBthreel.parameters, n.iter=19800, thin=198)
samples <- coda.samples(jags.avb, AVBthreel.parameters, n.iter=20000, thin=200)

# Assessing convergence 
plot(sampleshelp, ask=TRUE) 
plot(samples, ask=TRUE)

# For further output processing...
kette <- as.matrix(samples)
