######################################################
#Replication code for "Power Diffusion and Democracy"#
######################################################
# Julian Bernauer and Adrian Vatter 

# Replication code Chapter 5: turnout
library(R2jags)
dir <- "[...]"
setwd(dir)

# prepared data with CSES and PDD files 
load("PDD2018_ch5_leg.Rdata")

# new id for reduced set of countries (28)
DPD$id <- as.numeric(factor(DPD$ccodealp))
idcc <- DPD$id

# decentralisation at lowest level 
dec <- DPD$dec

# country-level 
DPDc <-aggregate(DPD, by=list(DPD$id),FUN=mean, na.rm=TRUE)
exelegc <- DPDc$exeleg
exelegc[is.na(exelegc)] <- NA
fedc <- round(DPDc$fed)
bicc <- round(DPDc$bic)
constc <- round(DPDc$const)
judc <- round(DPDc$jud)
dirc <- DPDc$dir
presc <- round(DPDc$pres)

# election level 
DPDe <-aggregate(DPD, by=list(DPD$ccodealp, DPD$eper),FUN=mean, na.rm=TRUE)
DPDe <- DPDe[order(DPDe$Group.1, DPDe$Group.2), ]    
idne <- as.numeric(factor(DPDe$id))
elece <- DPDe$elec
elece[is.na(elece)] <- NA
pare <- DPDe$par
pare[is.na(pare)] <- NA
cabe <- round(DPDe$cab)
cabe[is.na(cabe)] <- NA

# outcome model variables
acc <- cses3_red$acc
age <- cses3_red$age
age2 <- cses3_red$age2
cong <- cses3_red$cong
demsat <- cses3_red$demsat
eduh <- cses3_red$eduh
edum <- cses3_red$edum
fem <- cses3_red$fem
turn <- cses3_red$turn

# id individual level 
idni <- as.numeric(factor(cses3_red$ccodealp))

# adding mandatory voting 
manda2 <- c(1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)

#Control variables at country level 
old <- DPDc$old

N <- 28
NI <- length(turn)
NT <- length(dec)
NE <- length(elece)

micro.model <- "model{

for(i in 1:NI){
turn[i] ~ dbern(p.turn[i])
p.turn[i] <- 1 / (1 + exp(-z[i]))
z[i] <- a_turn[idni[i]] + bturn_fem*fem[i] + bturn_age*age[i] + bturn_age2*age[i]*age[i] + bturn_edum*edum[i] + bturn_eduh*eduh[i] 
}

bturn_fem ~ dnorm(0, .0001)
bturn_age ~ dnorm(0, .0001)
bturn_age2 ~ dnorm(0, .0001)
bturn_edum ~ dnorm(0, .0001)
bturn_eduh ~ dnorm(0, .0001)

for(j in 1:N){
a_turn[j] ~ dnorm(mu0_turn[j],tau.turn0) 
mu0_turn[j] <- a10 + bturn_manda*manda[j] + bturn_prop*cx1[j] + bturn_veto*cx2[j] + bturn_regime*cx3[j] + bturn_dir*cx4[j] + bturn_onetwo*cx1[j]*cx2[j] + bturn_old*old[j]
}

a10 ~ dnorm(0, .0001) I(,0)
bturn_manda ~ dnorm(0, .0001)  I(0,)
bturn_prop ~ dnorm(0, .0001)
bturn_veto ~ dnorm(0, .0001)
bturn_regime ~ dnorm(0, .0001)
bturn_dir ~ dnorm(0, .0001)
bturn_onetwo ~ dnorm(0, .0001)
bturn_old  ~ dnorm(0, .0001)

tau.turn0 <- pow(sigma.turn0, -2)
sigma.turn0 ~ dunif(0,100)


for(k in 1:NE){

elec[k] ~ dnorm(mu.elec[k],tau.elec)
mu.elec[k] <- alpha.elec[idne[k]] 
+ gamma.elec1*zex1[k] 

par[k] ~ dnorm(mu.par[k],tau.par)
mu.par[k] <- alpha.par[idne[k]] 
+ gamma.par1*zex1[k] 

cab[k] ~ dcat(p.cab[k,1:4])
p.cab[k,1] <- Q[k,1]
p.cab[k,2] <- Q[k,2] - Q[k,1] 
p.cab[k,3] <- Q[k,3] - Q[k,2] 
p.cab[k,4] <- 1 - Q[k,3]

for (k.cut in 1:3){
logit(Q[k,k.cut]) <-  C[idne[k],k.cut] 
- 
beta.cab1*zex1[k] 
}
}

for(j in 1:N){
alpha.elec[j] ~ dnorm(mu.ae[j],tau.ae)
mu.ae[j] <- ae + a.elec1*cx1[j]	
alpha.par[j] ~ dnorm(mu.ap[j],tau.ap) 
mu.ap[j] <- ap + a.par1*cx1[j]	
}

ae ~ dnorm(0,.0001)
a.elec1 ~ dnorm(0,.0001) I(,0)
tau.ae <- pow(sigma.ae, -2)
sigma.ae ~ dunif(0, 50)

ap ~ dnorm(0,.0001)
a.par1 ~ dnorm(0,.0001) I(0,)
tau.ap <- pow(sigma.ap, -2) 
sigma.ap ~ dunif(0, 50)

tau.elec <- pow(sigma.elec, -2)
sigma.elec ~ dunif(0, 50)

tau.par <- pow(sigma.par, -2)
sigma.par ~ dunif(0, 50)

gamma.elec1 ~ dnorm(0, .001) 
gamma.par1 ~ dnorm(0, .001) I(0,)

for (j in 1:N){
C[j,1] ~ dnorm (mu.ac[j], tau.ac)
mu.ac[j] <- acab 
- 
(
  a.cab1*cx1[j]
  + beta.prescab*pres[j] 
  + beta.dircab*dir[j]
) 
  delta[j,1] ~ dexp(2)
  delta[j,2] ~ dexp(2)
  C[j, 2] <- C[j,1] + delta[j,1]
  C[j, 3] <- C[j,2] + delta[j,2]
}

acab ~ dnorm(0,.0001)
tau.ac <- pow(sigma.ac,-2)
sigma.ac ~ dunif(0,50)

a.cab1 ~ dnorm(0, .001) 
#I(0,)

beta.cab1 ~ dnorm(0, .001) I(0,)

for(i in 1:NT){
dec[i] ~ dnorm(mu.dec[i],tau.dec)
mu.dec[i] <- alpha.dec[idn[i]] 
+ gamma.dec2*x2[i] 

}

for(j in 1:N){
alpha.dec[j] ~ dnorm(mu.ad[j],tau.ad) 
mu.ad[j] <- adec 
+ a.dec2*cx2[j]
}

adec ~ dnorm(0,.0001)
a.dec2 ~ dnorm(0,.0001)
tau.ad <- pow(sigma.ad, -2)
sigma.ad ~ dunif(0, 50)

tau.dec <- pow(sigma.dec, -2)
sigma.dec ~ dunif(0, 50)

gamma.dec2 ~ dnorm(0, .001) I(0,)

for(i in 1:N){

exeleg[i] ~ dnorm(mu.exeleg[i],tau.exeleg)
mu.exeleg[i] <- alpha.exeleg 
+ gamma.exeleg1*cx1[i] 
+ gamma.presexeleg*pres[i]

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
beta.bic2 ~ dnorm(0, .001) I(0,)
beta.const2 ~ dnorm(0, .001)
beta.jud2 ~ dnorm(0, .001)
beta.pres3 ~ dnorm(0, .001) I(0,)


for(k in 1:NE){
zex1[k] ~ dnorm(cx1[idne[k]],1) 
}

for(i in 1:NT){
x2[i] ~ dnorm(cx2[idn[i]],1)  
}

#CH 3, US 28, max. 28

for(k in 4:27){
cx1[k] ~ dnorm(0,1) 
cx2[k] ~ dnorm(0,1)  
cx3[k] ~ dnorm(0,1) 
cx4[k] ~ dnorm(0,1) 

}

cx1[1] ~ dnorm(0,1)
cx1[2] ~ dnorm(0,1) 
cx1[3] ~ dnorm(0,1) I(0,)
cx1[28] ~ dnorm(0,1)

cx2[1] ~ dnorm(0,1) 
cx2[2] ~ dnorm(0,1) 
cx2[3] ~ dnorm(0,1)I(0,)
cx2[28] ~ dnorm(0,1)

cx3[1] ~ dnorm(0,1) 
cx3[2] ~ dnorm(0,1) 
cx3[3] ~ dnorm(0,1) 
cx3[28] ~ dnorm(0,1) I(0,)

cx4[1] ~ dnorm(0,1) I(0,)
cx4[2] ~ dnorm(0,1) 
cx4[3] ~ dnorm(0,1)
cx4[28] ~ dnorm(0,1)I(0,)

beta.dircab ~ dnorm(0, .001) 
beta.prescab ~ dnorm(0, .001) 
gamma.presexeleg  ~ dnorm(0, .001) I(,0)

}"

write(micro.model, file="micro.model.jags")

micro.data <- list(N=N, NI=NI, NT=NT, NE=NE, idn=idcc, idne = idne, idni=idni, par=pare, elec=elece, exeleg=exelegc, cab=cabe, fed=fedc, dec=dec, bic=bicc, const=constc, jud=judc, dir=dirc, pres=presc, demsat=demsat, turn=turn, manda=manda2, cong=cong, fem=fem, age=age, edum=edum, eduh=eduh, acc=acc, old=old)

micro.parameters <- c("sigma.elec", "sigma.par", "sigma.exeleg", "sigma.dec", "sigma.dir", "alpha.exeleg", "alpha.dir", "gamma.elec1", "gamma.par1", "gamma.exeleg1", "gamma.dec2", "gamma.dir4", "tau.fed", "tau.bic", "tau.const", "tau.jud", "tau.pres", "beta.cab1", "beta.fed2", "beta.bic2", "beta.const2", "beta.jud2", "beta.pres3", "cx1", "cx2", "cx3", "cx4", "beta.prescab", "beta.dircab", "gamma.presexeleg", "acab", "adec", "ae", "ap", "a.elec1",  "a.par1", "a.dec2", "a.cab1", "tau.ac", "sigma.ac", "mu.ac", "mu.ad", "mu.ae", "mu.ap", "zex1", "bturn_fem" , "bturn_age", "bturn_age2","bturn_edum" ,"bturn_eduh", "bturn_demsat", "bturn_cong", "a_turn" ,"sigma.turn0",  "bturn_manda" , "bturn_prop" ,"bturn_veto" ,"bturn_regime", "bturn_dir" ,  "bturn_onetwo","bturn_old", "a10")

jags.micro <- jags.model(file="micro.model.jags", data = micro.data, n.chains = 3, n.adapt = 100)

sampleshelp <- coda.samples(jags.micro, micro.parameters, n.iter=100, thin=1)

samplesburn <- coda.samples(jags.micro, micro.parameters, n.iter=1800, thin=18)

samples <- coda.samples(jags.micro, micro.parameters, n.iter=2000, thin=20)

plot(sampleshelp, ask=TRUE)

plot(samples, ask=TRUE)


#Fig. 5.8a: turnout full

kette <- as.matrix(samples)

bturn_fem <- kette[,"bturn_fem"] 
mturf <- mean(bturn_fem)
sturf <- sd(bturn_fem)

bturn_age <- kette[,"bturn_age"]
mtura <- mean(bturn_age)
stura <- sd(bturn_age)

bturn_age2 <- kette[,"bturn_age2"]
mtura2 <- mean(bturn_age2)
stura2 <- sd(bturn_age2)

bturn_edum<- kette[,"bturn_edum"]
mturem <- mean(bturn_edum)
sturem <- sd(bturn_edum)

bturn_eduh <- kette[,"bturn_eduh"]
mtureh <- mean(bturn_eduh)
stureh <- sd(bturn_eduh)

bturn_manda <- kette[,"bturn_manda"]
mturm <- mean(bturn_manda)
sturm <- sd(bturn_manda)

bturn_prop <- kette[,"bturn_prop"]
mturp <- mean(bturn_prop)
sturp <- sd(bturn_prop)

bturn_veto <- kette[,"bturn_veto"]
mturv <- mean(bturn_veto)
sturv <- sd(bturn_veto)

bturn_regime <- kette[,"bturn_regime"]
mturr <- mean(bturn_regime)
sturr <- sd(bturn_regime)

bturn_dir <- kette[,"bturn_dir"] 
mturd <- mean(bturn_dir)
sturd <- sd(bturn_dir)

bturn_onetwo <- kette[,"bturn_onetwo"]
mturot <- mean(bturn_onetwo)
sturot <- sd(bturn_onetwo)

bturn_old <- kette[,"bturn_old"]
mturold <- mean(bturn_old)
sturold <- sd(bturn_old)

mturn <- c(mturf, mtura*10, mtura2*10, mturem, mtureh, mturm, mturp, mturv, mturr, mturd, mturot, mturold)

sturn <- c(sturf, stura*10, stura2*10, sturem, stureh , sturm, sturp, sturv, sturr, sturd, sturot, sturold)

vlabels <- c("Female" , "Age (x10)", "Age squared (x10)", "Education (med.)" ,"Education (high)", 
             "Mandatory voting" , "Prop. power diff." ,"Decentral power diff." ,"Regime power diff.", "Direct power diff." , "Prop. x dec.", "'Old' democracy")

var.names <- c(vlabels)
m.v <- mturn
sd.v <- sturn

postscript("fig5_8_turn_full.eps", width = 800, height = 800, horizontal=FALSE)

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-2.5,2.5), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-2.5,2.5, by = .5), label = seq(-2.5,2.5, by = .5), cex.axis=.9)
axis(2, at = y.axis, label = var.names, las = 1, tick = T, font=1, cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(m.v-qnorm(.975)*sd.v, y.axis, m.v+qnorm(.975)*sd.v, y.axis, lwd =  1.5)
abline(v=0, lty = 2)
segments(m.v-qnorm(.9)*sd.v, y.axis -.1, m.v-qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5) 
segments(m.v+qnorm(.9)*sd.v, y.axis -.1, m.v+qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .6 
segments(left.side,12,left.side,8) 
segments(left.side,12,left.side+.1,12) 
segments(left.side,8,left.side+.1,8)
text(.5, 10, "Individual Level", srt = 90, cex=.9)
segments(left.side,7,left.side,1) 
segments(left.side,7,left.side+.1,7) 
segments(left.side,1,left.side+.1,1)
text(.5, 3.5, "Country Level", srt = 90, cex=.9)

dev.off()


###
# only 16 old democracies 

# clear workspace, set up data again 
rm(list=ls())

dir <- "[...]"
setwd(dir)

library(R2jags)

# prepared data with CSES and PDD files 
load("PDD2018_ch5_leg.Rdata")

DPD <- DPD[DPD$old==1,]

# new id for reduced set of countries 
DPD$id <- as.numeric(factor(DPD$ccodealp))
idcc <- DPD$id

# decentralisation at lowest level 
dec <- DPD$dec

# country-level 
DPDc <-aggregate(DPD, by=list(DPD$id),FUN=mean, na.rm=TRUE)
exelegc <- DPDc$exeleg
exelegc[is.na(exelegc)] <- NA
fedc <- round(DPDc$fed)
bicc <- round(DPDc$bic)
constc <- round(DPDc$const)
judc <- round(DPDc$jud)
dirc <- DPDc$dir
presc <- round(DPDc$pres)

# election level 
DPDe <-aggregate(DPD, by=list(DPD$ccodealp, DPD$eper),FUN=mean, na.rm=TRUE)
DPDe <- DPDe[order(DPDe$Group.1, DPDe$Group.2), ]    
idne <- as.numeric(factor(DPDe$id))
elece <- DPDe$elec
elece[is.na(elece)] <- NA
pare <- DPDe$par
pare[is.na(pare)] <- NA
cabe <- round(DPDe$cab)
cabe[is.na(cabe)] <- NA

# CSES only old democracies 
cses3_red <- cses3_red[cses3_red$old==1,]

# outcome model variables
acc <- cses3_red$acc
age <- cses3_red$age
age2 <- cses3_red$age2
cong <- cses3_red$cong
demsat <- cses3_red$demsat
eduh <- cses3_red$eduh
edum <- cses3_red$edum
fem <- cses3_red$fem
turn <- cses3_red$turn

# id individual level 
idni <- as.numeric(factor(cses3_red$ccodealp))

# adding mandatory voting 
manda2 <- c(1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)

#Control variables at country level 
old <- DPDc$old


micro.model <- "model{

for(i in 1:NI){
turn[i] ~ dbern(p.turn[i])
p.turn[i] <- 1 / (1 + exp(-z[i]))
z[i] <- a_turn[idni[i]] + bturn_fem*fem[i] + bturn_age*age[i] + bturn_age2*age[i]*age[i] + bturn_edum*edum[i] + bturn_eduh*eduh[i] 
}

bturn_fem ~ dnorm(0, .0001)
bturn_age ~ dnorm(0, .0001)
bturn_age2 ~ dnorm(0, .0001)
bturn_edum ~ dnorm(0, .0001)
bturn_eduh ~ dnorm(0, .0001)

for(j in 1:N){
a_turn[j] ~ dnorm(mu0_turn[j],tau.turn0) 
mu0_turn[j] <- a10 + bturn_manda*manda[j] + bturn_prop*cx1[j] + bturn_veto*cx2[j] + bturn_regime*cx3[j] + bturn_dir*cx4[j] + bturn_onetwo*cx1[j]*cx2[j] 
#+ bturn_old*old[j]
}

a10 ~ dnorm(0, .0001) I(,0)
bturn_manda ~ dnorm(0, .0001)  I(0,)
bturn_prop ~ dnorm(0, .0001)
bturn_veto ~ dnorm(0, .0001)
bturn_regime ~ dnorm(0, .0001)
bturn_dir ~ dnorm(0, .0001)
bturn_onetwo ~ dnorm(0, .0001)
#bturn_old  ~ dnorm(0, .0001)

tau.turn0 <- pow(sigma.turn0, -2)
sigma.turn0 ~ dunif(0,100)


for(k in 1:NE){

elec[k] ~ dnorm(mu.elec[k],tau.elec)
mu.elec[k] <- alpha.elec[idne[k]] 
+ gamma.elec1*zex1[k] 

par[k] ~ dnorm(mu.par[k],tau.par)
mu.par[k] <- alpha.par[idne[k]] 
+ gamma.par1*zex1[k] 

cab[k] ~ dcat(p.cab[k,1:4])
p.cab[k,1] <- Q[k,1]
p.cab[k,2] <- Q[k,2] - Q[k,1] 
p.cab[k,3] <- Q[k,3] - Q[k,2] 
p.cab[k,4] <- 1 - Q[k,3]

for (k.cut in 1:3){
logit(Q[k,k.cut]) <-  C[idne[k],k.cut] 
- 
beta.cab1*zex1[k] 
}
}

for(j in 1:N){
alpha.elec[j] ~ dnorm(mu.ae[j],tau.ae)
mu.ae[j] <- ae + a.elec1*cx1[j]	
alpha.par[j] ~ dnorm(mu.ap[j],tau.ap) 
mu.ap[j] <- ap + a.par1*cx1[j]	
}

ae ~ dnorm(0,.0001)
a.elec1 ~ dnorm(0,.0001) I(,0)
tau.ae <- pow(sigma.ae, -2)
sigma.ae ~ dunif(0, 50)

ap ~ dnorm(0,.0001)
a.par1 ~ dnorm(0,.0001) I(0,)
tau.ap <- pow(sigma.ap, -2) 
sigma.ap ~ dunif(0, 50)

tau.elec <- pow(sigma.elec, -2)
sigma.elec ~ dunif(0, 50)

tau.par <- pow(sigma.par, -2)
sigma.par ~ dunif(0, 50)

gamma.elec1 ~ dnorm(0, .001) 
gamma.par1 ~ dnorm(0, .001) I(0,)

for (j in 1:N){
C[j,1] ~ dnorm (mu.ac[j], tau.ac)
mu.ac[j] <- acab 
- 
(
  a.cab1*cx1[j]
  + beta.prescab*pres[j] 
  + beta.dircab*dir[j]
) 
  delta[j,1] ~ dexp(2)
  delta[j,2] ~ dexp(2)
  C[j, 2] <- C[j,1] + delta[j,1]
  C[j, 3] <- C[j,2] + delta[j,2]
}

acab ~ dnorm(0,.0001)
tau.ac <- pow(sigma.ac,-2)
sigma.ac ~ dunif(0,50)

a.cab1 ~ dnorm(0, .001) 
#I(0,)

beta.cab1 ~ dnorm(0, .001) I(0,)

for(i in 1:NT){
dec[i] ~ dnorm(mu.dec[i],tau.dec)
mu.dec[i] <- alpha.dec[idn[i]] 
+ gamma.dec2*x2[i] 

}

for(j in 1:N){
alpha.dec[j] ~ dnorm(mu.ad[j],tau.ad) 
mu.ad[j] <- adec 
+ a.dec2*cx2[j]
}

adec ~ dnorm(0,.0001)
a.dec2 ~ dnorm(0,.0001)
tau.ad <- pow(sigma.ad, -2)
sigma.ad ~ dunif(0, 50)

tau.dec <- pow(sigma.dec, -2)
sigma.dec ~ dunif(0, 50)

gamma.dec2 ~ dnorm(0, .001) I(0,)

for(i in 1:N){

exeleg[i] ~ dnorm(mu.exeleg[i],tau.exeleg)
mu.exeleg[i] <- alpha.exeleg 
+ gamma.exeleg1*cx1[i] 
+ gamma.presexeleg*pres[i]

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
beta.bic2 ~ dnorm(0, .001) I(0,)
beta.const2 ~ dnorm(0, .001)
beta.jud2 ~ dnorm(0, .001)
beta.pres3 ~ dnorm(0, .001) I(0,)


for(k in 1:NE){
zex1[k] ~ dnorm(cx1[idne[k]],1) 
}

for(i in 1:NT){
x2[i] ~ dnorm(cx2[idn[i]],1)  
}

#CH 3, US 16, max. 16

for(k in 4:15){
cx1[k] ~ dnorm(0,1) 
cx2[k] ~ dnorm(0,1)  
cx3[k] ~ dnorm(0,1) 
cx4[k] ~ dnorm(0,1) 

}

cx1[1] ~ dnorm(0,1)
cx1[2] ~ dnorm(0,1) 
cx1[3] ~ dnorm(0,1) I(0,)
cx1[16] ~ dnorm(0,1)

cx2[1] ~ dnorm(0,1) 
cx2[2] ~ dnorm(0,1) 
cx2[3] ~ dnorm(0,1)I(0,)
cx2[16] ~ dnorm(0,1)

cx3[1] ~ dnorm(0,1) 
cx3[2] ~ dnorm(0,1) 
cx3[3] ~ dnorm(0,1) 
cx3[16] ~ dnorm(0,1) I(0,)

cx4[1] ~ dnorm(0,1) I(0,)
cx4[2] ~ dnorm(0,1) 
cx4[3] ~ dnorm(0,1)
cx4[16] ~ dnorm(0,1)I(0,)

beta.dircab ~ dnorm(0, .001) 
beta.prescab ~ dnorm(0, .001) 
gamma.presexeleg  ~ dnorm(0, .001) I(,0)

}"

write(micro.model, file="micro.model.jags")

micro.data <- list(N=N, NI=NI, NT=NT, NE=NE, idn=idcc, idne = idne, idni=idni, par=pare, elec=elece, exeleg=exelegc, cab=cabe, fed=fedc, dec=dec, bic=bicc, const=constc, jud=judc, dir=dirc, pres=presc, demsat=demsat, turn=turn, manda=manda2, cong=cong, fem=fem, age=age, edum=edum, eduh=eduh, acc=acc, old=old)

micro.parameters <- c("sigma.elec", "sigma.par", "sigma.exeleg", "sigma.dec", "sigma.dir", "alpha.exeleg", "alpha.dir", "gamma.elec1", "gamma.par1", "gamma.exeleg1", "gamma.dec2", "gamma.dir4", "tau.fed", "tau.bic", "tau.const", "tau.jud", "tau.pres", "beta.cab1", "beta.fed2", "beta.bic2", "beta.const2", "beta.jud2", "beta.pres3", "cx1", "cx2", "cx3", "cx4", "beta.prescab", "beta.dircab", "gamma.presexeleg", "acab", "adec", "ae", "ap", "a.elec1",  "a.par1", "a.dec2", "a.cab1", "tau.ac", "sigma.ac", "mu.ac", "mu.ad", "mu.ae", "mu.ap", "zex1", "bturn_fem" , "bturn_age", "bturn_age2","bturn_edum" ,"bturn_eduh", "a_turn" ,"sigma.turn0", "bturn_manda" , "bturn_prop" ,"bturn_veto" ,"bturn_regime", "bturn_dir", "bturn_onetwo",
#"bturn_old", 
"a10")

jags.micro <- jags.model(file="micro.model.jags", data = micro.data, n.chains = 3, n.adapt = 100)

sampleshelp <- coda.samples(jags.micro, micro.parameters, n.iter=100, thin=1)

samplesburn <- coda.samples(jags.micro, micro.parameters, n.iter=1800, thin=18)

samples2 <- coda.samples(jags.micro, micro.parameters, n.iter=2000, thin=20)

#plot(sampleshelp, ask=TRUE)
#plot(samples2, ask=TRUE)


#Fig. 5.8b: turnout reduced 

kette <- as.matrix(samples2)

bturn_fem <- kette[,"bturn_fem"] 
mturf <- mean(bturn_fem)
sturf <- sd(bturn_fem)

bturn_age <- kette[,"bturn_age"]
mtura <- mean(bturn_age)
stura <- sd(bturn_age)

bturn_age2 <- kette[,"bturn_age2"]
mtura2 <- mean(bturn_age2)
stura2 <- sd(bturn_age2)

bturn_edum<- kette[,"bturn_edum"]
mturem <- mean(bturn_edum)
sturem <- sd(bturn_edum)

bturn_eduh <- kette[,"bturn_eduh"]
mtureh <- mean(bturn_eduh)
stureh <- sd(bturn_eduh)

bturn_manda <- kette[,"bturn_manda"]
mturm <- mean(bturn_manda)
sturm <- sd(bturn_manda)

bturn_prop <- kette[,"bturn_prop"]
mturp <- mean(bturn_prop)
sturp <- sd(bturn_prop)

bturn_veto <- kette[,"bturn_veto"]
mturv <- mean(bturn_veto)
sturv <- sd(bturn_veto)

bturn_regime <- kette[,"bturn_regime"]
mturr <- mean(bturn_regime)
sturr <- sd(bturn_regime)

bturn_dir <- kette[,"bturn_dir"] 
mturd <- mean(bturn_dir)
sturd <- sd(bturn_dir)

bturn_onetwo <- kette[,"bturn_onetwo"]
mturot <- mean(bturn_onetwo)
sturot <- sd(bturn_onetwo)

bturn_old <- kette[,"bturn_old"]
mturold <- mean(bturn_old)
sturold <- sd(bturn_old)

mturn <- c(mturf, mtura, mtura2, mturem, mtureh, mturm, mturp, mturv, mturr, mturd, mturot)
           #, mturold)

sturn <- c(sturf, stura, stura2, sturem, stureh , sturm, sturp, sturv, sturr, sturd, sturot)
#, sturold)

vlabels <- c("Female" , "Age", "Age squared", "Education (med.)" ,"Education (high)", 
             "Mandatory voting" , "Prop. power diff." ,"Decentral power diff." ,"Regime power diff.", "Direct power diff." , "Prop. X dec.")
#, "Old democracy")

var.names <- c(vlabels)
m.v <- mturn
sd.v <- sturn


postscript("fig5.8b_turn_red.eps", width = 800, height = 800, horizontal=FALSE)

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-2.5,2.5), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-2.5,2.5, by = .5), label = seq(-2.5,2.5, by = .5), cex.axis=.9)
axis(2, at = y.axis, label = var.names, las = 1, tick = T, font=1, cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(m.v-qnorm(.975)*sd.v, y.axis, m.v+qnorm(.975)*sd.v, y.axis, lwd =  1.5)
abline(v=0, lty = 2)
segments(m.v-qnorm(.9)*sd.v, y.axis -.1, m.v-qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5) 
segments(m.v+qnorm(.9)*sd.v, y.axis -.1, m.v+qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .6 
segments(left.side,11,left.side,7) 
segments(left.side,11,left.side+.1,11) 
segments(left.side,7,left.side+.1,7)
text(.5, 9, "Individual level", srt = 90, cex=.9)
segments(left.side,6,left.side,1) 
segments(left.side,6,left.side+.1,6) 
segments(left.side,1,left.side+.1,1)
text(.5, 3.5, "Country level", srt = 90, cex=.9)

dev.off()

