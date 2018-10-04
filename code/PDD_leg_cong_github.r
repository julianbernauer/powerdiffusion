######################################################
#Replication code for "Power Diffusion and Democracy"#
######################################################
# Julian Bernauer and Adrian Vatter 

# Replication code Chapter 5: Policy congruence   

library(R2jags)

dir <- "[...]"

setwd(dir)

# prepared data with CSES and PDD files 
load("PDD2018_ch5_leg.Rdata")

##################
#Policy congruence

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

#Control variable at country level 
old <- DPDc$old

N <- 28
NI <- length(turn)
NT <- length(dec)
NE <- length(elece)

###
#model of policy congruence 

micro.model <- "model{

for(i in 1:NI){
cong[i] ~ dnorm(mu.cong[i],tau.cong)
mu.cong[i] <- a_cong[idni[i]] +  bcong_fem*fem[i] + bcong_age*age[i] + bcong_age2*age[i]*age[i] + bcong_edum*edum[i] + bcong_eduh*eduh[i] 
}

tau.cong <- pow(sigma.cong, -2)
sigma.cong ~ dunif(0, 50)

bcong_fem ~ dnorm(0, .0001)
bcong_age ~ dnorm(0, .0001)
bcong_age2 ~ dnorm(0, .0001)
bcong_edum ~ dnorm(0, .0001)
bcong_eduh ~ dnorm(0, .0001)

for(j in 1:N){
a_cong[j] ~ dnorm(mu0_cong[j],tau.cong0) 
mu0_cong[j] <- a30 + bcong_prop*cx1[j] + bcong_veto*cx2[j] + bcong_regime*cx3[j] + bcong_dir*cx4[j] + bcong_onetwo*cx1[j]*cx2[j] + bcong_old*old[j]
}

a30 ~ dnorm(0, .0001) 
bcong_prop ~ dnorm(0, .0001)  
bcong_veto ~ dnorm(0, .0001)
bcong_regime ~ dnorm(0, .0001)
bcong_dir ~ dnorm(0, .0001)
bcong_onetwo ~ dnorm(0, .0001)
bcong_old ~ dnorm(0, .0001)

tau.cong0 <- pow(sigma.cong0, -2)
sigma.cong0 ~ dunif(0,100)


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

beta.cab1 ~ dnorm(0, .001) I(0,)

for(i in 1:NT){
dec[i] ~ dnorm(mu.dec[i],tau.dec)
mu.dec[i] <- alpha.dec[idn[i]] 
+ gamma.dec2*x2[i] 

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

gamma.dec2 ~ dnorm(0, .001) I(0,)

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

micro.parameters <- c("sigma.elec", "sigma.par", "sigma.exeleg", "sigma.dec", "sigma.dir", "alpha.exeleg", "alpha.dir", "gamma.elec1", "gamma.par1", "gamma.exeleg1", "gamma.dec2", "gamma.dir4", "tau.fed", "tau.bic", "tau.const", "tau.jud", "tau.pres", "beta.cab1", "beta.fed2", "beta.bic2", "beta.const2", "beta.jud2", "beta.pres3", "cx1", "cx2", "cx3", "cx4", "beta.prescab", "beta.dircab", "gamma.presexeleg", "acab", "adec", "ae", "ap", "a.elec1",  "a.par1", "a.dec2", "a.cab1", "tau.ac", "sigma.ac", "mu.ac", "mu.ad", "mu.ae", "mu.ap", "zex1","bcong_fem" ,"bcong_age" ,"bcong_age2" ,"bcong_edum" ,"bcong_eduh","a_cong" ,"sigma.cong0", "sigma.cong", "bcong_prop" , "bcong_veto" , "bcong_regime",  "bcong_dir" , "bcong_onetwo", "bcong_old", "a30")

jags.micro <- jags.model(file="micro.model.jags", data = micro.data, n.chains = 3, n.adapt = 100)

sampleshelp <- coda.samples(jags.micro, micro.parameters, n.iter=100, thin=1)

samplesburn <- coda.samples(jags.micro, micro.parameters, n.iter=1800, thin=18)

samples <- coda.samples(jags.micro, micro.parameters, n.iter=2000, thin=20)

#plot(sampleshelp, ask=TRUE)
#plot(samples, ask=TRUE)


###
#Figure 5.5a: model of congruence

kette <- as.matrix(samples)

bperclose_fem <- kette[,"bcong_fem"] 
mcongf <- mean(bperclose_fem)
scongf <- sd(bperclose_fem)

bperclose_age <- kette[,"bcong_age"]
mconga <- mean(bperclose_age)
sconga <- sd(bperclose_age)

bperclose_age2 <- kette[,"bcong_age2"]
mconga2 <- mean(bperclose_age2)
sconga2 <- sd(bperclose_age2)

bperclose_edum<- kette[,"bcong_edum"]
mcongem <- mean(bperclose_edum)
scongem <- sd(bperclose_edum)

bperclose_eduh <- kette[,"bcong_eduh"]
mcongeh <- mean(bperclose_eduh)
scongeh <- sd(bperclose_eduh)

bperclose_prop <- kette[,"bcong_prop"]
mcongp <- mean(bperclose_prop)
scongp <- sd(bperclose_prop)

bperclose_veto <- kette[,"bcong_veto"]
mcongv <- mean(bperclose_veto)
scongv <- sd(bperclose_veto)

bperclose_regime <- kette[,"bcong_regime"]
mcongr <- mean(bperclose_regime)
scongr <- sd(bperclose_regime)

bperclose_dir <- kette[,"bcong_dir"] 
mcongd <- mean(bperclose_dir)
scongd <- sd(bperclose_dir)

bperclose_onetwo <- kette[,"bcong_onetwo"]
mcongot <- mean(bperclose_onetwo)
scongot <- sd(bperclose_onetwo)

bperclose_old <- kette[,"bcong_old"]
mcongold <- mean(bperclose_old)
scongold <- sd(bperclose_old)


mcong <- c(mcongf, mconga*10, mconga2*10, mcongem, mcongeh, mcongp, mcongv, mcongr, mcongd, mcongot, mcongold)

scong <- c(scongf, sconga*10, sconga2*10, scongem, scongeh, scongp, scongv, scongr, scongd, scongot, scongold)


vlabels <- c("Female" , "Age (x10)", "Age squared (x10)", "Education (med.)" ,"Education (high)", "Prop. power diff." ,"Decentral power diff." ,"Regime power diff.", 
             "Direct power diff." , "Prop. x dec.", "'Old' democracy")

var.names <- c(vlabels)
m.v <- mcong
sd.v <- scong


postscript("fig5.5a_cong_full.eps", width = 800, height = 800, horizontal=FALSE)

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-1,1), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-1,1, by = 1), label = seq(-1,1, by = 1), cex.axis=.9)
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
text(.5, 9, "Individual Level", srt = 90, cex=.9)
segments(left.side,6,left.side,1) 
segments(left.side,6,left.side+.1,6) 
segments(left.side,1,left.side+.1,1)
text(.5, 3.5, "Country Level", srt = 90, cex=.9)

dev.off()



###
# policy congruence only 16 old democracies 

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

N <- 16
NI <- length(turn)
NT <- length(dec)
NE <- length(elece)


micro.model <- "model{

for(i in 1:NI){
cong[i] ~ dnorm(mu.cong[i],tau.cong)
mu.cong[i] <- a_cong[idni[i]] +  bcong_fem*fem[i] + bcong_age*age[i] + bcong_age2*age[i]*age[i] + bcong_edum*edum[i] + bcong_eduh*eduh[i] 
}

tau.cong <- pow(sigma.cong, -2)
sigma.cong ~ dunif(0, 50)

bcong_fem ~ dnorm(0, .0001)
bcong_age ~ dnorm(0, .0001)
bcong_age2 ~ dnorm(0, .0001)
bcong_edum ~ dnorm(0, .0001)
bcong_eduh ~ dnorm(0, .0001)

for(j in 1:N){
a_cong[j] ~ dnorm(mu0_cong[j],tau.cong0) 
mu0_cong[j] <- a30 + bcong_prop*cx1[j] + bcong_veto*cx2[j] + bcong_regime*cx3[j] + bcong_dir*cx4[j] + bcong_onetwo*cx1[j]*cx2[j] 
#+ bcong_old*old[j]
}

a30 ~ dnorm(0, .0001) 
bcong_prop ~ dnorm(0, .0001)  
bcong_veto ~ dnorm(0, .0001)
bcong_regime ~ dnorm(0, .0001)
bcong_dir ~ dnorm(0, .0001)
bcong_onetwo ~ dnorm(0, .0001)
#bcong_old ~ dnorm(0, .0001)

tau.cong0 <- pow(sigma.cong0, -2)
sigma.cong0 ~ dunif(0,100)


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
- beta.cab1*zex1[k] 
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

micro.parameters <- c("sigma.elec", "sigma.par", "sigma.exeleg", "sigma.dec", "sigma.dir", "alpha.exeleg", "alpha.dir", "gamma.elec1", "gamma.par1", "gamma.exeleg1", "gamma.dec2", "gamma.dir4", "tau.fed", "tau.bic", "tau.const", "tau.jud", "tau.pres", "beta.cab1", "beta.fed2", "beta.bic2", "beta.const2", "beta.jud2", "beta.pres3", "cx1", "cx2", "cx3", "cx4", "beta.prescab", "beta.dircab", "gamma.presexeleg", "acab", "adec", "ae", "ap", "a.elec1",  "a.par1", "a.dec2", "a.cab1", "tau.ac", "sigma.ac", "mu.ac", "mu.ad", "mu.ae", "mu.ap", "zex1", "bcong_fem" ,"bcong_age" ,"bcong_age2" ,"bcong_edum" ,"bcong_eduh", "a_cong" ,"sigma.cong0", "sigma.cong", "bcong_prop" , "bcong_veto" , "bcong_regime",  "bcong_dir" , "bcong_onetwo",
#"bcong_old", 
"a30")

jags.micro <- jags.model(file="micro.model.jags", data = micro.data, n.chains = 3, n.adapt = 100)

sampleshelp <- coda.samples(jags.micro, micro.parameters, n.iter=100, thin=1)

samplesburn <- coda.samples(jags.micro, micro.parameters, n.iter=1800, thin=18)

samples2 <- coda.samples(jags.micro, micro.parameters, n.iter=2000, thin=20)

#plot(sampleshelp, ask=TRUE)
#plot(samples2, ask=TRUE)


kette <- as.matrix(samples2)

bperclose_fem <- kette[,"bcong_fem"] 
mcongf <- mean(bperclose_fem)
scongf <- sd(bperclose_fem)

bperclose_age <- kette[,"bcong_age"]
mconga <- mean(bperclose_age)
sconga <- sd(bperclose_age)

bperclose_age2 <- kette[,"bcong_age2"]
mconga2 <- mean(bperclose_age2)
sconga2 <- sd(bperclose_age2)

bperclose_edum<- kette[,"bcong_edum"]
mcongem <- mean(bperclose_edum)
scongem <- sd(bperclose_edum)

bperclose_eduh <- kette[,"bcong_eduh"]
mcongeh <- mean(bperclose_eduh)
scongeh <- sd(bperclose_eduh)

bperclose_prop <- kette[,"bcong_prop"]
mcongp <- mean(bperclose_prop)
scongp <- sd(bperclose_prop)

bperclose_veto <- kette[,"bcong_veto"]
mcongv <- mean(bperclose_veto)
scongv <- sd(bperclose_veto)

bperclose_regime <- kette[,"bcong_regime"]
mcongr <- mean(bperclose_regime)
scongr <- sd(bperclose_regime)

bperclose_dir <- kette[,"bcong_dir"] 
mcongd <- mean(bperclose_dir)
scongd <- sd(bperclose_dir)

bperclose_onetwo <- kette[,"bcong_onetwo"]
mcongot <- mean(bperclose_onetwo)
scongot <- sd(bperclose_onetwo)


mcong <- c(mcongf, mconga*10, mconga2*10, mcongem, mcongeh, mcongp, mcongv, mcongr, mcongd, mcongot)
scong <- c(scongf, sconga*10, sconga2*10, scongem, scongeh, scongp, scongv, scongr, scongd, scongot)

vlabels <- c("Female" , "Age (x10)", "Age squared (x10)", "Education (med.)" ,"Education (high)", "Prop. power diff." ,"Decentral power diff." ,
             "Regime power diff.", "Direct power diff." , "Prop. x dec.")

var.names <- c(vlabels)
m.v <- mcong
sd.v <- scong

postscript("fig5.5b_cong_red.eps", width = 800, height = 800, horizontal=FALSE)

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-1,1), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-1,1, by = 1), label = seq(-1,1, by = 1), cex.axis=.9)
axis(2, at = y.axis, label = var.names, las = 1, tick = T, font=1, cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(m.v-qnorm(.975)*sd.v, y.axis, m.v+qnorm(.975)*sd.v, y.axis, lwd =  1.5)
abline(v=0, lty = 2)
segments(m.v-qnorm(.9)*sd.v, y.axis -.1, m.v-qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5) 
segments(m.v+qnorm(.9)*sd.v, y.axis -.1, m.v+qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .6 
segments(left.side,10,left.side,6) 
segments(left.side,10,left.side+.1,10) 
segments(left.side,6,left.side+.1,6)
text(.5, 8, "Individual Level", srt = 90, cex=.9)
segments(left.side,5,left.side,1) 
segments(left.side,5,left.side+.1,5) 
segments(left.side,1,left.side+.1,1)
text(.5, 2.5, "Country Level", srt = 90, cex=.9)

dev.off()


#Fig. 5.6: Interaction between proportional and decentral power diffusion in the explanation of policy congruence 

postscript("fig5.6_cong_red_int.eps", width = 800, height = 800, horizontal=FALSE)

par(mar=c(3,4,0,1) + 0.1)    
par(oma=c(1,1,1,0)+.1)
par(xaxs = "i") 
par(mfrow=c(1,1))
curve(mcongp + mcongot*x, from=-1.5,
      to=2,ylim=c(-.8,.6), ylab="",  xlab="", axes=F, col="white")
axis(2, at=seq(-.8, .6, by=.2))
axis(1, at=seq(-1.5, 2, by=.5))
axis(2, at = -.1, label = "Effect of Proportional Power Diffusion on Policy Congruence ('Old' Democracies)", las = 0, tick=F, outer=F, cex.axis=1, line=2)
axis(1, at = .25, label = "Decentral Power Diffusion", las = 0, tick=F, outer=F, cex.axis=1, line=2)
for (i in 100:300){
  curve(bperclose_prop[i] + bperclose_onetwo[i]*x, from=-1.5, to=2,add=T,
        col="grey", lwd=1, lty=3)
  #lty=2 or 3
}
abline(h=0)
curve(mcongp + mcongot*x, from=-1.5,
      to=2,ylim=c(-.8,.6), ylab="", xlab="", col="black", lwd=2, add=T)
box()

dev.off()

