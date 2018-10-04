######################################################
#Replication code for "Power Diffusion and Democracy"#
######################################################
# Julian Bernauer and Adrian Vatter 

# Replication code Chapter 5: perceptions of accountability    
library(R2jags)
dir <- "[...]"
setwd(dir)

# prepared data with CSES and PDD files 
load("PDD2018_ch5_leg.Rdata")

# Exlude Romania, implausible score of accountability 
cses3_red <- subset(cses3_red, cses3_red$ccodealp!="ROU")   
cses3_red <- cses3_red[order(cses3_red$ccodealp),]   
DPD <- subset(DPD, DPD$cname!="Romania") 

DPD <- DPD[order(DPD$ccodealp, DPD$year),]   

# new id for reduced set of countries (here 27)
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

N <- 27
NI <- length(turn)
NT <- length(dec)
NE <- length(elece)


micro.model <- "model{

for(i in 1:NI){
acc[i] ~ dcat(p.acc[i,1:5])
p.acc[i,1] <- Q.acc[i,1]
p.acc[i,2] <- Q.acc[i,2] - Q.acc[i,1] 
p.acc[i,3] <- Q.acc[i,3] - Q.acc[i,2] 
p.acc[i,4] <- Q.acc[i,4] - Q.acc[i,3] 
p.acc[i,5] <- 1 - Q.acc[i,4]
for (k.cut in 1:4){
logit(Q.acc[i,k.cut]) <-  C.acc[idni[i],k.cut] - (bacc_fem*fem[i] + bacc_age*age[i] + bacc_age2*age[i]*age[i] + bacc_edum*edum[i] + bacc_eduh*eduh[i] )}
}

bacc_fem ~ dnorm(0, .0001)
bacc_age ~ dnorm(0, .0001)
bacc_age2 ~ dnorm(0, .0001)
bacc_edum ~ dnorm(0, .0001)
bacc_eduh ~ dnorm(0, .0001)

for(j in 1:N){
C.acc[j,1] ~ dnorm (mu.acc[j], tau.acc0)
mu.acc[j] <- a40 - (bacc_prop*cx1[j] + bacc_veto*cx2[j] + bacc_regime*cx3[j] + bacc_dir*cx4[j] + bacc_onetwo*cx1[j]*cx2[j] +  bacc_old*old[j]) 
delta.acc[j,1] ~ dexp(2)
delta.acc[j,2] ~ dexp(2)
delta.acc[j,3] ~ dexp(2)
C.acc[j, 2] <- C[j,1] + delta.acc[j,1]
C.acc[j, 3] <- C[j,2] + delta.acc[j,2]
C.acc[j, 4] <- C[j,3] + delta.acc[j,3]
}

tau.acc0 <- pow(sigma.acc0, -2)
sigma.acc0 ~ dunif(0,100)

a40 ~ dnorm(0, .0001) 
bacc_prop ~ dnorm(0, .0001)
bacc_veto ~ dnorm(0, .0001)
bacc_regime ~ dnorm(0, .0001)
bacc_dir ~ dnorm(0, .0001)
bacc_onetwo ~ dnorm(0, .0001)
bacc_old ~ dnorm(0, .0001)


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

#CH 3, US 27, max. 27

for(k in 4:26){
cx1[k] ~ dnorm(0,1) 
cx2[k] ~ dnorm(0,1)  
cx3[k] ~ dnorm(0,1) 
cx4[k] ~ dnorm(0,1) 

}

cx1[1] ~ dnorm(0,1)
cx1[2] ~ dnorm(0,1) 
cx1[3] ~ dnorm(0,1) I(0,)
cx1[27] ~ dnorm(0,1)

cx2[1] ~ dnorm(0,1) 
cx2[2] ~ dnorm(0,1) 
cx2[3] ~ dnorm(0,1)I(0,)
cx2[27] ~ dnorm(0,1)

cx3[1] ~ dnorm(0,1) 
cx3[2] ~ dnorm(0,1) 
cx3[3] ~ dnorm(0,1) 
cx3[27] ~ dnorm(0,1) I(0,)

cx4[1] ~ dnorm(0,1) I(0,)
cx4[2] ~ dnorm(0,1) 
cx4[3] ~ dnorm(0,1)
cx4[27] ~ dnorm(0,1)I(0,)

beta.dircab ~ dnorm(0, .001) 
beta.prescab ~ dnorm(0, .001) 
gamma.presexeleg  ~ dnorm(0, .001) I(,0)

}"

write(micro.model, file="micro.model.jags")

micro.data <- list(N=N, NI=NI, NT=NT, NE=NE, idn=idcc, idne = idne, idni=idni, par=pare, elec=elece, exeleg=exelegc, cab=cabe, fed=fedc, dec=dec, bic=bicc, const=constc, jud=judc, dir=dirc, pres=presc, demsat=demsat, turn=turn, cong=cong, fem=fem, age=age, edum=edum, eduh=eduh, acc=acc, old=old)

micro.parameters <- c("sigma.elec", "sigma.par", "sigma.exeleg", "sigma.dec", "sigma.dir", "alpha.exeleg", "alpha.dir", "gamma.elec1", "gamma.par1", "gamma.exeleg1", "gamma.dec2", "gamma.dir4", "tau.fed", "tau.bic", "tau.const", "tau.jud", "tau.pres", "beta.cab1", "beta.fed2", "beta.bic2", "beta.const2", "beta.jud2", "beta.pres3", "cx1", "cx2", "cx3", "cx4", "beta.prescab", "beta.dircab", "gamma.presexeleg", "acab", "adec", "ae", "ap", "a.elec1",  "a.par1", "a.dec2", "a.cab1", "tau.ac", "sigma.ac", "mu.ac", "mu.ad", "mu.ae", "mu.ap", "zex1", "bacc_fem", "bacc_age", "bacc_age2", "bacc_edum", "bacc_eduh", "sigma.acc0", "bacc_prop", "bacc_veto", "bacc_regime", "bacc_dir", "bacc_onetwo", "bacc_old", "a40", "C.acc")

jags.micro <- jags.model(file="micro.model.jags", data = micro.data, n.chains = 3, n.adapt = 100)

sampleshelp <- coda.samples(jags.micro, micro.parameters, n.iter=100, thin=1)

samplesburn <- coda.samples(jags.micro, micro.parameters, n.iter=1800, thin=18)

samples <- coda.samples(jags.micro, micro.parameters, n.iter=2000, thin=20)

# plot(sampleshelp, ask=TRUE)
# plot(samples, ask=TRUE)

kette <- as.matrix(samples)

#Fig. 5.7: model of accountability
bacc_fem <- kette[,"bacc_fem"] 
maccf <- mean(bacc_fem)
saccf <- sd(bacc_fem)

bacc_age <- kette[,"bacc_age"]
macca <- mean(bacc_age)
sacca <- sd(bacc_age)

bacc_age2 <- kette[,"bacc_age2"]
macca2 <- mean(bacc_age2)
sacca2 <- sd(bacc_age2)

bacc_edum<- kette[,"bacc_edum"]
maccem <- mean(bacc_edum)
saccem <- sd(bacc_edum)

bacc_eduh <- kette[,"bacc_eduh"]
macceh <- mean(bacc_eduh)
sacceh <- sd(bacc_eduh)

bacc_prop <- kette[,"bacc_prop"]
maccp <- mean(bacc_prop)
saccp <- sd(bacc_prop)

bacc_veto <- kette[,"bacc_veto"]
maccv <- mean(bacc_veto)
saccv <- sd(bacc_veto)

bacc_regime <- kette[,"bacc_regime"]
maccr <- mean(bacc_regime)
saccr <- sd(bacc_regime)

bacc_dir <- kette[,"bacc_dir"] 
maccd <- mean(bacc_dir)
saccd <- sd(bacc_dir)

bacc_onetwo <- kette[,"bacc_onetwo"]
maccot <- mean(bacc_onetwo)
saccot <- sd(bacc_onetwo)

bacc_old <- kette[,"bacc_old"]
maccold <- mean(bacc_old)
saccold <- sd(bacc_old)

macc <- c(maccf, macca*10, macca2*10, maccem, macceh, maccp, maccv, maccr, maccd, maccot, maccold/5)

sacc <- c(saccf, sacca*10, sacca2*10, saccem, sacceh, saccp, saccv, saccr, saccd, saccot, saccold/5)

vlabels <- c("Female" , "Age (x10)", "Age squared (x10)", "Education (med.)" ,"Education (high)", "Prop. power diff." ,"Decentral power diff." ,"Regime power diff.", "Direct power diff." , "Prop. x dec.", "'Old' democracy (/5)")

var.names <- c(vlabels)
m.v <- macc
sd.v <- sacc

postscript("fig5.7_acc_full.eps", width = 800, height = 800, horizontal=FALSE)

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
#draw vertical tick marks at 80% confidence intervals
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

