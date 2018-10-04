######################################################
#Replication code for "Power Diffusion and Democracy"#
######################################################
# Julian Bernauer and Adrian Vatter 

# Replication code Chapter 5: satisfaction with democracy 
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
# win
win <- cses3_red$win 

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


### winner interacted with proportional power diffusion 

micro.model <- "model{

for(i in 1:NI){
demsat[i] ~ dcat(p.dem[i,1:4])
p.dem[i,1] <- Q.dem[i,1]
p.dem[i,2] <- Q.dem[i,2] - Q.dem[i,1] 
p.dem[i,3] <- Q.dem[i,3] - Q.dem[i,2] 
p.dem[i,4] <- 1 - Q.dem[i,3]
for (k.cut in 1:3){
logit(Q.dem[i,k.cut]) <-  C.dem[idni[i],k.cut] - (bdemsat_fem*fem[i] + bdemsat_age*age[i] + bdemsat_age2*age[i]*age[i] + bdemsat_edum*edum[i] + bdemsat_eduh*eduh[i] + bdemsat_win[idni[i]]*win[i])}
}

bdemsat_fem ~ dnorm(0, .0001)
bdemsat_age ~ dnorm(0, .0001)
bdemsat_age2 ~ dnorm(0, .0001)
bdemsat_edum ~ dnorm(0, .0001)
bdemsat_eduh ~ dnorm(0, .0001)
#bdemsat_win ~ dnorm(0, .0001)

for(j in 1:N){
C.dem[j,1] ~ dnorm (mu.dem[j], tau.demsat0)
mu.dem[j] <- a20 - (bdemsat_prop*cx1[j] + bdemsat_veto*cx2[j] + bdemsat_regime*cx3[j] + bdemsat_dir*cx4[j] + bdemsat_onetwo*cx1[j]*cx2[j] +  bdemsat_old*old[j]) 
delta.dem[j,1] ~ dexp(2)
delta.dem[j,2] ~ dexp(2)
C.dem[j, 2] <- C.dem[j,1] + delta.dem[j,1]
C.dem[j, 3] <- C.dem[j,2] + delta.dem[j,2]
}

a20 ~ dnorm(0, .0001)  
bdemsat_prop ~ dnorm(0, .0001)
bdemsat_veto ~ dnorm(0, .0001)
bdemsat_regime ~ dnorm(0, .0001)
bdemsat_dir ~ dnorm(0, .0001)
bdemsat_onetwo ~ dnorm(0, .0001)
bdemsat_old ~ dnorm(0, .0001)

tau.demsat0 <- pow(sigma.demsat0, -2)
sigma.demsat0 ~ dunif(0,100)

# random slope and interaction win 
for(j in 1:N){
bdemsat_win[j] ~ dnorm (mu.win[j], tau.win)
mu.win[j] <- a_win + prop_win*cx1[j] 
}

a_win ~ dnorm(0, .0001)  
prop_win ~ dnorm(0, .0001)

tau.win <- pow(sigma.win, -2)
sigma.win ~ dunif(0,100)

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

micro.data <- list(N=N, NI=NI, NT=NT, NE=NE, idn=idcc, idne = idne, idni=idni, par=pare, elec=elece, exeleg=exelegc, cab=cabe, fed=fedc, dec=dec, bic=bicc, const=constc, jud=judc, dir=dirc, pres=presc, demsat=demsat, turn=turn, manda=manda2, cong=cong, fem=fem, age=age, edum=edum, eduh=eduh, acc=acc, old=old, win=win)

micro.parameters <- c("sigma.elec", "sigma.par", "sigma.exeleg", "sigma.dec", "sigma.dir", "alpha.exeleg", "alpha.dir", "gamma.elec1", "gamma.par1", "gamma.exeleg1", "gamma.dec2", "gamma.dir4", "tau.fed", "tau.bic", "tau.const", "tau.jud", "tau.pres", "beta.cab1", "beta.fed2", "beta.bic2", "beta.const2", "beta.jud2", "beta.pres3", "cx1", "cx2", "cx3", "cx4", "beta.prescab", "beta.dircab", "gamma.presexeleg", "acab", "adec", "ae", "ap", "a.elec1",  "a.par1", "a.dec2", "a.cab1", "tau.ac", "sigma.ac", "mu.ac", "mu.ad", "mu.ae", "mu.ap", "zex1", "bdemsat_fem" ,"bdemsat_age" ,"bdemsat_age2" ,"bdemsat_edum" ,"bdemsat_eduh" , "sigma.demsat0", "bdemsat_prop" ,"bdemsat_veto" ,"bdemsat_regime", "bdemsat_dir" ,
"bdemsat_onetwo", "bdemsat_old",  "a20", "C.dem", "prop_win", "a_win", "tau.win")

jags.micro <- jags.model(file="micro.model.jags", data = micro.data, n.chains = 3, n.adapt = 100)

sampleshelp <- coda.samples(jags.micro, micro.parameters, n.iter=100, thin=1)

samplesburn <- coda.samples(jags.micro, micro.parameters, n.iter=1800, thin=18)

samples <- coda.samples(jags.micro, micro.parameters, n.iter=2000, thin=20)

# plot(sampleshelp, ask=TRUE)
# plot(samples, ask=TRUE)


#Fig. 5.9a: satisfaction with democracy full

kette <- as.matrix(samples)

bdemsat_fem <- kette[,"bdemsat_fem"] 
mdemf <- mean(bdemsat_fem)
sdemf <- sd(bdemsat_fem)

bdemsat_age <- kette[,"bdemsat_age"]
mdema <- mean(bdemsat_age)
sdema <- sd(bdemsat_age)

bdemsat_age2 <- kette[,"bdemsat_age2"]
mdema2 <- mean(bdemsat_age2)
sdema2 <- sd(bdemsat_age2)

bdemsat_edum<- kette[,"bdemsat_edum"]
mdemem <- mean(bdemsat_edum)
sdemem <- sd(bdemsat_edum)

bdemsat_eduh <- kette[,"bdemsat_eduh"]
mdemeh <- mean(bdemsat_eduh)
sdemeh <- sd(bdemsat_eduh)

bdemsat_win <- kette[,"a_win"]
mdemwi <- mean(bdemsat_win)
sdemwi <- sd(bdemsat_win)

bdemsat_propwin <- kette[,"prop_win"]
mdempw <- mean(bdemsat_propwin)
sdempw <- sd(bdemsat_propwin)

bdemsat_prop <- kette[,"bdemsat_prop"]
mdemp <- mean(bdemsat_prop)
sdemp <- sd(bdemsat_prop)

bdemsat_veto <- kette[,"bdemsat_veto"]
mdemv <- mean(bdemsat_veto)
sdemv <- sd(bdemsat_veto)

bdemsat_regime <- kette[,"bdemsat_regime"]
mdemr <- mean(bdemsat_regime)
sdemr <- sd(bdemsat_regime)

bdemsat_dir <- kette[,"bdemsat_dir"] 
mdemd <- mean(bdemsat_dir)
sdemd <- sd(bdemsat_dir)

bdemsat_onetwo <- kette[,"bdemsat_onetwo"]
mdemot <- mean(bdemsat_onetwo)
sdemot <- sd(bdemsat_onetwo)

bdemsat_old <- kette[,"bdemsat_old"]
mdemold <- mean(bdemsat_old)
sdemold <- sd(bdemsat_old)

mdem <- c(mdemf, mdema*10, mdema2*10, mdemem, mdemeh, mdemwi, mdemp, mdemv, mdemr, mdemd, mdemot, mdempw, mdemold/2)

sdem <- c(sdemf, sdema*10, sdema2*10, sdemem, sdemeh, sdemwi, sdemp, sdemv, sdemr, sdemd, sdemot, sdempw, sdemold/2)

vlabels <- c("Female" , "Age (x10)", "Age squared (x10)", "Education (med.)" ,"Education (high)", "Winner", 
             "Prop. power diff." ,"Decentral power diff." ,"Regime power diff.", "Direct power diff." , "Prop. x dec.", "Prop. x win.", "'Old' democracy (/2)")

var.names <- c(vlabels)
m.v <- mdem
sd.v <- sdem

postscript("fig5.9a_demsat_full.eps", width = 800, height = 800, horizontal=FALSE)

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-1,1.5), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-1,1.5, by = .5), label = seq(-1,1.5, by = .5), cex.axis=.9)
axis(2, at = y.axis, label = var.names, las = 1, tick = T, font=1, cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(m.v-qnorm(.975)*sd.v, y.axis, m.v+qnorm(.975)*sd.v, y.axis, lwd =  1.5)
abline(v=0, lty = 2)
segments(m.v-qnorm(.9)*sd.v, y.axis -.1, m.v-qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5) 
segments(m.v+qnorm(.9)*sd.v, y.axis -.1, m.v+qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .6 
segments(left.side,13,left.side,8) 
segments(left.side,13,left.side+.1,13) 
segments(left.side,8,left.side+.1,8)
text(.5, 10.5, "Individual Level", srt = 90, cex=.9)
segments(left.side,7,left.side,1) 
segments(left.side,7,left.side+.1,7) 
segments(left.side,1,left.side+.1,1)
text(.5, 4, "Country/Cross Level", srt = 90, cex=.9)

dev.off() 



#Fig. 5.10a: Interaction between proportional power diffusion and winner for satisfaction with democracy  

postscript("fig5.10a_cong_full_propwin.eps", width = 800, height = 800, horizontal=FALSE)

par(mar=c(3,4,0,1) + 0.1)    
par(oma=c(1,1,1,0)+.1)
par(xaxs = "i") 
par(mfrow=c(1,1))
curve(mcongp + mcongot*x, from=-1.5,
      to=2,ylim=c(-.8,.6), ylab="",  xlab="", axes=F, col="white")
axis(2, at=seq(-.8, .6, by=.2))
axis(1, at=seq(-1.5, 2, by=.5))
axis(2, at = -.1, label = "Effect of proportional power diffusion on satisfaction with democracy", las = 0, tick=F, outer=F, cex.axis=1, line=2)
axis(1, at = .25, label = "Winner vs. loser", las = 0, tick=F, outer=F, cex.axis=1, line=2)
for (i in 100:300){
  curve(a_win[i], from=-1.5, to=2,add=T,
        col="grey", lwd=1, lty=3)
  curve(a_win[i] + prop_win[i], from=-1.5, to=2,add=T,
        col="grey", lwd=1, lty=3)
  #lty=2 or 3
}
abline(h=0)
curve(mdemwi, from=-1.5,
      to=2,ylim=c(-.8,.6), ylab="", xlab="", col="black", lwd=2, lty=2, add=T)
curve(mdemwi + mdempw, from=-1.5,
      to=2,ylim=c(-.8,.6), ylab="", xlab="", col="black", lwd=2, lty=1, add=T)
box()

dev.off()


###
# winner interacted with direct power diffusion 

micro.model <- "model{

for(i in 1:NI){
demsat[i] ~ dcat(p.dem[i,1:4])
p.dem[i,1] <- Q.dem[i,1]
p.dem[i,2] <- Q.dem[i,2] - Q.dem[i,1] 
p.dem[i,3] <- Q.dem[i,3] - Q.dem[i,2] 
p.dem[i,4] <- 1 - Q.dem[i,3]
for (k.cut in 1:3){
logit(Q.dem[i,k.cut]) <-  C.dem[idni[i],k.cut] - (bdemsat_fem*fem[i] + bdemsat_age*age[i] + bdemsat_age2*age[i]*age[i] + bdemsat_edum*edum[i] + bdemsat_eduh*eduh[i] + bdemsat_win[idni[i]]*win[i])}
}

bdemsat_fem ~ dnorm(0, .0001)
bdemsat_age ~ dnorm(0, .0001)
bdemsat_age2 ~ dnorm(0, .0001)
bdemsat_edum ~ dnorm(0, .0001)
bdemsat_eduh ~ dnorm(0, .0001)
#bdemsat_win ~ dnorm(0, .0001)

for(j in 1:N){
C.dem[j,1] ~ dnorm (mu.dem[j], tau.demsat0)
mu.dem[j] <- a20 - (bdemsat_prop*cx1[j] + bdemsat_veto*cx2[j] + bdemsat_regime*cx3[j] + bdemsat_dir*cx4[j] + bdemsat_onetwo*cx1[j]*cx2[j] +  bdemsat_old*old[j]) 
delta.dem[j,1] ~ dexp(2)
delta.dem[j,2] ~ dexp(2)
C.dem[j, 2] <- C.dem[j,1] + delta.dem[j,1]
C.dem[j, 3] <- C.dem[j,2] + delta.dem[j,2]
}

a20 ~ dnorm(0, .0001)  
bdemsat_prop ~ dnorm(0, .0001)
bdemsat_veto ~ dnorm(0, .0001)
bdemsat_regime ~ dnorm(0, .0001)
bdemsat_dir ~ dnorm(0, .0001)
bdemsat_onetwo ~ dnorm(0, .0001)
bdemsat_old ~ dnorm(0, .0001)

tau.demsat0 <- pow(sigma.demsat0, -2)
sigma.demsat0 ~ dunif(0,100)

# random slope and interaction win 
for(j in 1:N){
bdemsat_win[j] ~ dnorm (mu.win[j], tau.win)
mu.win[j] <- a_win 
#+ prop_win*cx1[j] 
+ dir_win*cx4[j] 
}

a_win ~ dnorm(0, .0001)  
#prop_win ~ dnorm(0, .0001)
dir_win ~ dnorm(0, .0001)

tau.win <- pow(sigma.win, -2)
sigma.win ~ dunif(0,100)

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

micro.data <- list(N=N, NI=NI, NT=NT, NE=NE, idn=idcc, idne = idne, idni=idni, par=pare, elec=elece, exeleg=exelegc, cab=cabe, fed=fedc, dec=dec, bic=bicc, const=constc, jud=judc, dir=dirc, pres=presc, demsat=demsat, turn=turn, manda=manda2, cong=cong, fem=fem, age=age, edum=edum, eduh=eduh, acc=acc, old=old, win=win)

micro.parameters <- c("sigma.elec", "sigma.par", "sigma.exeleg", "sigma.dec", "sigma.dir", "alpha.exeleg", "alpha.dir", "gamma.elec1", "gamma.par1", "gamma.exeleg1", "gamma.dec2", "gamma.dir4", "tau.fed", "tau.bic", "tau.const", "tau.jud", "tau.pres", "beta.cab1", "beta.fed2", "beta.bic2", "beta.const2", "beta.jud2", "beta.pres3", "cx1", "cx2", "cx3", "cx4", "beta.prescab", "beta.dircab", "gamma.presexeleg", "acab", "adec", "ae", "ap", "a.elec1",  "a.par1", "a.dec2", "a.cab1", "tau.ac", "sigma.ac", "mu.ac", "mu.ad", "mu.ae", "mu.ap", "zex1", "bdemsat_fem" ,"bdemsat_age" ,"bdemsat_age2" ,"bdemsat_edum" ,"bdemsat_eduh" , "sigma.demsat0", "bdemsat_prop" ,"bdemsat_veto" ,"bdemsat_regime", "bdemsat_dir" ,"bdemsat_onetwo", "bdemsat_old", "a20", "C.dem", "dir_win", "a_win", "tau.win")

jags.micro <- jags.model(file="micro.model.jags", data = micro.data, n.chains = 3, n.adapt = 100)

sampleshelp <- coda.samples(jags.micro, micro.parameters, n.iter=100, thin=1)

samplesburn <- coda.samples(jags.micro, micro.parameters, n.iter=1800, thin=18)

samples <- coda.samples(jags.micro, micro.parameters, n.iter=2000, thin=20)

# plot(sampleshelp, ask=TRUE)
# plot(samples, ask=TRUE)


kette <- as.matrix(samples)

bdemsat_fem <- kette[,"bdemsat_fem"] 
mdemf <- mean(bdemsat_fem)
sdemf <- sd(bdemsat_fem)

bdemsat_age <- kette[,"bdemsat_age"]
mdema <- mean(bdemsat_age)
sdema <- sd(bdemsat_age)

bdemsat_age2 <- kette[,"bdemsat_age2"]
mdema2 <- mean(bdemsat_age2)
sdema2 <- sd(bdemsat_age2)

bdemsat_edum<- kette[,"bdemsat_edum"]
mdemem <- mean(bdemsat_edum)
sdemem <- sd(bdemsat_edum)

bdemsat_eduh <- kette[,"bdemsat_eduh"]
mdemeh <- mean(bdemsat_eduh)
sdemeh <- sd(bdemsat_eduh)

bdemsat_win <- kette[,"a_win"]
mdemwi <- mean(bdemsat_win)
sdemwi <- sd(bdemsat_win)

bdemsat_dirwin <- kette[,"dir_win"]
mdemdw <- mean(bdemsat_dirwin)
sdemdw <- sd(bdemsat_dirwin)

bdemsat_prop <- kette[,"bdemsat_prop"]
mdemp <- mean(bdemsat_prop)
sdemp <- sd(bdemsat_prop)

bdemsat_veto <- kette[,"bdemsat_veto"]
mdemv <- mean(bdemsat_veto)
sdemv <- sd(bdemsat_veto)

bdemsat_regime <- kette[,"bdemsat_regime"]
mdemr <- mean(bdemsat_regime)
sdemr <- sd(bdemsat_regime)

bdemsat_dir <- kette[,"bdemsat_dir"] 
mdemd <- mean(bdemsat_dir)
sdemd <- sd(bdemsat_dir)

bdemsat_onetwo <- kette[,"bdemsat_onetwo"]
mdemot <- mean(bdemsat_onetwo)
sdemot <- sd(bdemsat_onetwo)

bdemsat_old <- kette[,"bdemsat_old"]
mdemold <- mean(bdemsat_old)
sdemold <- sd(bdemsat_old)

mdem <- c(mdemf, mdema*10, mdema2*10, mdemem, mdemeh, mdemwi, mdemp, mdemv, mdemr, mdemd, mdemot, mdemdw, mdemold/2)

sdem <- c(sdemf, sdema*10, sdema2*10, sdemem, sdemeh, sdemwi, sdemp, sdemv, sdemr, sdemd, sdemot, sdemdw, sdemold/2)

vlabels <- c("Female" , "Age (x10)", "Age squared (x10)", "Education (med.)" ,"Education (high)", "Winner", 
             "Prop. power diff." ,"Decentral power diff." ,"Regime power diff.", "Direct power diff." , "Prop. x dec.", "Dir. x win.", "'Old' democracy (/2)")

var.names <- c(vlabels)
m.v <- mdem
sd.v <- sdem


postscript("fig5.9b_demsat_full_dir.eps", width = 800, height = 800, horizontal=FALSE)

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-1,1.5), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-1,1.5, by = .5), label = seq(-1,1.5, by = .5), cex.axis=.9)
axis(2, at = y.axis, label = var.names, las = 1, tick = T, font=1, cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(m.v-qnorm(.975)*sd.v, y.axis, m.v+qnorm(.975)*sd.v, y.axis, lwd =  1.5)
abline(v=0, lty = 2)
segments(m.v-qnorm(.9)*sd.v, y.axis -.1, m.v-qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5) 
segments(m.v+qnorm(.9)*sd.v, y.axis -.1, m.v+qnorm(.9)*sd.v, y.axis +.1, lwd = 1.5)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .6 
segments(left.side,13,left.side,8) 
segments(left.side,13,left.side+.1,13) 
segments(left.side,8,left.side+.1,8)
text(.5, 10.5, "Individual level", srt = 90, cex=.9)
segments(left.side,7,left.side,1) 
segments(left.side,7,left.side+.1,7) 
segments(left.side,1,left.side+.1,1)
text(.5, 4, "Country/cross level", srt = 90, cex=.9)

dev.off() 


#Fig. 5.10a: Interaction between direct power diffusion and winner for satisfaction with democracy  

postscript("fig5.10a_demsat_full_dirwin.eps", width = 800, height = 800, horizontal=FALSE)

par(mar=c(3,4,0,1) + .1)    
par(oma=c(1,1,1,0) + .1)
par(xaxs = "i") 
par(mfrow=c(1,1))
curve(mdemwi + mdemdw*x, from=-1,to=3,ylim=c(0,1), ylab="",  xlab="", axes=F, col="white")
axis(2, at=seq(0, 1, by=.2))
axis(1, at=seq(-1, 3, by=.5))

axis(2, at = .5, label = "Effect of Winner on Satisfaction with Democracy", las = 0, tick=F, outer=F, cex.axis=1, line=2)

axis(1, at = 1, label = "Direct Power Diffusion", las = 0, tick=F, outer=F, cex.axis=1, line=2)

curve(mdemwi + mdemdw*x, from=-1,to=3,ylim=c(0,1), ylab="", xlab="", col="black", lwd=2, lty=1, add=T)

for (i in 100:300){
  
curve(bdemsat_win[i] + bdemsat_dirwin[i]*x, from=-1,to=3,add=T,col="grey", lwd=1, lty=3)
  
}

box()

dev.off()



