#################################################################
#Replication code for "Power Diffusion and Democracy": Chapter 3#
#################################################################
# Julian Bernauer and Adrian Vatter 

# Replication code factor analytical item response theoretical (FAIRT) approach to generate scores of power diffusion 
# Uses R and JAGS 

library(foreign)
library(R2WinBUGS)
library(R2jags)
library(rjags)

dir <- "[...]"

setwd(dir)

load("DPD2018July.Rdata")
DPD <- DPD18
# ls(DPD)
attach(DPD)

# Main measurement model 
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
# Country-level model for electoral disproportionality and the number of parties
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
# Country-year- and country-level model for fiscal decentralization
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
# Country-level models for the remaining indicators
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
# country-years (decentral power diffusion),
for(i in 1:NT){
x2[i] ~ dnorm(cx2[idn[i]],1)
}
# or countries (all variants)
for(k in 1:55){
cx1[k] ~ dnorm(0,1)
cx2[k] ~ dnorm(0,1)
cx3[k] ~ dnorm(0,1)
cx4[k] ~ dnorm(0,1)
}
# Restriction of country-level proportional power diffusion for Switzerland
cx1[56] ~ dnorm(0,1) I(0,)
cx1[57] ~ dnorm(0,1)
cx1[58] ~ dnorm(0,1)
cx1[59] ~ dnorm(0,1)
cx1[60] ~ dnorm(0,1)
cx1[61] ~ dnorm(0,1)
# Restriction of country-level decentral power diffusion for Switzerland
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
# Restriction of country-level presidential power diffusion for the United States
cx3[60] ~ dnorm(0,1) I(0,)
cx3[61] ~ dnorm(0,1)
# Restriction of country-level direct power diffusion for Switzerland
cx4[56] ~ dnorm(0,1) I(0,)
cx4[57] ~ dnorm(0,1)
cx4[58] ~ dnorm(0,1)
cx4[59] ~ dnorm(0,1)
cx4[60] ~ dnorm(0,1)
cx4[61] ~ dnorm(0,1)
# Collected cross-indicator effects
beta.dircab ~ dnorm(0, .001)
beta.prescab ~ dnorm(0, .001)
# Restriction of relationship between presidentialism and parliamentary power
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

a.elec1 <- kette[,"a.elec1"]
mael1 <- mean(a.elec1)
sael1 <- sd(a.elec1)
a.par1 <- kette[,"a.par1"]
map1 <- mean(a.par1)
sap1 <- sd(a.par1)
a.cab1 <- kette[,"a.cab1"]
mac1 <- mean(a.cab1)
sac1 <- sd(a.cab1)
a.dec2 <- kette[,"a.dec2"]
madec2 <- mean(a.dec2)
sadec2 <- sd(a.dec2)

gamma.elec1 <- kette[,"gamma.elec1"]
mgel1 <- mean(gamma.elec1)
sgel1 <- sd(gamma.elec1)
gamma.par1 <- kette[,"gamma.par1"]
mgp1 <- mean(gamma.par1)
sgp1 <- sd(gamma.par1)
beta.cab1 <- kette[,"beta.cab1"]
mbc1 <- mean(beta.cab1)
sbc1 <- sd(beta.cab1)
gamma.exeleg1 <- kette[,"gamma.exeleg1"]
mgex1 <- mean(gamma.exeleg1)
sgex1 <- sd(gamma.exeleg1)

beta.fed2 <- kette[,"beta.fed2"]
mbf2 <- mean(beta.fed2)
sbf2 <- sd(beta.fed2)
gamma.dec2 <- kette[,"gamma.dec2"]
mgdec2 <- mean(gamma.dec2)
sgdec2 <- sd(gamma.dec2)
beta.bic2 <- kette[,"beta.bic2"]
mbb2 <- mean(beta.bic2)
sbb2 <- sd(beta.bic2)
beta.jud2 <- kette[,"beta.jud2"]
mbj2 <- mean(beta.jud2)
sbj2 <- sd(beta.jud2)
beta.const2 <- kette[,"beta.const2"]
mbc2 <- mean(beta.const2)
sbc2 <- sd(beta.const2)

beta.pres3 <- kette[,"beta.pres3"]
mbp3 <- mean(beta.pres3)
sbp3 <- sd(beta.pres3)

gamma.dir4 <- kette[,"gamma.dir4"]
mgdir4 <- mean(gamma.dir4)
sgdir4 <- sd(gamma.dir4)

gamma.elecpar <- kette[,"gamma.elecpar"]
mgep <- mean(gamma.elecpar)
sgep <- sd(gamma.elecpar)
gamma.feddec <- kette[,"gamma.feddec"]
mgfd <- mean(gamma.feddec)
sgfd <- sd(gamma.feddec)
beta.parcab <- kette[,"beta.parcab"]
mbpac <- mean(beta.parcab)
sbpac <- sd(beta.parcab)
beta.fedbic <- kette[,"beta.fedbic"]
mbfb <- mean(beta.fedbic)
sbfb <- sd(beta.fedbic)
beta.fedconst <- kette[,"beta.fedconst"]
mbfeco <- mean(beta.fedconst)
sbfeco <- sd(beta.fedconst)
beta.constjud <- kette[,"beta.constjud"]
mbcj <- mean(beta.constjud)
sbcj <- sd(beta.constjud)
beta.prescab <- kette[,"beta.prescab"]
mbprc <- mean(beta.prescab)
sbprc <- sd(beta.prescab)
beta.dircab <- kette[,"beta.dircab"]
mbdc <- mean(beta.dircab)
sbdc <- sd(beta.dircab)
gamma.presexeleg <- kette[,"gamma.presexeleg"]
mgpe <- mean(gamma.presexeleg)
sgpe <- sd(gamma.presexeleg)

stdmgel1 <- (mgel1/sd(elece, na.rm=TRUE))*3.29
stdmgp1 <- (mgp1/sd(pare, na.rm=TRUE))*3.29
stdmgex1 <- (mgex1/sd(exelegc, na.rm=TRUE))*3.29
stdmgdec2 <- (mgdec2/sd(DPDc$dec, na.rm=TRUE))*3.29
stdmgdir4 <- (mgdir4/sd(dirc, na.rm=TRUE))*3.29

stdmael1 <- (mael1/sd(DPDc$elec, na.rm=TRUE))*3.29
stdmap1 <- (map1/sd(DPDc$par, na.rm=TRUE))*3.29
stdmadec2 <- (madec2/sd(DPDc$dec, na.rm=TRUE))*3.29

stdsgel1 <- (sgel1/sd(elece, na.rm=TRUE))*3.29
stdsgp1 <- (sgp1/sd(pare, na.rm=TRUE))*3.29
stdsgex1 <- (sgex1/sd(exelegc, na.rm=TRUE))*3.29
stdsgdec2 <- (sgdec2/sd(DPDc$dec, na.rm=TRUE))*3.29
stdsgdir4 <- (sgdir4/sd(dirc, na.rm=TRUE))*3.29

stdsael1 <- (sael1/sd(DPDc$elec, na.rm=TRUE))*3.29
stdsap1 <- (sap1/sd(DPDc$par, na.rm=TRUE))*3.29
stdsadec2 <- (sadec2/sd(DPDc$dec, na.rm=TRUE))*3.29

mcoef <- c(stdmael1,stdmgel1,stdmap1,stdmgp1,mac1,mbc1,stdmgex1,mbf2/5,stdmadec2,stdmgdec2,mbb2,mbj2,mbc2,mbp3/5,stdmgdir4/5)
scoef <- c(stdsael1,stdsgel1,stdsap1,stdsgp1,sac1,sbc1,stdsgex1,sbf2/5,stdsadec2,stdsgdec2,sbb2,sbj2,sbc2,sbp3/5,stdsgdir4/5)

stdmgep <- (mgep/sd(pare, na.rm=TRUE))*3.29
stdmgfd <- (mgfd/sd(dec, na.rm=TRUE))*3.29
stdmgpe <- (mgpe/sd(exelegc, na.rm=TRUE))*3.29

stdsgep <- (sgep/sd(pare, na.rm=TRUE))*3.29
stdsgfd <- (sgfd/sd(dec, na.rm=TRUE))*3.29
stdsgpe <- (sgpe/sd(exelegc, na.rm=TRUE))*3.29

mq <- c(mbprc,stdmgpe,mbdc)
sq <- c(sbprc,stdsgpe,sbdc)

m.v <- c(mcoef,mq)
sd.v <- c(scoef,sq)

var.names <- c("Electoral disproportionality","Elect. disprop. (intra-country)","Effective nr. of parties","Eff. nr. of part. (intra-country)","Cabinet type","Cab. type (intra-country)","Parliamentary power","Federalism (/5)","Decentralisation","Dec. (intra-country)","Bicameralism","Judicial review","Constitutional rigidity","Presidentialism (/5)","Direct democracy (/5)","Pres. -> cab.","Pres. -> parl. pow.","Dir. -> cab.")


# Figure 3.4: Measurement Model 
postscript("fig_3_4.eps", width = 800, height = 800, horizontal=FALSE) 

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-4,4), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-4,4, by = 2), label = seq(-4,4, by = 2), cex.axis=.9)
axis(2, at = y.axis, label = var.names, las = 1, tick = T, font=1, cex.axis=.7)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(m.v-qnorm(.975)*sd.v, y.axis, m.v+qnorm(.975)*sd.v, y.axis, lwd =  1.5)
abline(v=0, lty = 2)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .7 
segments(left.side,18,left.side,12) 
segments(left.side,18,left.side+.1,18) 
segments(left.side,12,left.side+.1,12)
text(.5, 15, "Proportional", srt = 90, cex=.8)
segments(left.side,11,left.side,6) 
segments(left.side,11,left.side+.1,11) 
segments(left.side,6,left.side+.1,6)
text(.5, 8.5, "Decentral", srt = 90, cex=.8)
segments(left.side,5,left.side,5) 
segments(left.side,5,left.side+.1,5) 
segments(left.side,5,left.side+.1,5)
text(.5, 5, "Pres.", srt = 90, cex=.8)
segments(left.side,4,left.side,4) 
segments(left.side,4,left.side+.1,4) 
segments(left.side,4,left.side+.1,4)
text(.5, 4, "Dir.", srt = 90, cex=.8)
segments(left.side,3,left.side,1) 
segments(left.side,3,left.side+.1,3) 
segments(left.side,1,left.side+.1,1)
text(.5, 2, "Inter-Dim. Eff.", srt = 90, cex=.8)

dev.off()


# Country scores estimated

# alternatively use adaption of 
# output <- as.data.frame(kette) 
# x1 <- 0
# for(i in [...]:[...]){x1[i] <- mean(output[,i])}
# x1 <- x1[[...]:[...]]

cx11 <- kette[,"cx1[1]"]
cx12 <- kette[,"cx1[2]"]
cx13 <- kette[,"cx1[3]"]
cx14 <- kette[,"cx1[4]"]
cx15 <- kette[,"cx1[5]"]
cx16 <- kette[,"cx1[6]"]
cx17 <- kette[,"cx1[7]"]
cx18 <- kette[,"cx1[8]"]
cx19 <- kette[,"cx1[9]"]
cx110 <- kette[,"cx1[10]"]
cx111 <- kette[,"cx1[11]"]
cx112 <- kette[,"cx1[12]"]
cx113 <- kette[,"cx1[13]"]
cx114 <- kette[,"cx1[14]"]
cx115 <- kette[,"cx1[15]"]
cx116 <- kette[,"cx1[16]"]
cx117 <- kette[,"cx1[17]"]
cx118 <- kette[,"cx1[18]"]
cx119 <- kette[,"cx1[19]"]
cx120 <- kette[,"cx1[20]"]
cx121 <- kette[,"cx1[21]"]
cx122 <- kette[,"cx1[22]"]
cx123 <- kette[,"cx1[23]"]
cx124 <- kette[,"cx1[24]"]
cx125 <- kette[,"cx1[25]"]
cx126 <- kette[,"cx1[26]"]
cx127 <- kette[,"cx1[27]"]
cx128 <- kette[,"cx1[28]"]
cx129 <- kette[,"cx1[29]"]
cx130 <- kette[,"cx1[30]"]
cx131 <- kette[,"cx1[31]"]
cx132 <- kette[,"cx1[32]"]
cx133 <- kette[,"cx1[33]"]
cx134 <- kette[,"cx1[34]"]
cx135 <- kette[,"cx1[35]"]
cx136 <- kette[,"cx1[36]"]
cx137 <- kette[,"cx1[37]"]
cx138 <- kette[,"cx1[38]"]
cx139 <- kette[,"cx1[39]"]
cx140 <- kette[,"cx1[40]"]
cx141 <- kette[,"cx1[41]"]
cx142 <- kette[,"cx1[42]"]
cx143 <- kette[,"cx1[43]"]
cx144 <- kette[,"cx1[44]"]
cx145 <- kette[,"cx1[45]"]
cx146 <- kette[,"cx1[46]"]
cx147 <- kette[,"cx1[47]"]
cx148 <- kette[,"cx1[48]"]
cx149 <- kette[,"cx1[49]"]
cx150 <- kette[,"cx1[50]"]
cx151 <- kette[,"cx1[51]"]
cx152 <- kette[,"cx1[52]"]
cx153 <- kette[,"cx1[53]"]
cx154 <- kette[,"cx1[54]"]
cx155 <- kette[,"cx1[55]"]
cx156 <- kette[,"cx1[56]"]
cx157 <- kette[,"cx1[57]"]
cx158 <- kette[,"cx1[58]"]
cx159 <- kette[,"cx1[59]"]
cx160 <- kette[,"cx1[60]"]
cx161 <- kette[,"cx1[61]"]

cx21 <- kette[,"cx2[1]"]
cx22 <- kette[,"cx2[2]"]
cx23 <- kette[,"cx2[3]"]
cx24 <- kette[,"cx2[4]"]
cx25 <- kette[,"cx2[5]"]
cx26 <- kette[,"cx2[6]"]
cx27 <- kette[,"cx2[7]"]
cx28 <- kette[,"cx2[8]"]
cx29 <- kette[,"cx2[9]"]
cx210 <- kette[,"cx2[10]"]
cx211 <- kette[,"cx2[11]"]
cx212 <- kette[,"cx2[12]"]
cx213 <- kette[,"cx2[13]"]
cx214 <- kette[,"cx2[14]"]
cx215 <- kette[,"cx2[15]"]
cx216 <- kette[,"cx2[16]"]
cx217 <- kette[,"cx2[17]"]
cx218 <- kette[,"cx2[18]"]
cx219 <- kette[,"cx2[19]"]
cx220 <- kette[,"cx2[20]"]
cx221 <- kette[,"cx2[21]"]
cx222 <- kette[,"cx2[22]"]
cx223 <- kette[,"cx2[23]"]
cx224 <- kette[,"cx2[24]"]
cx225 <- kette[,"cx2[25]"]
cx226 <- kette[,"cx2[26]"]
cx227 <- kette[,"cx2[27]"]
cx228 <- kette[,"cx2[28]"]
cx229 <- kette[,"cx2[29]"]
cx230 <- kette[,"cx2[30]"]
cx231 <- kette[,"cx2[31]"]
cx232 <- kette[,"cx2[32]"]
cx233 <- kette[,"cx2[33]"]
cx234 <- kette[,"cx2[34]"]
cx235 <- kette[,"cx2[35]"]
cx236 <- kette[,"cx2[36]"]
cx237 <- kette[,"cx2[37]"]
cx238 <- kette[,"cx2[38]"]
cx239 <- kette[,"cx2[39]"]
cx240 <- kette[,"cx2[40]"]
cx241 <- kette[,"cx2[41]"]
cx242 <- kette[,"cx2[42]"]
cx243 <- kette[,"cx2[43]"]
cx244 <- kette[,"cx2[44]"]
cx245 <- kette[,"cx2[45]"]
cx246 <- kette[,"cx2[46]"]
cx247 <- kette[,"cx2[47]"]
cx248 <- kette[,"cx2[48]"]
cx249 <- kette[,"cx2[49]"]
cx250 <- kette[,"cx2[50]"]
cx251 <- kette[,"cx2[51]"]
cx252 <- kette[,"cx2[52]"]
cx253 <- kette[,"cx2[53]"]
cx254 <- kette[,"cx2[54]"]
cx255 <- kette[,"cx2[55]"]
cx256 <- kette[,"cx2[56]"]
cx257 <- kette[,"cx2[57]"]
cx258 <- kette[,"cx2[58]"]
cx259 <- kette[,"cx2[59]"]
cx260 <- kette[,"cx2[60]"]
cx261 <- kette[,"cx2[61]"]

cx31 <- kette[,"cx3[1]"]
cx32 <- kette[,"cx3[2]"]
cx33 <- kette[,"cx3[3]"]
cx34 <- kette[,"cx3[4]"]
cx35 <- kette[,"cx3[5]"]
cx36 <- kette[,"cx3[6]"]
cx37 <- kette[,"cx3[7]"]
cx38 <- kette[,"cx3[8]"]
cx39 <- kette[,"cx3[9]"]
cx310 <- kette[,"cx3[10]"]
cx311 <- kette[,"cx3[11]"]
cx312 <- kette[,"cx3[12]"]
cx313 <- kette[,"cx3[13]"]
cx314 <- kette[,"cx3[14]"]
cx315 <- kette[,"cx3[15]"]
cx316 <- kette[,"cx3[16]"]
cx317 <- kette[,"cx3[17]"]
cx318 <- kette[,"cx3[18]"]
cx319 <- kette[,"cx3[19]"]
cx320 <- kette[,"cx3[20]"]
cx321 <- kette[,"cx3[21]"]
cx322 <- kette[,"cx3[22]"]
cx323 <- kette[,"cx3[23]"]
cx324 <- kette[,"cx3[24]"]
cx325 <- kette[,"cx3[25]"]
cx326 <- kette[,"cx3[26]"]
cx327 <- kette[,"cx3[27]"]
cx328 <- kette[,"cx3[28]"]
cx329 <- kette[,"cx3[29]"]
cx330 <- kette[,"cx3[30]"]
cx331 <- kette[,"cx3[31]"]
cx332 <- kette[,"cx3[32]"]
cx333 <- kette[,"cx3[33]"]
cx334 <- kette[,"cx3[34]"]
cx335 <- kette[,"cx3[35]"]
cx336 <- kette[,"cx3[36]"]
cx337 <- kette[,"cx3[37]"]
cx338 <- kette[,"cx3[38]"]
cx339 <- kette[,"cx3[39]"]
cx340 <- kette[,"cx3[40]"]
cx341 <- kette[,"cx3[41]"]
cx342 <- kette[,"cx3[42]"]
cx343 <- kette[,"cx3[43]"]
cx344 <- kette[,"cx3[44]"]
cx345 <- kette[,"cx3[45]"]
cx346 <- kette[,"cx3[46]"]
cx347 <- kette[,"cx3[47]"]
cx348 <- kette[,"cx3[48]"]
cx349 <- kette[,"cx3[49]"]
cx350 <- kette[,"cx3[50]"]
cx351 <- kette[,"cx3[51]"]
cx352 <- kette[,"cx3[52]"]
cx353 <- kette[,"cx3[53]"]
cx354 <- kette[,"cx3[54]"]
cx355 <- kette[,"cx3[55]"]
cx356 <- kette[,"cx3[56]"]
cx357 <- kette[,"cx3[57]"]
cx358 <- kette[,"cx3[58]"]
cx359 <- kette[,"cx3[59]"]
cx360 <- kette[,"cx3[60]"]
cx361 <- kette[,"cx3[61]"]

cx41 <- kette[,"cx4[1]"]
cx42 <- kette[,"cx4[2]"]
cx43 <- kette[,"cx4[3]"]
cx44 <- kette[,"cx4[4]"]
cx45 <- kette[,"cx4[5]"]
cx46 <- kette[,"cx4[6]"]
cx47 <- kette[,"cx4[7]"]
cx48 <- kette[,"cx4[8]"]
cx49 <- kette[,"cx4[9]"]
cx410 <- kette[,"cx4[10]"]
cx411 <- kette[,"cx4[11]"]
cx412 <- kette[,"cx4[12]"]
cx413 <- kette[,"cx4[13]"]
cx414 <- kette[,"cx4[14]"]
cx415 <- kette[,"cx4[15]"]
cx416 <- kette[,"cx4[16]"]
cx417 <- kette[,"cx4[17]"]
cx418 <- kette[,"cx4[18]"]
cx419 <- kette[,"cx4[19]"]
cx420 <- kette[,"cx4[20]"]
cx421 <- kette[,"cx4[21]"]
cx422 <- kette[,"cx4[22]"]
cx423 <- kette[,"cx4[23]"]
cx424 <- kette[,"cx4[24]"]
cx425 <- kette[,"cx4[25]"]
cx426 <- kette[,"cx4[26]"]
cx427 <- kette[,"cx4[27]"]
cx428 <- kette[,"cx4[28]"]
cx429 <- kette[,"cx4[29]"]
cx430 <- kette[,"cx4[30]"]
cx431 <- kette[,"cx4[31]"]
cx432 <- kette[,"cx4[32]"]
cx433 <- kette[,"cx4[33]"]
cx434 <- kette[,"cx4[34]"]
cx435 <- kette[,"cx4[35]"]
cx436 <- kette[,"cx4[36]"]
cx437 <- kette[,"cx4[37]"]
cx438 <- kette[,"cx4[38]"]
cx439 <- kette[,"cx4[39]"]
cx440 <- kette[,"cx4[40]"]
cx441 <- kette[,"cx4[41]"]
cx442 <- kette[,"cx4[42]"]
cx443 <- kette[,"cx4[43]"]
cx444 <- kette[,"cx4[44]"]
cx445 <- kette[,"cx4[45]"]
cx446 <- kette[,"cx4[46]"]
cx447 <- kette[,"cx4[47]"]
cx448 <- kette[,"cx4[48]"]
cx449 <- kette[,"cx4[49]"]
cx450 <- kette[,"cx4[50]"]
cx451 <- kette[,"cx4[51]"]
cx452 <- kette[,"cx4[52]"]
cx453 <- kette[,"cx4[53]"]
cx454 <- kette[,"cx4[54]"]
cx455 <- kette[,"cx4[55]"]
cx456 <- kette[,"cx4[56]"]
cx457 <- kette[,"cx4[57]"]
cx458 <- kette[,"cx4[58]"]
cx459 <- kette[,"cx4[59]"]
cx460 <- kette[,"cx4[60]"]
cx461 <- kette[,"cx4[61]"]

mcx11 <- mean(cx11)
mcx12 <- mean(cx12)
mcx13 <- mean(cx13)
mcx14 <- mean(cx14)
mcx15 <- mean(cx15)
mcx16 <- mean(cx16)
mcx17 <- mean(cx17)
mcx18 <- mean(cx18)
mcx19 <- mean(cx19)
mcx110 <- mean(cx110)
mcx111 <- mean(cx111)
mcx112 <- mean(cx112)
mcx113 <- mean(cx113)
mcx114 <- mean(cx114)
mcx115 <- mean(cx115)
mcx116 <- mean(cx116)
mcx117 <- mean(cx117)
mcx118 <- mean(cx118)
mcx119 <- mean(cx119)
mcx120 <- mean(cx120)
mcx121 <- mean(cx121)
mcx122 <- mean(cx122)
mcx123 <- mean(cx123)
mcx124 <- mean(cx124)
mcx125 <- mean(cx125)
mcx126 <- mean(cx126)
mcx127 <- mean(cx127)
mcx128 <- mean(cx128)
mcx129 <- mean(cx129)
mcx130 <- mean(cx130)
mcx131 <- mean(cx131)
mcx132 <- mean(cx132)
mcx133 <- mean(cx133)
mcx134 <- mean(cx134)
mcx135 <- mean(cx135)
mcx136 <- mean(cx136)
mcx137 <- mean(cx137)
mcx138 <- mean(cx138)
mcx139 <- mean(cx139)
mcx140 <- mean(cx140)
mcx141 <- mean(cx141)
mcx142 <- mean(cx142)
mcx143 <- mean(cx143)
mcx144 <- mean(cx144)
mcx145 <- mean(cx145)
mcx146 <- mean(cx146)
mcx147 <- mean(cx147)
mcx148 <- mean(cx148)
mcx149 <- mean(cx149)
mcx150 <- mean(cx150)
mcx151 <- mean(cx151)
mcx152 <- mean(cx152)
mcx153 <- mean(cx153)
mcx154 <- mean(cx154)
mcx155 <- mean(cx155)
mcx156 <- mean(cx156)
mcx157 <- mean(cx157)
mcx158 <- mean(cx158)
mcx159 <- mean(cx159)
mcx160 <- mean(cx160)
mcx161 <- mean(cx161)

mcx21 <- mean(cx21)
mcx22 <- mean(cx22)
mcx23 <- mean(cx23)
mcx24 <- mean(cx24)
mcx25 <- mean(cx25)
mcx26 <- mean(cx26)
mcx27 <- mean(cx27)
mcx28 <- mean(cx28)
mcx29 <- mean(cx29)
mcx210 <- mean(cx210)
mcx211 <- mean(cx211)
mcx212 <- mean(cx212)
mcx213 <- mean(cx213)
mcx214 <- mean(cx214)
mcx215 <- mean(cx215)
mcx216 <- mean(cx216)
mcx217 <- mean(cx217)
mcx218 <- mean(cx218)
mcx219 <- mean(cx219)
mcx220 <- mean(cx220)
mcx221 <- mean(cx221)
mcx222 <- mean(cx222)
mcx223 <- mean(cx223)
mcx224 <- mean(cx224)
mcx225 <- mean(cx225)
mcx226 <- mean(cx226)
mcx227 <- mean(cx227)
mcx228 <- mean(cx228)
mcx229 <- mean(cx229)
mcx230 <- mean(cx230)
mcx231 <- mean(cx231)
mcx232 <- mean(cx232)
mcx233 <- mean(cx233)
mcx234 <- mean(cx234)
mcx235 <- mean(cx235)
mcx236 <- mean(cx236)
mcx237 <- mean(cx237)
mcx238 <- mean(cx238)
mcx239 <- mean(cx239)
mcx240 <- mean(cx240)
mcx241 <- mean(cx241)
mcx242 <- mean(cx242)
mcx243 <- mean(cx243)
mcx244 <- mean(cx244)
mcx245 <- mean(cx245)
mcx246 <- mean(cx246)
mcx247 <- mean(cx247)
mcx248 <- mean(cx248)
mcx249 <- mean(cx249)
mcx250 <- mean(cx250)
mcx251 <- mean(cx251)
mcx252 <- mean(cx252)
mcx253 <- mean(cx253)
mcx254 <- mean(cx254)
mcx255 <- mean(cx255)
mcx256 <- mean(cx256)
mcx257 <- mean(cx257)
mcx258 <- mean(cx258)
mcx259 <- mean(cx259)
mcx260 <- mean(cx260)
mcx261 <- mean(cx261)

mcx31 <- mean(cx31)
mcx32 <- mean(cx32)
mcx33 <- mean(cx33)
mcx34 <- mean(cx34)
mcx35 <- mean(cx35)
mcx36 <- mean(cx36)
mcx37 <- mean(cx37)
mcx38 <- mean(cx38)
mcx39 <- mean(cx39)
mcx310 <- mean(cx310)
mcx311 <- mean(cx311)
mcx312 <- mean(cx312)
mcx313 <- mean(cx313)
mcx314 <- mean(cx314)
mcx315 <- mean(cx315)
mcx316 <- mean(cx316)
mcx317 <- mean(cx317)
mcx318 <- mean(cx318)
mcx319 <- mean(cx319)
mcx320 <- mean(cx320)
mcx321 <- mean(cx321)
mcx322 <- mean(cx322)
mcx323 <- mean(cx323)
mcx324 <- mean(cx324)
mcx325 <- mean(cx325)
mcx326 <- mean(cx326)
mcx327 <- mean(cx327)
mcx328 <- mean(cx328)
mcx329 <- mean(cx329)
mcx330 <- mean(cx330)
mcx331 <- mean(cx331)
mcx332 <- mean(cx332)
mcx333 <- mean(cx333)
mcx334 <- mean(cx334)
mcx335 <- mean(cx335)
mcx336 <- mean(cx336)
mcx337 <- mean(cx337)
mcx338 <- mean(cx338)
mcx339 <- mean(cx339)
mcx340 <- mean(cx340)
mcx341 <- mean(cx341)
mcx342 <- mean(cx342)
mcx343 <- mean(cx343)
mcx344 <- mean(cx344)
mcx345 <- mean(cx345)
mcx346 <- mean(cx346)
mcx347 <- mean(cx347)
mcx348 <- mean(cx348)
mcx349 <- mean(cx349)
mcx350 <- mean(cx350)
mcx351 <- mean(cx351)
mcx352 <- mean(cx352)
mcx353 <- mean(cx353)
mcx354 <- mean(cx354)
mcx355 <- mean(cx355)
mcx356 <- mean(cx356)
mcx357 <- mean(cx357)
mcx358 <- mean(cx358)
mcx359 <- mean(cx359)
mcx360 <- mean(cx360)
mcx361 <- mean(cx361)

mcx41 <- mean(cx41)
mcx42 <- mean(cx42)
mcx43 <- mean(cx43)
mcx44 <- mean(cx44)
mcx45 <- mean(cx45)
mcx46 <- mean(cx46)
mcx47 <- mean(cx47)
mcx48 <- mean(cx48)
mcx49 <- mean(cx49)
mcx410 <- mean(cx410)
mcx411 <- mean(cx411)
mcx412 <- mean(cx412)
mcx413 <- mean(cx413)
mcx414 <- mean(cx414)
mcx415 <- mean(cx415)
mcx416 <- mean(cx416)
mcx417 <- mean(cx417)
mcx418 <- mean(cx418)
mcx419 <- mean(cx419)
mcx420 <- mean(cx420)
mcx421 <- mean(cx421)
mcx422 <- mean(cx422)
mcx423 <- mean(cx423)
mcx424 <- mean(cx424)
mcx425 <- mean(cx425)
mcx426 <- mean(cx426)
mcx427 <- mean(cx427)
mcx428 <- mean(cx428)
mcx429 <- mean(cx429)
mcx430 <- mean(cx430)
mcx431 <- mean(cx431)
mcx432 <- mean(cx432)
mcx433 <- mean(cx433)
mcx434 <- mean(cx434)
mcx435 <- mean(cx435)
mcx436 <- mean(cx436)
mcx437 <- mean(cx437)
mcx438 <- mean(cx438)
mcx439 <- mean(cx439)
mcx440 <- mean(cx440)
mcx441 <- mean(cx441)
mcx442 <- mean(cx442)
mcx443 <- mean(cx443)
mcx444 <- mean(cx444)
mcx445 <- mean(cx445)
mcx446 <- mean(cx446)
mcx447 <- mean(cx447)
mcx448 <- mean(cx448)
mcx449 <- mean(cx449)
mcx450 <- mean(cx450)
mcx451 <- mean(cx451)
mcx452 <- mean(cx452)
mcx453 <- mean(cx453)
mcx454 <- mean(cx454)
mcx455 <- mean(cx455)
mcx456 <- mean(cx456)
mcx457 <- mean(cx457)
mcx458 <- mean(cx458)
mcx459 <- mean(cx459)
mcx460 <- mean(cx460)
mcx461 <- mean(cx461)

scx11 <- sd(cx11)
scx12 <- sd(cx12)
scx13 <- sd(cx13)
scx14 <- sd(cx14)
scx15 <- sd(cx15)
scx16 <- sd(cx16)
scx17 <- sd(cx17)
scx18 <- sd(cx18)
scx19 <- sd(cx19)
scx110 <- sd(cx110)
scx111 <- sd(cx111)
scx112 <- sd(cx112)
scx113 <- sd(cx113)
scx114 <- sd(cx114)
scx115 <- sd(cx115)
scx116 <- sd(cx116)
scx117 <- sd(cx117)
scx118 <- sd(cx118)
scx119 <- sd(cx119)
scx120 <- sd(cx120)
scx121 <- sd(cx121)
scx122 <- sd(cx122)
scx123 <- sd(cx123)
scx124 <- sd(cx124)
scx125 <- sd(cx125)
scx126 <- sd(cx126)
scx127 <- sd(cx127)
scx128 <- sd(cx128)
scx129 <- sd(cx129)
scx130 <- sd(cx130)
scx131 <- sd(cx131)
scx132 <- sd(cx132)
scx133 <- sd(cx133)
scx134 <- sd(cx134)
scx135 <- sd(cx135)
scx136 <- sd(cx136)
scx137 <- sd(cx137)
scx138 <- sd(cx138)
scx139 <- sd(cx139)
scx140 <- sd(cx140)
scx141 <- sd(cx141)
scx142 <- sd(cx142)
scx143 <- sd(cx143)
scx144 <- sd(cx144)
scx145 <- sd(cx145)
scx146 <- sd(cx146)
scx147 <- sd(cx147)
scx148 <- sd(cx148)
scx149 <- sd(cx149)
scx150 <- sd(cx150)
scx151 <- sd(cx151)
scx152 <- sd(cx152)
scx153 <- sd(cx153)
scx154 <- sd(cx154)
scx155 <- sd(cx155)
scx156 <- sd(cx156)
scx157 <- sd(cx157)
scx158 <- sd(cx158)
scx159 <- sd(cx159)
scx160 <- sd(cx160)
scx161 <- sd(cx161)

scx21 <- sd(cx21)
scx22 <- sd(cx22)
scx23 <- sd(cx23)
scx24 <- sd(cx24)
scx25 <- sd(cx25)
scx26 <- sd(cx26)
scx27 <- sd(cx27)
scx28 <- sd(cx28)
scx29 <- sd(cx29)
scx210 <- sd(cx210)
scx211 <- sd(cx211)
scx212 <- sd(cx212)
scx213 <- sd(cx213)
scx214 <- sd(cx214)
scx215 <- sd(cx215)
scx216 <- sd(cx216)
scx217 <- sd(cx217)
scx218 <- sd(cx218)
scx219 <- sd(cx219)
scx220 <- sd(cx220)
scx221 <- sd(cx221)
scx222 <- sd(cx222)
scx223 <- sd(cx223)
scx224 <- sd(cx224)
scx225 <- sd(cx225)
scx226 <- sd(cx226)
scx227 <- sd(cx227)
scx228 <- sd(cx228)
scx229 <- sd(cx229)
scx230 <- sd(cx230)
scx231 <- sd(cx231)
scx232 <- sd(cx232)
scx233 <- sd(cx233)
scx234 <- sd(cx234)
scx235 <- sd(cx235)
scx236 <- sd(cx236)
scx237 <- sd(cx237)
scx238 <- sd(cx238)
scx239 <- sd(cx239)
scx240 <- sd(cx240)
scx241 <- sd(cx241)
scx242 <- sd(cx242)
scx243 <- sd(cx243)
scx244 <- sd(cx244)
scx245 <- sd(cx245)
scx246 <- sd(cx246)
scx247 <- sd(cx247)
scx248 <- sd(cx248)
scx249 <- sd(cx249)
scx250 <- sd(cx250)
scx251 <- sd(cx251)
scx252 <- sd(cx252)
scx253 <- sd(cx253)
scx254 <- sd(cx254)
scx255 <- sd(cx255)
scx256 <- sd(cx256)
scx257 <- sd(cx257)
scx258 <- sd(cx258)
scx259 <- sd(cx259)
scx260 <- sd(cx260)
scx261 <- sd(cx261)

scx31 <- sd(cx31)
scx32 <- sd(cx32)
scx33 <- sd(cx33)
scx34 <- sd(cx34)
scx35 <- sd(cx35)
scx36 <- sd(cx36)
scx37 <- sd(cx37)
scx38 <- sd(cx38)
scx39 <- sd(cx39)
scx310 <- sd(cx310)
scx311 <- sd(cx311)
scx312 <- sd(cx312)
scx313 <- sd(cx313)
scx314 <- sd(cx314)
scx315 <- sd(cx315)
scx316 <- sd(cx316)
scx317 <- sd(cx317)
scx318 <- sd(cx318)
scx319 <- sd(cx319)
scx320 <- sd(cx320)
scx321 <- sd(cx321)
scx322 <- sd(cx322)
scx323 <- sd(cx323)
scx324 <- sd(cx324)
scx325 <- sd(cx325)
scx326 <- sd(cx326)
scx327 <- sd(cx327)
scx328 <- sd(cx328)
scx329 <- sd(cx329)
scx330 <- sd(cx330)
scx331 <- sd(cx331)
scx332 <- sd(cx332)
scx333 <- sd(cx333)
scx334 <- sd(cx334)
scx335 <- sd(cx335)
scx336 <- sd(cx336)
scx337 <- sd(cx337)
scx338 <- sd(cx338)
scx339 <- sd(cx339)
scx340 <- sd(cx340)
scx341 <- sd(cx341)
scx342 <- sd(cx342)
scx343 <- sd(cx343)
scx344 <- sd(cx344)
scx345 <- sd(cx345)
scx346 <- sd(cx346)
scx347 <- sd(cx347)
scx348 <- sd(cx348)
scx349 <- sd(cx349)
scx350 <- sd(cx350)
scx351 <- sd(cx351)
scx352 <- sd(cx352)
scx353 <- sd(cx353)
scx354 <- sd(cx354)
scx355 <- sd(cx355)
scx356 <- sd(cx356)
scx357 <- sd(cx357)
scx358 <- sd(cx358)
scx359 <- sd(cx359)
scx360 <- sd(cx360)
scx361 <- sd(cx361)

scx41 <- sd(cx41)
scx42 <- sd(cx42)
scx43 <- sd(cx43)
scx44 <- sd(cx44)
scx45 <- sd(cx45)
scx46 <- sd(cx46)
scx47 <- sd(cx47)
scx48 <- sd(cx48)
scx49 <- sd(cx49)
scx410 <- sd(cx410)
scx411 <- sd(cx411)
scx412 <- sd(cx412)
scx413 <- sd(cx413)
scx414 <- sd(cx414)
scx415 <- sd(cx415)
scx416 <- sd(cx416)
scx417 <- sd(cx417)
scx418 <- sd(cx418)
scx419 <- sd(cx419)
scx420 <- sd(cx420)
scx421 <- sd(cx421)
scx422 <- sd(cx422)
scx423 <- sd(cx423)
scx424 <- sd(cx424)
scx425 <- sd(cx425)
scx426 <- sd(cx426)
scx427 <- sd(cx427)
scx428 <- sd(cx428)
scx429 <- sd(cx429)
scx430 <- sd(cx430)
scx431 <- sd(cx431)
scx432 <- sd(cx432)
scx433 <- sd(cx433)
scx434 <- sd(cx434)
scx435 <- sd(cx435)
scx436 <- sd(cx436)
scx437 <- sd(cx437)
scx438 <- sd(cx438)
scx439 <- sd(cx439)
scx440 <- sd(cx440)
scx441 <- sd(cx441)
scx442 <- sd(cx442)
scx443 <- sd(cx443)
scx444 <- sd(cx444)
scx445 <- sd(cx445)
scx446 <- sd(cx446)
scx447 <- sd(cx447)
scx448 <- sd(cx448)
scx449 <- sd(cx449)
scx450 <- sd(cx450)
scx451 <- sd(cx451)
scx452 <- sd(cx452)
scx453 <- sd(cx453)
scx454 <- sd(cx454)
scx455 <- sd(cx455)
scx456 <- sd(cx456)
scx457 <- sd(cx457)
scx458 <- sd(cx458)
scx459 <- sd(cx459)
scx460 <- sd(cx460)
scx461 <- sd(cx461)

mcx1 <- c(mcx11, mcx12, mcx13, mcx14, mcx15, mcx16, mcx17, mcx18, mcx19, mcx110, mcx111, mcx112, mcx113, mcx114, mcx115, mcx116, mcx117, mcx118, mcx119, mcx120, mcx121, mcx122, mcx123, mcx124, mcx125, mcx126, mcx127, mcx128, mcx129, mcx130, mcx131, mcx132, mcx133, mcx134, mcx135, mcx136, mcx137, mcx138, mcx139, mcx140, mcx141, mcx142, mcx143, mcx144, mcx145, mcx146, mcx147, mcx148, mcx149, mcx150, mcx151, mcx152, mcx153, mcx154, mcx155, mcx156, mcx157, mcx158, mcx159, mcx160, mcx161)
mcx2 <- c(mcx21, mcx22, mcx23, mcx24, mcx25, mcx26, mcx27, mcx28, mcx29, mcx210, mcx211, mcx212, mcx213, mcx214, mcx215, mcx216, mcx217, mcx218, mcx219, mcx220, mcx221, mcx222, mcx223, mcx224, mcx225, mcx226, mcx227, mcx228, mcx229, mcx230, mcx231, mcx232, mcx233, mcx234, mcx235, mcx236, mcx237, mcx238, mcx239, mcx240, mcx241, mcx242, mcx243, mcx244, mcx245, mcx246, mcx247, mcx248, mcx249, mcx250, mcx251, mcx252, mcx253, mcx254, mcx255, mcx256, mcx257, mcx258, mcx259, mcx260, mcx261)
mcx3 <- c(mcx31, mcx32, mcx33, mcx34, mcx35, mcx36, mcx37, mcx38, mcx39, mcx310, mcx311, mcx312, mcx313, mcx314, mcx315, mcx316, mcx317, mcx318, mcx319, mcx320, mcx321, mcx322, mcx323, mcx324, mcx325, mcx326, mcx327, mcx328, mcx329, mcx330, mcx331, mcx332, mcx333, mcx334, mcx335, mcx336, mcx337, mcx338, mcx339, mcx340, mcx341, mcx342, mcx343, mcx344, mcx345, mcx346, mcx347, mcx348, mcx349, mcx350, mcx351, mcx352, mcx353, mcx354, mcx355, mcx356, mcx357, mcx358, mcx359, mcx360, mcx361)
mcx4 <- c(mcx41, mcx42, mcx43, mcx44, mcx45, mcx46, mcx47, mcx48, mcx49, mcx410, mcx411, mcx412, mcx413, mcx414, mcx415, mcx416, mcx417, mcx418, mcx419, mcx420, mcx421, mcx422, mcx423, mcx424, mcx425, mcx426, mcx427, mcx428, mcx429, mcx430, mcx431, mcx432, mcx433, mcx434, mcx435, mcx436, mcx437, mcx438, mcx439, mcx440, mcx441, mcx442, mcx443, mcx444, mcx445, mcx446, mcx447, mcx448, mcx449, mcx450, mcx451, mcx452, mcx453, mcx454, mcx455, mcx456, mcx457, mcx458, mcx459, mcx460, mcx461)

scx1 <- c(scx11, scx12, scx13, scx14, scx15, scx16, scx17, scx18, scx19, scx110, scx111, scx112, scx113, scx114, scx115, scx116, scx117, scx118, scx119, scx120, scx121, scx122, scx123, scx124, scx125, scx126, scx127, scx128, scx129, scx130, scx131, scx132, scx133, scx134, scx135, scx136, scx137, scx138, scx139, scx140, scx141, scx142, scx143, scx144, scx145, scx146, scx147, scx148, scx149, scx150, scx151, scx152, scx153, scx154, scx155, scx156, scx157, scx158, scx159, scx160, scx161)
scx2 <- c(scx21, scx22, scx23, scx24, scx25, scx26, scx27, scx28, scx29, scx210, scx211, scx212, scx213, scx214, scx215, scx216, scx217, scx218, scx219, scx220, scx221, scx222, scx223, scx224, scx225, scx226, scx227, scx228, scx229, scx230, scx231, scx232, scx233, scx234, scx235, scx236, scx237, scx238, scx239, scx240, scx241, scx242, scx243, scx244, scx245, scx246, scx247, scx248, scx249, scx250, scx251, scx252, scx253, scx254, scx255, scx256, scx257, scx258, scx259, scx260, scx261)
scx3 <- c(scx31, scx32, scx33, scx34, scx35, scx36, scx37, scx38, scx39, scx310, scx311, scx312, scx313, scx314, scx315, scx316, scx317, scx318, scx319, scx320, scx321, scx322, scx323, scx324, scx325, scx326, scx327, scx328, scx329, scx330, scx331, scx332, scx333, scx334, scx335, scx336, scx337, scx338, scx339, scx340, scx341, scx342, scx343, scx344, scx345, scx346, scx347, scx348, scx349, scx350, scx351, scx352, scx353, scx354, scx355, scx356, scx357, scx358, scx359, scx360, scx361)
scx4 <- c(scx41, scx42, scx43, scx44, scx45, scx46, scx47, scx48, scx49, scx410, scx411, scx412, scx413, scx414, scx415, scx416, scx417, scx418, scx419, scx420, scx421, scx422, scx423, scx424, scx425, scx426, scx427, scx428, scx429, scx430, scx431, scx432, scx433, scx434, scx435, scx436, scx437, scx438, scx439, scx440, scx441, scx442, scx443, scx444, scx445, scx446, scx447, scx448, scx449, scx450, scx451, scx452, scx453, scx454, scx455, scx456, scx457, scx458, scx459, scx460, scx461)

clabels <- c("ARG", "AUS", "AUT", "BHS", "BRB", "BEL", "BEN", "BWA", "BGR", "CAN", "CPV", "CHL", "CRI", "HRV" ,"CZE", "DNK", "DOM", "SLV", "EST", "FIN", "FRA", "DEU", "GHA", "GRC", "HUN", "ISL", "IND", "IRL", "ISR", "ITA", "JAM", "JPN", "KOR", "LVA", "LTU", "LUX", "MLI", "MLT", "MUS", "MEX", "MNG", "NAM", "NLD", "NZL", "NOR", "PAN", "POL", "PRT", "ROU", "SVK","SVN", "ZAF", "ESP", "SUR", "SWE", "CHE", "TWN", "TTO", "GBR", "USA", "URY")

var.names <- c(clabels)

# Exporting mean scores 

DPDc2 <-aggregate(DPD, by=list(DPD$cname,DPD$ccodealp,DPD$id),FUN=mean, na.rm=TRUE)
country <- as.character(DPDc2$Group.1)
acronym <- as.character(DPDc2$Group.2)
id <- DPDc2$Group.3

prop_diff_mean <- round(mcx1, 2)
prop_diff_sd <- round(scx1, 2)
dec_diff_mean <- round(mcx2, 2)
dec_diff_sd <- round(scx2, 2)
dir_diff_mean <- round(mcx3, 2)
dir_diff_sd <- round(scx3, 2)
pres_diff_mean <- round(mcx4, 2)
pres_diff_sd <- round(scx4, 2)

DPD18_scores <- data.frame(country,acronym,id,prop_diff_mean,prop_diff_sd,dec_diff_mean,dec_diff_sd,dir_diff_mean,dir_diff_sd,pres_diff_mean,pres_diff_sd)
  
write.csv(DPD18_scores,file="DPD2018July_scores.csv")

write.dta(DPD18_scores, "DPD2018July_scores.dta") 

save(DPD18_scores,file="DPD2018July_scores.Rdata")

# Election-specific scores could be extracted as well... 


#Figure 3.5: Proportional and decentral power diffusion  
pic <- data.frame(var.names,mcx1,mcx2,mcx3,mcx4,scx1,scx2,scx3,scx4)
pic.sort <- pic[order(-mcx1) , ]
pic.sort

postscript("fig_3_5.eps", width = 800, height = 800, horizontal=FALSE) 

y.axis <- length(var.names):1 

layout(matrix(c(1,2),1,2, byrow = TRUE), widths=c(1,1))

plot(pic.sort$mcx1, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-4,4), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "Proportional")
axis(1,at = seq(-4,4, by = 1), label = seq(-4,4, by = 1), cex.axis=.9)
axis(2, at = y.axis, label = pic.sort$var.names, las = 1, tick = T, font=1, cex.axis=.8)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(pic.sort$mcx1-qnorm(.975)*pic.sort$scx1, y.axis, pic.sort$mcx1+qnorm(.975)*pic.sort$scx1, y.axis, lwd =  1.5)

plot(pic.sort$mcx2, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-4,4), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "Decentral")
axis(1,at = seq(-4,4, by = 1), label = seq(-4,4, by = 1), cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(pic.sort$mcx2-qnorm(.975)*pic.sort$scx2, y.axis, pic.sort$mcx2+qnorm(.975)*pic.sort$scx2, y.axis, lwd =  1.5)

dev.off()

# Figure 3.6: Presidential and direct power diffusion 
pic <- data.frame(var.names,mcx1,mcx2,mcx3,mcx4,scx1,scx2,scx3,scx4)
pic.sort <- pic[order(-mcx3) , ]
pic.sort

postscript("fig_3_6.eps", width = 800, height = 800, horizontal=FALSE) 

y.axis <- length(var.names):1 

layout(matrix(c(1,2),1,2, byrow = TRUE), widths=c(1,1))

plot(pic.sort$mcx3, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-4,4), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "Presidential")
axis(1,at = seq(-4,4, by = 1), label = seq(-4,4, by = 1), cex.axis=.9)
axis(2, at = y.axis, label = pic.sort$var.names, las = 1, tick = T, font=1, cex.axis=.8)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(pic.sort$mcx3-qnorm(.975)*pic.sort$scx3, y.axis, pic.sort$mcx3+qnorm(.975)*pic.sort$scx3, y.axis, lwd =  1.5)

plot(pic.sort$mcx4, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-2,6), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "Direct")
axis(1,at = seq(-2,6, by = 1), label = seq(-2,6, by = 1), cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(pic.sort$mcx4-qnorm(.975)*pic.sort$scx4, y.axis, pic.sort$mcx4+qnorm(.975)*pic.sort$scx4, y.axis, lwd =  1.5)

dev.off()

#Fig. 3.7: Maps of power diffusion  
ccodealp <- c("ARG", "AUS", "AUT", "BHS", "BRB", "BEL", "BEN", "BWA", "BGR", "CAN", "CPV", "CHL", "CRI", "HRV" ,"CZE", "DNK", "DOM", "SLV", "EST", "FIN", "FRA", "DEU", "GHA", "GRC", "HUN", "ISL", "IND", "IRL", "ISR", "ITA", "JAM", "JPN", "KOR", "LVA", "LTU", "LUX", "MLI", "MLT", "MUS", "MEX", "MNG", "NAM", "NLD", "NZL", "NOR", "PAN", "POL", "PRT", "ROU", "SVK","SVN", "ZAF", "ESP", "SUR", "SWE", "CHE", "TWN", "TTO", "GBR", "USA", "URY")

scd <- data.frame(mcx1,mcx2,mcx3,mcx4,ccodealp)

postscript("fig_3_7.eps", width = 800, height = 800, horizontal=FALSE) 

par(mfrow=c(3,2))
plot(scd$mcx1,scd$mcx2, main="Proportional and Decentral", xlab="Proportional", ylab="Decentral", type="n", xlim=c(-2,2.5), ylim=c(-1.5,2.5))
text(scd$mcx1,scd$mcx2,scd$ccodealp,cex=.8)
plot(jitter(scd$mcx1,factor=1000),scd$mcx3, main="Proportional and Presidential", xlab="Proportional", ylab="Presidential", type="n", xlim=c(-2,2.5), ylim=c(-1,1.5))
text(jitter(scd$mcx1,factor=1000),scd$mcx3,scd$ccodealp,cex=.8)
plot(scd$mcx1,scd$mcx4, main="Proportional and Direct", xlab="Proportional", ylab="Direct", type="n", xlim=c(-2,2.5), ylim=c(-1.5,3.5))
text(scd$mcx1,scd$mcx4,scd$ccodealp,cex=.8)
plot(jitter(scd$mcx2,factor=1000),scd$mcx3, main="Decentral and Presidential", xlab="Decentral", ylab="Presidential", type="n", xlim=c(-2,2.5), ylim=c(-1,1.5))
text(jitter(scd$mcx2,factor=1000),scd$mcx3,scd$ccodealp,cex=.8)
plot(scd$mcx2,scd$mcx4, main="Decentral and and Direct", xlab="Decentral", ylab="Direct", type="n", xlim=c(-2,2.5), ylim=c(-1.5,3.5))
text(scd$mcx2,scd$mcx4,scd$ccodealp,cex=.8)
plot(scd$mcx3,jitter(scd$mcx4,factor=1000), main="Presidential and Direct", xlab="Presidential", ylab="Direct", type="n", xlim=c(-1,1.5), ylim=c(-1.5,3.5))
text(scd$mcx3,jitter(scd$mcx4,factor=1000),scd$ccodealp,cex=.8)

dev.off()

#Fig. 3.8: Comparison with consensus democracy and centripetalism 
exepar8110 <- DPDc$exepar8110
feduni8110 <- DPDc$feduni8110
gtm_centrip <- DPDc$gtm_centrip
scatterdata3 <- data.frame(mcx1, mcx2, exepar8110, feduni8110, gtm_centrip, ccodealp)
scatterdata3 <- scatterdata3[complete.cases(scatterdata3), ]

postscript("fig_3_8.eps", width = 800, height = 800, horizontal=FALSE) 

par(mfrow=c(2,2))
plot(scatterdata3$mcx1, scatterdata3$exepar8110, xlab="Proportional Power Diffusion", ylab="Executives-Parties", type="n",xlim=c(-2,2.5), ylim=c(-2,2))
text(scatterdata3$mcx1, scatterdata3$exepar8110, scatterdata3$ccodealp, cex=.8)
abline(rega <- lm(scatterdata3$exepar8110~scatterdata3$mcx1))

plot(scatterdata3$mcx2, scatterdata3$feduni8110, xlab="Decentral Power Diffusion", ylab="Federal-Unitary", type="n",xlim=c(-1.5,2.5), ylim=c(-2,2.5))
text(scatterdata3$mcx2, scatterdata3$feduni8110, scatterdata3$ccodealp, cex=.8)
abline(regb <- lm(scatterdata3$feduni8110~scatterdata3$mcx2))

plot(scatterdata3$mcx1, scatterdata3$gtm_centrip, xlab="Proportional Power Diffusion", ylab="Centripetalism", type="n",xlim=c(-2,2.5), ylim=c(0,6))
text(scatterdata3$mcx1, scatterdata3$gtm_centrip, scatterdata3$ccodealp, cex=.8)
abline(regc <- lm(scatterdata3$gtm_centrip~scatterdata3$mcx1))

plot(scatterdata3$exepar8110, scatterdata3$gtm_centrip, xlab="Executives-Parties", ylab="Centripetalism", type="n",xlim=c(-2,2), ylim=c(0,6))
text(scatterdata3$exepar8110, scatterdata3$gtm_centrip, scatterdata3$ccodealp, cex=.8)
abline(regd <- lm(scatterdata3$gtm_centrip~scatterdata3$exepar8110))

dev.off()

cor(scatterdata3$mcx1,scatterdata3$exepar8110) 
cor(scatterdata3$mcx1,scatterdata3$gtm_centrip) 


#Descriptives
 
elecc <- DPDc$elec
parc <- DPDc$par
cabc <- DPDc$cab
decc <- DPDc$dec
exelecc <- DPDc$exelec
ppic <- DPDc$ppi
descrdata <- data.frame(elecc,parc,cabc,dirc,fedc,decc,ccodealp,exelegc,ppic)

#Fig. 3.1 sample world map -> see code for Chapter 6 (explaining power diffusion) 

#Fig. 3.2: elec and par  
postscript("fig_3_2.eps", width = 800, height = 800, horizontal=FALSE) 
plot(descrdata$elecc, descrdata$parc, xlab="Gallagher Index", ylab="Effective Number of Parties", type="n", xlim=c(0,20), ylim=c(1,9))
text(descrdata$elecc, descrdata$parc, descrdata$ccodealp, cex=.8)
abline(reg <- lm(descrdata$parc~descrdata$elecc))
dev.off()
cor.test(elecc,parc) 

cor.test(fedc,decc,use="complete.obs")

#Fig. 3.3: dir and cab 
postscript("fig_3_3.eps", width = 800, height = 800, horizontal=FALSE) 
plot(descrdata$dirc, descrdata$cabc, xlab="Direct Democracy", ylab="Cabinet Type", type="n", xlim=c(-3,14), ylim=c(1,4))
text(descrdata$dirc, descrdata$cabc, descrdata$ccodealp, cex=.8)
abline(reg <- lm(descrdata$cabc~descrdata$dirc))
dev.off()
cor.test(dirc,cabc)

