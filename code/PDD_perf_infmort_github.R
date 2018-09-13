######################################################
#Replication code for "Power Diffusion and Democracy"#
######################################################
# Julian Bernauer and Adrian Vatter 

# Replication code Chapter 4: Infant mortality 

library(R2jags)

dir <- "[...]"

setwd(dir)

# Data with infant mortality and controls as described in Chapter 4 of the book 
load("PDD2018_ch4_perf_infmort.Rdata")

#Variables
infmort <- DPDinfe_comp$infmort
pop <- DPDinfe_comp$pop

#Institutions from merged data 
DPDc <-aggregate(DPDinfe_comp, by=list(DPDinfe_comp$id.x),FUN=mean, na.rm=TRUE)
exelegc <- DPDc$exeleg
exelegc[is.na(exelegc)] <- NA
fedc <- round(DPDc$fed)
bicc <- round(DPDc$bic)
constc <- round(DPDc$const)
judc <- round(DPDc$jud)
dirc <- DPDc$dir
presc <- round(DPDc$pres)
#gdp measured at the country level 
gdpc <- DPDc$gdp

DPDe <- DPDinfe_comp

elece <- DPDe$elec
elece[is.na(elece)] <- NA
pare <- DPDe$par
pare[is.na(pare)] <- NA
cabe <- round(DPDe$cab)
cabe[is.na(cabe)] <- NA

period <- DPDe$eper

idne <- as.numeric(factor(DPDe$id.x))


#Model  

model <- "model{

for(k in 1:NE){

elec[k] ~ dnorm(mu.elec[k],tau.elec)
mu.elec[k] <- alpha.elec[idne[k]] 
+ gamma.elec1*ex1[k] 

par[k] ~ dnorm(mu.par[k],tau.par)
mu.par[k] <- alpha.par[idne[k]] 
+ gamma.par1*ex1[k] 

cab[k] ~ dcat(p.cab[k,1:4])
p.cab[k,1] <- Q[k,1]
p.cab[k,2] <- Q[k,2] - Q[k,1] 
p.cab[k,3] <- Q[k,3] - Q[k,2] 
p.cab[k,4] <- 1 - Q[k,3]

for (k.cut in 1:3){
logit(Q[k,k.cut]) <-  C[idne[k],k.cut] 
- 
beta.cab1*ex1[k] 
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

beta.cab1 ~ dnorm(0, .001) 

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
beta.pres3 ~ dnorm(0, .001)  


for(k in 1:NE){
ex1[k] ~ dnorm(cx1[idne[k]],1) 
}

for(i in 1:NT){
x2[i] ~ dnorm(cx2[idn[i]],1)  
}

#CH 56, USA 59

for(k in 1:55){
cx1[k] ~ dnorm(0,1) 
cx2[k] ~ dnorm(0,1)  
cx3[k] ~ dnorm(0,1) 
cx4[k] ~ dnorm(0,1) 

}

cx1[56] ~ dnorm(0,1) I(0,)
cx1[57] ~ dnorm(0,1) 
cx1[58] ~ dnorm(0,1)
cx1[59] ~ dnorm(0,1)
cx1[60] ~ dnorm(0,1) 

cx2[56] ~ dnorm(0,1) I(0,)
cx2[57] ~ dnorm(0,1) 
cx2[58] ~ dnorm(0,1)
cx2[59] ~ dnorm(0,1)
cx2[60] ~ dnorm(0,1)

cx3[56] ~ dnorm(0,1) 
cx3[57] ~ dnorm(0,1) 
cx3[58] ~ dnorm(0,1)
cx3[59] ~ dnorm(0,1) I(0,)
cx3[60] ~ dnorm(0,1) 

cx4[56] ~ dnorm(0,1) I(0,)
cx4[57] ~ dnorm(0,1) 
cx4[58] ~ dnorm(0,1)
cx4[59] ~ dnorm(0,1)
cx4[60] ~ dnorm(0,1)

beta.dircab ~ dnorm(0, .001) 
beta.prescab ~ dnorm(0, .001) 
gamma.presexeleg  ~ dnorm(0, .001) I(,0)


#outcome
for(i in 1:NE){

infmort[i] ~ dnorm(mu.infmort[i],tau.infmort)
mu.infmort[i] <- b0[idne[i]] 
+ b.t*period[i]

}

b.t ~ dnorm(0,.001)
sigma.infmort ~ dunif(0,100) 
tau.infmort <- pow(sigma.infmort,-2) 

for(j in 1:N){
b0[j]  ~ dnorm(mub[j], taub)
mub[j] <- 	gamma00 + 
gamma.cx1*cx1[j] +
gamma.cx2*cx2[j] + gamma.cx3*cx3[j] + gamma.cx4*cx4[j]
+ gamma.cx12*cx1[j]*cx2[j] + gamma.cx13*cx1[j]*cx3[j] + gamma.gdp*gdp[j]
}

gamma00 ~ dnorm(0, .0001) 
gamma.cx1 ~ dnorm(0, .0001)
gamma.cx2 ~ dnorm(0, .0001)
gamma.cx3 ~ dnorm(0, .0001)
gamma.cx4 ~ dnorm(0, .0001)
gamma.cx12 ~ dnorm(0, .0001)
gamma.cx13 ~ dnorm(0, .0001)
gamma.gdp ~ dnorm(0, .0001)

sigmab ~ dunif(0,100) 
taub <- pow(sigmab,-2) 

}"
 
write(model, file="infmort.jags")

N <- max(id)
NE <- length(infmort)
NT <- length(dec)

infmort.data <- list(N=N, NE=NE, NT=NT, idn=id, idne=idne, par=pare, elec=elece, exeleg=exelegc, cab=cabe, fed=fedc, dec=dec, bic=bicc, const=constc, jud=judc, dir=dirc, pres=presc,infmort=log(infmort),gdp=log(gdpc)-mean(log(gdpc)),pop=log(pop),period=period)

infmort.parameters <- c("sigma.elec", "sigma.par", "sigma.exeleg", "sigma.dec", "sigma.dir", "alpha.exeleg", "alpha.dir", "gamma.elec1", "gamma.par1", "gamma.exeleg1", "gamma.dec2", "gamma.dir4", 
                        "beta.cab1", "beta.fed2", "beta.bic2", "beta.const2", "beta.jud2", "beta.pres3", "cx1", "cx2", "cx3", "cx4", "beta.prescab", "beta.dircab", "gamma.presexeleg", 
                        "a.elec1", "a.par1", "a.dec2", "a.cab1", 
                        "sigma.ac", 
                        "b0", 
                        "gamma00", "gamma.cx1", "gamma.cx2","gamma.cx3","gamma.cx4","b.t","gamma.gdp",
                        "gamma.cx12", "gamma.cx13","sigma.infmort","sigmab")

jags.avb <- jags.model(file="infmort.jags", data = infmort.data, n.chains = 3, n.adapt = 100)

sampleshelp <- coda.samples(jags.avb, infmort.parameters, n.iter=100, thin=1)
samplesburn <- coda.samples(jags.avb, infmort.parameters, n.iter=19800, thin=198)
samples <- coda.samples(jags.avb, infmort.parameters, n.iter=10000, thin=100)

#plot(sampleshelp, ask=TRUE) 
#plot(samples, ask=TRUE)


#Figure 6.8: infant mortality 
kette <- as.matrix(samples)

ainf <- kette[,"gamma00"]
mainf <- mean(ainf)
sainf <- sd(ainf)
gcx1 <- kette[,"gamma.cx1"]
mgcx1 <- mean(gcx1)
sgcx1 <- sd(gcx1)
gcx2 <- kette[,"gamma.cx2"]
mgcx2 <- mean(gcx2)
sgcx2 <- sd(gcx2)
gcx3 <- kette[,"gamma.cx3"]
mgcx3 <- mean(gcx3)
sgcx3 <- sd(gcx3)
gcx4 <- kette[,"gamma.cx4"]
mgcx4 <- mean(gcx4)
sgcx4 <- sd(gcx4)
gpropdec <- kette[,"gamma.cx12"]
mgpropdec <- mean(gpropdec)
sgpropdec <- sd(gpropdec)
gproppres <- kette[,"gamma.cx13"]
mgproppres <- mean(gproppres)
sgproppres <- sd(gproppres)
ggdp <- kette[,"gamma.gdp"]
mggdp <- mean(ggdp)
sggdp <- sd(ggdp)
gt <- kette[,"b.t"]
mgt <- mean(gt)
sgt <- sd(gt)

varinf <- kette[,"sigmab"]
mvarinf <- mean(varinf)
svarinf <- sd(varinf)

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

beta.prescab <- kette[,"beta.prescab"]
mbprc <- mean(beta.prescab)
sbprc <- sd(beta.prescab)
beta.dircab <- kette[,"beta.dircab"]
mbdc <- mean(beta.dircab)
sbdc <- sd(beta.dircab)
gamma.presexeleg <- kette[,"gamma.presexeleg"]
mgpe <- mean(gamma.presexeleg)
sgpe <- sd(gamma.presexeleg)

#y-stand.
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

mcoef <- c(mainf, mgcx1, mgcx2, mgcx3, mgcx4, mgpropdec, mgproppres, mggdp, mgt, mvarinf,  stdmael1, stdmgel1, stdmap1, stdmgp1, mac1, mbc1, stdmgex1, mbf2/10, stdmadec2, stdmgdec2, mbb2, mbj2, mbc2, mbp3/5, stdmgdir4) 
scoef <- c(sainf, sgcx1, sgcx2, sgcx3, sgcx4, sgpropdec, sgproppres, sggdp, sgt, svarinf,  stdsael1, stdsgel1, stdsap1, stdsgp1, sac1, sbc1, stdsgex1, sbf2/10, stdsadec2, stdsgdec2, sbb2, sbj2, sbc2, sbp3/5, stdsgdir4)

m1 <- c(mcoef)
s1 <- c(scoef)

var.names <- c("Mean infant mortality","Proportional pow. diff.","Decentral pow. diff.","Presidential pow. diff.","Direct diff.","Prop. x dec. diff.","Prop. x pres. diff.","GDP per capita","Time trend", "Country SD inf. mort.", "Electoral disprop.","Elect. disprop. (intra-c.)","Effective nr. of parties","Eff. nr. of part. (intra-c.)","Cabinet type","Cab. type (intra-c.)","Parliamentary power","Federalism (/10)","Decentralisation","Dec. (intra-country)","Bicameralism","Judicial review","Constitutional rigidity","Presidentialism (/5)","Direct democracy")


postscript("fig4_8.eps", width = 800, height = 800, horizontal=FALSE) 

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(m1, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 21, xlim = c(-2,4), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "", bg = "black")
axis(1,at = seq(-2,4, by = 1), label = seq(-2,4, by = 1), cex.axis=.9)
axis(2, at = y.axis, label = var.names, las = 1, tick = T, font=1, cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(m1-qnorm(.975)*s1, y.axis, m1+qnorm(.975)*s1, y.axis, lwd =  1.5)
abline(v=0, lty = 2)
segments(m1-qnorm(.95)*s1, y.axis -.1, m1-qnorm(.95)*s1, y.axis +.1, lwd = 1.5) 
segments(m1+qnorm(.95)*s1, y.axis -.1, m1+qnorm(.95)*s1, y.axis +.1, lwd = 1.5)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .5 
segments(left.side,25,left.side,16) 
segments(left.side,25,left.side+.1,25) 
segments(left.side,16,left.side+.1,16)
text(.35, 21, "Outcome Model", srt = 90, cex=1)
left.side <- .5 
segments(left.side,15,left.side,1) 
segments(left.side,15,left.side+.1,15) 
segments(left.side,1,left.side+.1,1)
text(.35, 7.5, "Measurement Model", srt = 90, cex=1)

dev.off() 

#Figure 6.9: interaction proportional and decentral for infant mortality 

postscript("fig4_9.eps", width = 800, height = 800, horizontal=FALSE) 

par(mar=c(3,4,0,1) + 0.1)    
par(oma=c(1,1,1,0)+.1)
par(xaxs = "i") 
par(mfrow=c(1,1))
curve(mgcx1 + mgpropdec*x, from=-1.5,
      to=2,ylim=c(-0.8,.4), ylab="",  xlab="", axes=F, col="white")
axis(2, at=seq(-.8, .4, by=.2))
axis(1, at=seq(-1.5, 2, by=.5))
axis(2, at = -.2, label = "Effect of Proportional Power Diffusion on Infant Mortality", las = 0, tick=F, outer=F, cex.axis=1, line=2)
axis(1, at = .25, label = "Decentral Power Diffusion", las = 0, tick=F, outer=F, cex.axis=1, line=2)
for (i in 100:300){
  curve(gcx1[i] + gpropdec[i]*x, from=-1.5, to=2,add=T,
        col="grey", lwd=1, lty=3)
  #lty=2 or 3
}
abline(h=0)
curve(mgcx1 + mgpropdec*x, from=-1.5,
      to=2,ylim=c(-.8,.4), ylab="", xlab="", col="black", lwd=2, add=T)
box()

dev.off() 

