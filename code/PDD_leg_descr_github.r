######################################################
#Replication code for "Power Diffusion and Democracy"#
######################################################
# Julian Bernauer and Adrian Vatter 

# Replication code Chapter 5: descriptives     
library(R2jags)
dir <- "[...]"
setwd(dir)

# prepared data with CSES and PDD files 
load("PDD2018_ch5_leg.Rdata")

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
win <- cses3_red$win 

aggturn <-aggregate(cses3_red$turn, by=list(cses3_red$ccodealp), FUN=mean, na.rm=TRUE)
turnc <- aggturn$x
turnc

aggdem <-aggregate(cses3_red$demsat, by=list(cses3_red$ccodealp), FUN=mean, na.rm=TRUE)
demc <- aggdem$x
demc

aggcong <-aggregate(cses3_red$cong, by=list(cses3_red$ccodealp), FUN=mean, na.rm=TRUE)
congc <- aggcong$x
congc

aggacc <-aggregate(cses3_red$acc, by=list(cses3_red$ccodealp), FUN=mean, na.rm=TRUE)
accc <- aggacc$x
accc

ccodec <- c("AUS","CAN","CHE","CZE","DEU","DNK","EST","FIN","FRA","GRC","HRV","IRL","ISL","ISR","JPN","KOR","LVA","MEX","NLD","NOR","NZL","POL","ROU","SVK","SVN","SWE","URY","USA")
ccodec 

var.names <- c(ccodec)


#congruence
m.v <- congc
pic <- data.frame(var.names,m.v)
pic.sort <- pic[order(congc) , ]
pic.sort

postscript("fig5.1.eps", width = 800, height = 800, horizontal=FALSE)

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(pic.sort$m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-2,0), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-2,0, by = .5), label = seq(-2,0, by = .5), cex.axis=.9)
axis(2, at = y.axis, label = pic.sort$var.names, las = 1, tick = T, font=1, cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(pic.sort$var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .6 
segments(left.side,28,left.side,1) 
segments(left.side,28,left.side+.1,28) 
segments(left.side,1,left.side+.1,1)
text(.5, 14.5, "Ideological party congruence (country means)", srt = 90, cex=1)

dev.off()


#accountability
m.v <- accc
pic <- data.frame(var.names,m.v)
pic.sort <- pic[order(accc) , ]
pic.sort

postscript("fig5.2.eps", width = 800, height = 800, horizontal=FALSE)

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(pic.sort$m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(1,5), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(1,5, by = 1), label = seq(1,5, by = 1), cex.axis=.9)
axis(2, at = y.axis, label = pic.sort$var.names, las = 1, tick = T, font=1, cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(pic.sort$var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .6 
segments(left.side,28,left.side,1) 
segments(left.side,28,left.side+.1,28) 
segments(left.side,1,left.side+.1,1)
text(.5, 14.5, "Accountability (country means)", srt = 90, cex=1)

dev.off()


#turnout
m.v <- turnc
pic <- data.frame(var.names,m.v)
pic.sort <- pic[order(turnc) , ]
pic.sort

postscript("fig5.3.eps", width = 800, height = 800, horizontal=FALSE)

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(pic.sort$m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(0,1), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(0,1, by = .2), label = seq(0,1, by = .2), cex.axis=.9)
axis(2, at = y.axis, label = pic.sort$var.names, las = 1, tick = T, font=1, cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(pic.sort$var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .6 
segments(left.side,28,left.side,1) 
segments(left.side,28,left.side+.1,28) 
segments(left.side,1,left.side+.1,1)
text(.5, 14.5, "Reported turnout (country means)", srt = 90, cex=1)

dev.off()


#demsat
m.v <- demc
pic <- data.frame(var.names,m.v)
pic.sort <- pic[order(demc) , ]
pic.sort

postscript("fig5.4.eps", width = 800, height = 800, horizontal=FALSE)

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(pic.sort$m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(1,4), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(1,4, by = 1), label = seq(1,4, by = 1), cex.axis=.9)
axis(2, at = y.axis, label = pic.sort$var.names, las = 1, tick = T, font=1, cex.axis=.9)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(pic.sort$var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .6 
segments(left.side,28,left.side,1) 
segments(left.side,28,left.side+.1,28) 
segments(left.side,1,left.side+.1,1)
text(.5, 14.5, "Satisfaction with democracy (country means)", srt = 90, cex=1)

dev.off()

