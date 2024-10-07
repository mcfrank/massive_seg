# code from Frank, Tenenbaum, & Gibson (2013), PLOS
# code was written probably circa 2011
# updated and posted 2024

library(plotrix)

# f scores
fs <- matrix(NA,nrow=4,ncol=4)
ps <- matrix(NA,nrow=4,ncol=4)
rs <- matrix(NA,nrow=4,ncol=4)

# IIRC these are output from Matlab code from ~2008
ps[1,] <- c(0.69,0.59,0.53,0.45)
rs[1,] <- c(0.77,0.74,0.63,0.60)
fs[1,] <- c(0.71,0.64,0.56,0.50)
ps[2,] <- c(0.75,0.58,0.61,0.52)
rs[2,] <- c(0.81,0.84,0.64,0.76)
fs[2,] <- c(0.77,0.67,0.61,0.60)
ps[3,] <- c(0.37,0.39,0.34,0.32)
rs[3,] <- c(0.33,0.44,0.55,0.38)
fs[3,] <- c(0.33,0.40,0.40,0.34)
ps[4,] <- c(0.31,0.29,0.28,0.29)
rs[4,] <- c(0.35,0.38,0.33,0.38)
fs[4,] <- c(0.32,0.32,0.29,0.32)

baseline.CIs <- matrix(NA,nrow=2,ncol=4)
baseline.CIs[1,] <- c(   0.2730,    0.2743,    0.2442,   0.2734)
baseline.CIs[2,] <- c(    0.3714,    0.3733,    0.3473,    0.3691)


# make plot
cols = rep(gray(seq(.2,.9,.7/3)),4)
xs <- barplot(fs,beside=TRUE,col=cols,
	names.arg=c('KD','MB','DF','LB'),
	ylim=c(0,1),xlab="Participant",ylab="F-score")
plotCI(xs[4,],fs[4,],fs[4,] - baseline.CIs[1,],
	baseline.CIs[2,] - fs[4,],add=TRUE,pch=NA)
points(xs,ps,col="red",pch="-")
points(xs,rs,col="blue",pch="-")
legend(13,1.1,c("Immediate test","1-2 month test","Yoked control","Permuted baseline"),
	fill=cols,xpd="n",bty="n")
#legend(13,.8,c("Precision","Recall"),pch=1,col=c("red","blue"),bty="n")