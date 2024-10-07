# code from Frank, Tenenbaum, & Gibson (2013), PLOS
# code was written probably circa 2011
# updated and posted 2024

library(psyphy)
library(here)
library(lme4)

data <- read.csv(here("data","massive seg 3year followup.csv"))
data$log.freq <- log10(data$freq)

data$freq.bin <- NA
bins <- c(0,100,200,500,1000,2000,1000)
for (i in 1:(length(bins)-1)) {
	data$freq.bin[data$freq > bins[i] & data$freq < bins[i+1]] <- bins[i+1]
}
	

#data$freq.bin <- 10^(round(data$log.freq*3)/3)

# prune error trials
data <- data[as.character(data$ans.word) != as.character(data$dist.word),]

sub.data <- aggregate(data$correct,list(data$sub,data$freq.bin),sum)
sub.data2 <- aggregate(data$correct,list(data$sub,data$freq.bin),length)
names(sub.data) <- c("sub","tokens","corr")
sub.data$incorr <- sub.data2$x - sub.data$corr
sub.data$avg.corr <- sub.data$corr / (sub.data$corr + sub.data$incorr)

sub.data <- sub.data[sub.data$sub!="MCF",]
sub.data$sub <- factor(sub.data$sub)

# pdf("~/Projects/segmentation/writeup/massive seg journal paper/mseg tex/figures/mseg2afc.pdf",
# 	width=6,height=5)
cols <- c("red","green","blue","orange")

plot(NA,NA,
	pch=20,bty="n",xlim=c(100,2000),log="x",ylim=c(0,100),
	xlab="Token Frequency",ylab="Percent Correct")

subs <- unique(sub.data$sub)
sub.codes <- c("DF","MB","LB")
off <- c(-2,2,0)
for (i in 1:length(subs)) {
	this.data <- sub.data[sub.data$sub==subs[i],]
	x <- this.data$tokens
	y <- this.data$avg.corr*100
	points(x+.03*i*x,y,col=cols[i],xpd="n",pch=20)	
	ys <- matrix(NA,nrow=length(y),ncol=2)
	ys[,1] <- this.data$corr
	ys[,2] <- this.data$incorr
	tpt.glm <- glm(ys ~ log(x),family=binomial(mafc.logit(2)))
	preds <- data.frame(x)
	names(preds) <- c("tpt")
	preds$y <- predict(tpt.glm,preds,type="response")
	lines(preds$tpt,preds$y*100,col=cols[i])
	text(max(x)+600,max(y)+off[i],col=cols[i],sub.codes[i],xpd="n")
}

lines(c(2,10000),c(50,50),lty=2,col="black")


basic.lmer <- glmer(correct ~ log.freq + (1|sub),
                   data=data,
                   family="binomial")
summary(basic.lmer)
