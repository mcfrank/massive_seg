# code from Frank, Tenenbaum, & Gibson (2013), PLOS
# code was written probably circa 2011
# updated and posted 2024

library(Hmisc)
library(binom)
library(lme4)
library(ggplot2)
library(here)

data <- read.csv(here("data","mseg_freqs.csv"))

data$freq <- apply(data[,c("f1","f2")],1,max)
data$bound <- data$trial_type == "hit" | data$trial_type == "miss"
data$correct <- data$trial_type == "hit" | data$trial_type == "cr"

# qs <- quantile(data$freq,seq(0,1,.1))
qs <- 2^(0:11)
data$binned.freq <- cut(data$freq,breaks=qs,labels=qs[2:length(qs)])


ci.l <- function(x) {binom.confint(sum(x),length(x),method="bayes")$lower}
ci.h <- function(x) {binom.confint(sum(x),length(x),method="bayes")$upper}
ms <- aggregate(correct ~ binned.freq + bound,data,mean)
ms$cil <- aggregate(correct ~ binned.freq + bound,data,ci.l)$correct
ms$cih <- aggregate(correct ~ binned.freq + bound,data,ci.h)$correct
ms$binned.freq <- as.numeric(as.character(ms$binned.freq))


l <- coef(lm(correct ~ binned.freq,data=ms))
qplot(binned.freq,correct,ymin=cil,ymax=cih,data=ms,
	geom=c("pointrange"),colour=bound,ylim=c(0,1),
	xlab="Frequency at Boundary (Binned)",ylab="Probability of Correct Decision") + 
	coord_trans(x="log") +
	scale_colour_manual(values = c("black","gray"), labels=c("No Boundary","Boundary")) +
	scale_x_continuous(breaks = as.numeric(qs)) +
	stat_smooth(method="loess",se=F,span=2) + 
	# geom_abline(intercept=l[1],slope=l[2]) + 
	theme_bw() 

# theme syntax has changed in the last 13 years, so I am dropping this. 
# +
	# theme(panel.grid.major = theme_blank(), panel.grid.minor=theme_blank(),
		 # panel.border = theme_blank(), axis.line = theme_segment(colour="black",size=.5),
		 # axis.ticks = theme_segment(size=.5)) +
	# opts(legend.justification=c(1,0), legend.position=c(1,0), 
		 # legend.title=theme_blank()) + 
	# opts(axis.title.x = theme_text(vjust=-.5),
		 # axis.title.y = theme_text(angle=90,vjust=0.25))
		 
glmer(correct ~ log(f1) * log(f2) + (log(f1)*log(f2) | subject), 
	data=data[data$bound==T,], family="binomial")
	
l1 <- glmer(correct ~ freq + bound + (freq + bound|subject),
	data=data, family="binomial")
	
l2 <- glmer(correct ~ freq * bound + (freq * bound|subject),
	data=data, family="binomial")
	
l3 <- glmer(correct ~ log(freq) + bound + (log(freq) + bound|subject),
	data=data, family="binomial")
	
l4 <- glmer(correct ~ log(freq) * bound + (log(freq) * bound|subject),
	data=data, family="binomial")	