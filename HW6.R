df<-read.csv("http://130.111.193.22/stats/birdsdiet2.csv")
head(df)

library(lmodel2)
m<-lmodel2(log10(MaxAbund)~log10(Mass), data=df, nperm=1000) #OLS, RA, SMA
m
#2) How do the different slopes compare? Are the intercept and slope for SMA significantly different than for OLS? 
# OLS is a much shallower slope (-0.2361498) than MA or SMA (-0.6222027, -0.8760106 respectivly)

beta<-m$regression.results
beta
#plot a scatter plot of log(MaxAbund) vs log(Mass)
plot(log10(MaxAbund)~log10(Mass), data=df)
abline(beta[1,2],beta[1,3],lty=1)
abline(beta[2,2],beta[2,3],lty=2)
abline(beta[3,2],beta[3,3],lty=3)
legend(0.5,0.5,c("OLS","MA","SMA"),lty=c(1,2,3)) 

# 3) Visually which line seems like the best fit. Why? 
# Visually, MA or SMA apprear best, MA looks like it bits the lower end of log10Mass best, 
#SMA may bit the upper end a little better.

df<-read.csv("http://130.111.193.22/stats/dickcissel.csv")
lab<-log10(df$abund+0.1) 
dd<-df$clDD 

plot(lab~dd) 

m1<-lm(lab~dd)
abline(m1,lty=1) 
library(MASS)
m2<-rlm(lab~dd)
abline(m2,lty=2) 
mbsq<-rlm(lab~dd,method="MM")
abline(mbsq,lty=3) #bisquare M-estimation
mlts<-ltsreg(lab~dd)
abline(mlts,lty=4) #LTS robust regression 

summary(m1)
summary(m2)
summary(mbsq)
mlts

#4) Report the slope and intercept for the four regressions (OLS, Huber M-estimate,
# Bisquare M-estimate, LTS), no confidence intervals. [Caution: use the print command
# instead of summary to get coefficients for LTS) 

#OLS: slope=.00009930, intercept=-0.5208
#Huber M-estimate: slope=0.0001, intercept=-0.5284
#Bisquare M-estimate: slope=0.0001, intercept=-0.6907
#LTS: slope=.00001289, intercept=-1.029

library(quantreg)
#Linear quantiles (lines)
plot(lab~dd)
m10=rq(lab~dd,tau=0.10)
abline(m10, col="blue")
m50=rq(lab~dd,tau=0.5)
abline(m50)
m90=rq(lab~dd,tau=0.9)
abline(m90, col="red")

summary(m10)
summary(m50)
summary(m90)

#5) Report the slopes for these three lines. 
#10% slope=0
#50% slope=0.00015
#90% slope=0.00013

#fit a 90% quantile use qss to indicate a spline on dd
#constraint=N means no contraint (e.g. strictly increasing)
#lambda=1000 =smooth across a window 1000 wide 
fit<-rqss(lab~qss(dd,constraint="N",lambda=1000),tau=0.9) 
#normally plot blows away what is underneath, but not for rqss
plot(fit,col="forestgreen",lwd=2,lty=2,add=T) 

#6) Report approximately what degree day value has the maximum abundance according
#this graph (for DD<7000) 
abline(v=7000)
abline(v=4500)
abline(v=4700)
abline(v=4800)
#Degree day ~4800 has the maximum abundance
plot(rqss(lab~qss(dd,constraint="N",lambda=1000),tau=0.95),
     col="gold",lwd=2,lty=2,add=T) 
plot(rqss(lab~qss(dd,constraint="N",lambda=500),tau=0.95),
     col="gold",lwd=2,lty=1,add=T) 
plot(rqss(lab~qss(dd,constraint="N",lambda=2000),tau=0.95),
     col="gold",lwd=2,lty=3,add=T) 
plot(rqss(lab~qss(dd,constraint="N",lambda=3000),tau=0.95),
     col="red",lwd=2,lty=3,add=T) 
plot(rqss(lab~qss(dd,constraint="N",lambda=3000),tau=0.75),
     col="red",lwd=2,lty=1,add=T) 
plot(rqss(lab~qss(dd,constraint="N",lambda=3000),tau=0.60),
     col="red",lwd=2,lty=2,add=T) 
plot(rqss(lab~qss(dd,constraint="N",lambda=3000),tau=0.50),
     col="pink",lwd=2,lty=2,add=T) 


#7) Report a one sentence result of something interesting you found while playing with this. 
#Increasing the lamba above 1000 doesn't seem to have much effect on the pattern, 
# and the overall shape of the relationship is fairly static above the 60th percentile. 
plot(lab~dd)

mnl=nls(lab+1~a*dd/(half+dd),start=list(a=1,half=3000)) 
mnl
#8) Report the asymptote and temperature at which half the asymptote is reached 
#(caution – don’t forget to backtransform by subtracting 1 where needed). 
# a = 0.623
# half = 3079.223

x=seq(1000,8500,length=101)
lines(x,predict(mnl,data.frame(dd=x)),col="blue", lwd=2) 

library(segmented)
mln=lm(lab~dd,subset=which(lab>-1.0)) #subset to abundances off the x-axis 
m.seg<-segmented(mln,seg.Z=~dd, psi=4000)

m.seg
summary(m.seg)
confint(m.seg)
plot(m.seg,add=T, col="red")

#9) Does the model depend on initial guesses? 
#Only in that it needs to be good enough to converge

#Now fit a model with two breakpoints. Try a couple. 
m.seg2<-segmented(mln,seg.Z=~dd, psi=c(4000,4500))
m.seg2<-segmented(mln,seg.Z=~dd, psi=c(2000,4500))
plot(m.seg2,add=T, col="gold")
m.seg2<-segmented(mln,seg.Z=~dd, psi=c(1000,6000))
plot(m.seg2,add=T, col="green")
m.seg2<-segmented(mln,seg.Z=~dd, psi=c(4000,6500))
plot(m.seg2,add=T, col="forestgreen")

#10) Can you find any initial breakpoints that work? 
# May breakpoints work, given they are not too close together

#Try 4500/6500. Compare both of the one breakpoint and the two
#breakpoint models using the command AIC. 
AIC(m.seg, m.seg2)

#11) Which is superior? 
#The single break point is superior (delta AIC = 0.8578)

#### Section 4####
#setup for plotting
plot(lab~dd)
x=seq(1000,9000,length=101)
# loess plot
mlo=loess(lab~dd,span=0.75)
lines(x,predict(mlo,data.frame(dd=x)),lty=1,col="green")
# spline plot
library(splines)
msp=lm(lab~ns(dd,df=5))
lines(x,predict(msp,data.frame(dd=x)),lty=2,col="blue")
#kernel smoothing – note no object produce, output is two
# columns which go straight into lines as the x & y inputs
lines(ksmooth(dd,lab,"normal",bandwidth=1000),lty=3,col="red")

#12) What does your eyeball estimate tell you is the best smoothing parameter for loess? bandwidth for ksmooth? 
#For Loess a smoothing parameter of 0.5 and fpr ksmooth a binwidth of 2000 appear to be good fits. 
lines(ksmooth(dd,lab,"normal",bandwidth=2000),lty=1,col="red")
lines(ksmooth(dd,lab,"normal",bandwidth=500),lty=1,col="red")
mlo=loess(lab~dd,span=1)
lines(x,predict(mlo,data.frame(dd=x)),lty=3,col="forestgreen")
mlo=loess(lab~dd,span=.5)
lines(x,predict(mlo,data.frame(dd=x)),lty=3,col="forestgreen")


#13) Report a (non-eyeball) prediction of the lab for dd=5000 using the loess with span=0.9.
#0.1541846

mlo=loess(lab~dd,span=.9)
predict(mlo, 5000)
#Dummy Check
lines(x,predict(mlo,data.frame(dd=x)),lty=1,col="purple")
abline(v=5000)
abline(h=0.1541846)

#Section 5
dc.lm<-lm(log(abund+0.1)~.-Present,data=df)

r2<-function(y,obj) {cor(y,predict(obj))^2}
r2(log(df$abund+0.1),dc.lm)

library(mgcv)
dc.gam<-gam(log(abund+0.1)~s(clTma)+s(clP)+s(NDVI)+s(grass),data=df)
dc.gam
r2(log(df$abund+0.1),dc.gam)
#14) Report the r2 of the GAM. 
#0.49

par(mfrow=c(2,2))
plot(dc.gam)
#15) which variable has the least effect (plot most like a horizontal line)
#Grass has the falttest line, but NVDI is a close second. 

library(rpart)
#rpart doesn’t handle the ~.-Present notation
d2=df[,-2] #create a new dataframe without Present in
names(d2) #make sure Present is gone
dc.rp<-rpart(log10(abund+0.1)~.,data=d2)

par(mfrow=c(1,1))
plot(dc.rp)
text(dc.rp,digits=2)

print(dc.rp)

#17) Report what the top three nodes in the tree are, how many cases are found in each split, and what the predicted abundance is.
#clTma< 29.75 (n=242) abund=0.61129970, clTmin>=-0.35 (n=404) abund=0.23370760, cIP>=101.6 (n=158) abund = 0.89592470

samp<-sample(1:646,323)
dc.rp1<-rpart(log10(abund+0.1)~.,data=d2[samp,])
dc.rp2<-rpart(log10(abund+0.1)~.,data=d2[-samp,])

par(mfrow=c(1,2))
plot(dc.rp1)
text(dc.rp1,digits=2)
plot(dc.rp2)
text(dc.rp2,digits=2)
#18) Are the trees the same? Do they have the same biology? Report briefly. 
#The trees are similar, but not the same, this split has the same varaibles in the top 3 splits, but different split points and different predicted values
# The further down the tree you go, the more extream the differences, so there is likly an undlying broad pattern, but the lower branches may be due to over fitting

plotcp(dc.rp)

#19) Report the cp and n value
#cp~0.2, or n=8, the pruned tree is slightly smaller

dc.rp.prune=prune(dc.rp,cp=0.02)
print(dc.rp.prune)
plot(dc.rp.prune)

plot(dc.rp)
text(dc.rp,digits=2)
plot(dc.rp.prune)
text(dc.rp.prune,digits=2)
