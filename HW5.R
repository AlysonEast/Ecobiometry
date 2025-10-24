df<-read.csv("http://130.111.193.22/stats/rich.csv")

#1a Blocking – Whether to treat blocking factors
str(df)
head(df)
table(df$precip)
table(df$disturb)
table(df$rich)

pairs(df[,-1])
library(lattice)
xyplot(rich~precip|site, data=df)
xyplot(rich~disturb|site, data=df)

#It looks like there is a relationship between both Richness and Precip, and Richness and Disturbance
#When you look at these by site, there is a clear interaction between precip and site, and maybe one between disturbance and site
#With this visual inspection we test the following models:
#Model Selection
m1<-lm(rich~precip, data = df)
m2<-lm(rich~disturb, data = df)
m3<-lm(rich~precip+disturb, data = df)
m4<-lm(rich~precip*site+disturb, data = df)
m5<-lm(rich~precip*site+disturb*site, data = df)
m6<-lm(rich~precip*disturb*site, data = df)

#and test the model comparison with AIC
AIC(m1, m2, m3, m4, m5, m6)

#Methods

#Results


#1b Blocking – How to treat blocking factors (GLM, GLS, GEE, LMM, GLMM) 





#Obtain the AIC score for the five models that have AIC (one model doesn’t – which one?)
#and determine the best fit. How do the effect sizes and significances vary between models
#(you can’t get p-values for GLMM)? 