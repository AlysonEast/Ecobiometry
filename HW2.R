
df<-read.csv("http://130.111.193.22/stats/dickcissel.csv")

head(df)
str(df)

hist(df$abund)

#1) Yes, a transforamtion is needed due to the heaxy skew of the histogram
df$abund<-log10(df$abund+0.1)


pairs(df[,-2])
cor(df[,-2])

#2) clDD and clFD are very negitivly corrialted (-0.98), as is clDD and clTmi (0.97), and clDD with clTmn (0.99)
m<-lm(abund~clTma+NDVI+grass, data = df)
m.scaled=lm(scale(abund)~scale(clTma)+ scale(NDVI)+
              scale(grass), data=df)

summary(m)
summary(m.scaled)
#3) The P-values on all of the coeffiecents satayed the same between scaled and unscaped, except for the intercept, which only significant in the unscaled model

#4) Overall model mdoel stats do not change

#5) clTma has the greatest effect followed by grass followed by NDVI

plot(abund~clDD,data=df) 

temp.linear=lm(abund~clDD,data=df)
temp.quad=lm(abund~clDD+I(clDD^2),data=df)
temp.cubic=lm(abund~clDD+I(clDD^2)+I(clDD^3),data=df) 

anova(temp.linear,temp.quad,temp.cubic) 
summary(temp.quad)

#6) Based on the ANOVA we should use the second order model: abund~clDD+I(clDD^2) with p-value 8.279e-07 and R-Squared of 0.04

full<-lm(abund~.-Present, data=df)
summary(full)
#7a) R-Squared 0.6066 and p-value < 2.2e-16
step<-step(full)
step
colnames(df[,-2])
summary(step)

#7b) crop and wetland were dropped from the full model, the new R-squared is 0.06062 and the p-value is still  < 2.2e-16

#8) Yes, I expected the R-squared to reduce given that there were fewer varaibles, but it was a very very modest reduction

summary(lm(abund~clDD+clFD+clTmn, data = df))
dim(df)

for (i in 1:10) {
  print(summary(lm(abund~clDD+clFD+clTmn,data = df[sample(nrow(df),100),]))) 
}
#9-11)Coefficient change a lot, including changing signs from one run to the next, given the high level of correlation in these varaibles this is a bouncing beta


#12)
