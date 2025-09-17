
df<-read.csv("http://130.111.193.22/stats/dickcissel.csv")

head(df)
str(df)

hist(df$abund)

#1) Yes, a transformation is needed due to the heavy skew of the histogram
df$abund<-log10(df$abund+0.1)


pairs(df[,-2])
cor(df[,-2])
cor(df[,-2])[,1]
sort(cor(df[,-2])[,1])

#2) The top three variables most highly correlated with abundance are clTma, grass, and broadleaf
# clTma, grass are both positively correlated, and broadleaf is negatively correlated. 

m<-lm(abund~clTma+NDVI+grass, data = df)
m.scaled=lm(scale(abund)~scale(clTma)+ scale(NDVI)+
              scale(grass), data=df)

summary(m)
summary(m.scaled)
#3) The P-values on all of the coefficients stayed the same between scaled and unscaled, except for the intercept, which only significant in the unscaled model

#4) Overall model stats do not change

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
#9-11)Coefficient change a lot, including changing signs from one run to the next, given the high level of correlation in these variables this is a bouncing beta

#Borcard partition 
colnames(df)
m_full<-lm(abund~clDD+clFD+clTmi+clTma+clTmn+clP+broadleaf+conif+grass+crop+urban+wetland, data = df)
m_clim<-lm(abund~clDD+clFD+clTmi+clTma+clTmn+clP, data = df)
m_habitat<-lm(abund~broadleaf+conif+grass+crop+urban+wetland, data = df)

summary(m_full)
1-0.6053
summary(m_clim)
0.6053-0.5933 #habitat only
summary(m_habitat)
0.6053-0.08919 #Clim only

1-0.51611-0.012-0.3947


#12) Unexplained Variance: 0.3947
# Climate Only: 0.51611
# Habitat Only: 0.012
# Both Explain: 0.07719

library(lavaan)

GB05Mat<-matrix(c(1006.2, -26.2, -139.4, 3636.3,
                  -26.2, 2.722, 13.47, -170.4,
                  -139.4, 13.47, 157.8, -1459.6,
                  3636.3, -170.4, -1459.6, 66693), ncol=4) 
rownames(GB05Mat)<-colnames(GB05Mat)<-c("Plant.Cover",
                                        "Fire.Severity", "Stand.Age", "Elevation") 
GB05Mat

#13) We create a contrivance matrix with varaine on the diagonal. 


model<-'Stand.Age~Elevation
        Fire.Severity~Stand.Age
        Plant.Cover~Elevation+Fire.Severity'

fit<-sem(model = model, sample.cov = GB05Mat, sample.nobs = 90)
summary(fit)
standardizedSolution(fit) 

-0.022*0.085*-7.395

#14) The Estimated coefficient of direct effect is 0.036, while the indirect effect has a coefficient of 0.0138
