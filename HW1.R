df<-read.csv("http://130.111.193.22/stats/birdsdiet.csv")

str(df)

# 1) Family level data bout bird abundance, mass, and diet, as well as if the family is passerine of aquatic

df$Aquatic<-as.logical(df$Aquatic)
df$Passerine<-as.logical(df$Passerine)

table(df$Diet)
table(df$Family)

df$Diet<-as.factor(df$Diet)
df$Family<-as.factor(df$Family)

str(df)

#2) Both Diet and Family are unorderd

df[18,]

#3) The mass of Cuckoos is 95.21462

par(mfrow=c(1,2))
hist(df$Mass)
hist(log10(df$Mass))

library(e1071)
skewness(df$Mass)
skewness(log10(df$Mass))

#4) Mass is heavily right skewed, a log10 transformation makes the distribution much closer to normal (skew of 3.1 compared to 0.4)

table(df$Diet)
mean(subset(df, Diet=="Insect" | Diet=="InsectVert" | Diet=="PlantInsect")$AvgAbund)
mean(subset(df, Diet=="PlantInsect")$AvgAbund)
mean(subset(df, Passerine==TRUE)$AvgAbund)

#5) The mean of families that eat insect (even if they also eat other things) os 5.75, for families that eat insects and plants it is 8.36, and for Passerines it is 7.32
library(ggplot2)
library(ggpubr)

ggplot(df, aes(log10(MaxAbund))) +
  geom_histogram() +
  theme_pubr()

ggplot(df, aes(log10(MaxAbund))) +
  geom_density() +
  theme_pubr()

ggplot(df, aes(log10(AvgAbund), fill = Aquatic)) +
  geom_boxplot() + 
  theme_pubr()
t.test(x=log10(df$AvgAbund), y=df$Aquatic)

library(car)
??powerTransform()
powerTransform(df$Mass, family = "bcPower")

#6) According to powerTransform the Estimated transformation parameter is -0.155985 

quantile(Soils$Ca)

#7) the 25% is 3.820, the 50% is 5.040, and the 75% is 7.305

pairs(df)
m1<-lm(MaxAbund~Mass,data=df) 
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))
hist(m1$residuals)

#8) the QQplot indicates a lack of normallity where point diverge from the 1:1 line at the upper end of the quantiles, this is supported by the histogram of residuals plot. A trandformation is needed

#9) The Residuals vs fitted indicate that the data is heteroscedastic, and there are several high leverage points.

df[32,]
df[23,]
df[43,]

#10) Pelicans, Loons, and Starlings are all high leverage

summary(m1)
m2<-lm(log10(MaxAbund)~log10(Mass),data=df) 
plot(log10(MaxAbund)~log10(Mass),data=df,
     xlab="log Mass (g)",ylab="Max Abund")
abline(m2)
summary(m2)

#11) With a log transformation, the results of the model of Max abundance as a function of Mass is significant (p-value<0.05) with very low variance explained by the model (R-Squared=0.07)
# As mass increases, the Max abundance decreases (both on a log scale)

m3<-lm(log10(MaxAbund)~log10(Mass),data=df,
       sub=!df$Aquatic) 
summary(m3)

#12) when subsetted to just terrestrial birds, the relationship shows higher levels of significance (p-value < 0.0008), and the variance explained increases (R-squared=0.2682)