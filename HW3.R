

df<-read.csv("http://130.111.193.22/stats/birdsdiet.csv")

str(df)
head(df)

hist(df$MaxAbund)
df$MaxAbund<-log10(df$MaxAbund+0.1)
hist(df$MaxAbund)

table(df$Diet)
df$Diet<-factor(df$Diet, levels = c("Plant", "PlantInsect","Insect","InsectVert","Vertebrate"))

m1<-lm(MaxAbund~Diet, data = df)
model.matrix(m1)
par(mfrow=c(2,2))
plot(m1)

summary(m1)

#1) What is the effects size of various levels of diet? 
#1) InsectVert has the lowest log10 abundance and is on average -0.8725 less than Plant,
#followed by Vertebrate which is on average -0.5231 less than Plant 
#then DietInsect is on average -0.2294 less than Plant
# and DietPlantInsect is 0.1811 more than plant

m2<-lm(MaxAbund~Diet*Aquatic, data = df)
par(mfrow=c(1,1))
interaction.plot(df$Diet,df$Aquatic,df$MaxAbund) 
#2) What do the gaps in the line for the Aquatic group mean? 
# The gaps in the lines are the difference in average in MaxAbund between Aquatic and non-Aquatic for each diet type


#3) Does the plot suggest an interaction?
# the gaps in the lines are inconstant, this would indicate a potential interaction

par(mfrow=c(2,2))
plot(m2)
summary(m2)
anova(m2)

#4) Is the interaction term significant? 
# The interaction term appears does not appear to be significant overall (p-value = 0.07518)


m3<-lm(MaxAbund~Diet+Aquatic, data = df)
plot(m3)
summary(m3)
anova(m3)

#5) What is the treatment effect of diet and aquatic/terrestrial on abundance (when the interaction is omitted)? 
# on average log10 MaxAbund of Aquatic is 0.1888 higher than terrestrial, but this relationship was not significant (p=0.39980)

#Finally, lets do an ANCOVA with diet and mass. 
par(mfrow=c(1,1))
hist(df$Mass)
hist(log10(df$Mass))

m4<-lm(MaxAbund~Diet+log10(Mass), data = df)
par(mfrow=c(2,2))
plot(m4)
# m5<-lm(MaxAbund~Diet*log10(Mass), data = df)
# plot(m5)

summary(m4)
anova(m4)
# 6) Was diet significant? 
# Diet was significant (p-value= 0.03415)

# 7) What is the effect sizes of diet & mass. 
# For every one unit increase in log10(Mass), there is a 0.1005 decrease in log10(MaxAbund)
# The average log10(MaxAbund) of species with a diet of plants would be 1.5720
# A species with a InsectVert diet would be an average 0.8343 log10(MaxAbund) less than a plant diet
# A species with a Vertebrate diet would be an average 0.4031 log10(MaxAbund) less than a plant diet
# A species with a Insect diet would be an average 0.2398 log10(MaxAbund) less than a plant diet
# A species with a PlantInsect diet would be an average 0.1712 log10(MaxAbund) Higher than a plant diet 

#8) What is your r2? 
# R-squared is 0.1978


m5<-lm(MaxAbund~log10(Mass), data = df)
summary(m5)
anova(m4)

# 9) How does this compare to the r2 with just mass?
# The R-squared with just Mass is 0.07219

mmassdietnoaq<-lm(MaxAbund~log10(Mass)*Diet,data=df,sub=!df$Aquatic)

library(ggplot2)
ggplot(data=df,aes(x=log(Mass),y=MaxAbund))+geom_point()+
  geom_smooth(method='lm',se=FALSE)+facet_wrap(~Diet) 

#10) What if you limit the analysis to terrestrial birds (using the “sub” option). What is this r2?
#Write these results into your report. Did this analysis allow the slopes to vary between diets?
