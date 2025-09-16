
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

#5) ????

plot(abund~clDD,data=df) 

temp.linear=lm(abund~clDD,data=df)
temp.quad=lm(abund~clDD+I(clDD^2),data=df)
temp.cubic=lm(abund~clDD+I(clDD^2)+I(clDD^3),data=df) 

anova(temp.linear,temp.quad,temp.cubic) 

#6)

full<-lm(abund~.-Present, data=df)
step<-step(full)

