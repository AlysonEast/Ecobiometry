# Section #1 - GLIM
# Unit #1a – Logistic regression
# This unit also uses the dickcissel dataset (http://130.111.193.18/stats/dickcissel.csv). In the past
# we did regressions using log10(abund) as the dependent variable. For logistic regression we will use the
# variable Present which indicates presence (abund>0) or absence (abund=0). Note that this is a habitat
# suitability model similar to the two paper by Zabel. We want Present to be coded as a 0 for absent and a 1
# for present. Newer versions of R don’t do this, so type the following:
#   d$Present=as.numeric(d$Present)-1

df<-read.csv("http://130.111.193.22/stats/dickcissel.csv")
head(df)
str(df)
table(df$Present)

df$Present<-as.numeric(ifelse(df$Present=="Present", 1, 0))
str(df)
table(df$Present)

# Lets look at how precipitation affects presence/absence
plot(Present~clP,data=df) # doesn’t give what you expect
plot(clP~Present,data=df) # little more useful
plot(df$clP,df$Present) # uses plot(x,y) format
plot(df$clP+df$clP^2,df$Present) # looks like there might be some separation in quadratic
# Now confirm the intuition from plots using the glm command. Create two model objects using the glm
# command. Have one using Present~clP and one with Present~clP+I(clP^2). When you use the glm
# command, don’t forget you have to ALWAYS SPECIFY THE FAMILY. What family should you use for
# logistic? Compare these two objects using the “anova” command. Is this significant? Do you remember
# what the distribution of this difference is? now try
m1<-glm(Present~clP, data = df, family = "binomial")
m2<-glm(Present~clP+I(clP^2), data = df, family = "binomial")

anova(m1,m2,test="Chisq") # m1,m2 are your two model objects
AIC(m1, m2)
863.0832-861.1801

# 1) Report the p-value for a quadratic relation of precipitation vs. linear. Also report the two AIC
# values and the How would Burnham and Anderson describe the support for the linear model
# relative to the quadratic (look up  in the table given in the class notes)

#### The chi squared test indicates that the quadratic model is significantly better (p-value = 0.0482), 
#### however AIC indicates that the strength of evidence against the higher-AIC model is substantial (Delta AIC = 1.9)

# Plot the linear case:
plot(df$clP,df$Present)
x=seq(0,200,0.5) #creates a vector of 0,0.5,1,1.5, ….
lines(x, predict(m1,data.frame(clP=x),type="response")) #m1 is your model object
abline(h=0.5, col="red")

# Looking at the graph, 

#2) report at approximately what value of clP do we get a 50% probability of being present?
#### ~110

#   Unit #1b – Poisson regression for log linear models
# Start by loading the dataset http://130.111.193.18/stats/birdsdiet2.csv . Let’s printout a
# contingency table of the count of cells in each category of Passerine/Aquatic/Diet:
df2<-read.csv("http://130.111.193.22/stats/birdsdiet2.csv")
table(df2$Diet,df2$Suborder,df2$Habit)#prints out contingency table
xtabs(~Diet+Suborder+Habit,data=df2)
# To do Poisson regression we need a column that has counts. To get it do this:
d2<-as.data.frame(xtabs(~Diet+Suborder+Habit,data=df2)) 
head(d2)
# Look at your new dataframe. What did we do? What is the variable/column with counts? Run a poisson
# regression using glm on your new dataframe. Don’t forget to specify a family! Hint: don’t use the 
# oneway terms such as Diet – hint on hint – this means you don’t want to use Diet*Suborder*Habit (and only do 3-way interaction)                                                                                                                                                                                                    do 3-way interaction). Analyze your results using the Anova command (library car). Alternatively, for
# GLIM, using anova(m,test=”Chisq”) will give the same result. 
m3 <- glm(Freq ~ Diet + Suborder + Habit + Diet:Suborder + Diet:Habit + Suborder:Habit,
         data = d2, family = poisson)

anova(m3,test="Chisq") 

#3) Report what significant interactions are there?
#### All interactions are significant. 

# Look for overdispersion: 
summary(m3)


# 4) is the residual deviance close to the degrees of freedom?. 
#### no, 3.1802e-10  on  4  degrees of freedom

# If it is much larger it is overdispersed. Run a model assuming overdispersion using the family=quasipoisson.
# Look at the output. 
m4 <- glm(Freq ~ Diet + Suborder + Habit + Diet:Suborder + Diet:Habit + Suborder:Habit,
          data = d2, family = quasipoisson)
summary(m4)
# 5) What changed?
# Nothing changed

#   Unit # 1c – Poisson (etc) regression on count data
# Load the dataset “dickcissel.csv”. Look at the fields again (you’ve seen this before). Note that the
# abundances are decimal (averaged over 5 years). Round the abund (i.e set abund to round(abund) ).Plot a
# histogram of abundances. 
df3<-read.csv("http://130.111.193.22/stats/dickcissel.csv")
head(df3)
str(df3)
hist(df3$abund)
table(df3$abund)
table(df3$abund)[1]
table(df3$abund)[1]/dim(df3)[1]


#6) How many abundances are zero? What percentage of all records is this? 
#### 306 instances are 0, that is 47% of all records.

# Fit a Poisson regression of round(d$abund) vs. clDD, clFD, clP, NDVI (no interaction terms!).
# Check for overdispersion. 
m5<-glm(abund~clDD+clFD+clP+NDVI, data = df3, family = poisson)
summary(m5)

# 7) What is the deviance? How does this compare to the degrees of freedom?
#### the deviance is very high and much higher than the degrees of freedom: 16419  on 641  degrees of freedom

#   Now run a quasipoisson, a negative binomial (hint: glm.nb in library MASS), a zero-inflated
# negative binomial, and a hurdle negative binomial (hint library pscl for the last two – see class lecture notes).
m6<-glm(abund~clDD+clFD+clP+NDVI, data = df3, family = quasipoisson)
library(MASS)
library(pscl)
m7<-glm.nb(abund~clDD+clFD+clP+NDVI, data = df3)
m8<-hurdle(as.integer(abund)~clDD+clFD+clP+NDVI, data = df3)
summary(m6)
summary(m7)
summary(m8)

AIC(m6, m7, m8)

# 8) what are the AICs for each model? Can you get AIC for every model? Which model appears the best? 
#### The quasipoisson does not give and AIC model, but based on AIC the negitive binomial appears to be the best model