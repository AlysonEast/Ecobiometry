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
m4<-lm(rich~precip+disturb+site, data = df)
m5<-lm(rich~precip*site+disturb, data = df)
m6<-lm(rich~precip*site+disturb*site, data = df)
m7<-lm(rich~precip*disturb*site, data = df)

#and test the model comparison with AIC
AIC(m1, m2, m3, m4, m5, m6, m7)

par(mfrow=c(2,2))
plot(m5)

#Methods

#Results
# The top models by AIC are all the models that contain interaction terms. 
# rich~precip*site+disturb, rich~precip*site+disturb*site and the three way interaction rich~precip*disturb*site
# All rank similarly by AIC (Delta AIC <2).


#1b Blocking – How to treat blocking factors (GLM, GLS, GEE, LMM, GLMM) 

#For the generalized models use Poisson regression
#otherwise just use linear regression assuming that richness is far
#enough from zero that we can treat it as normal. 

#For GLM/GLIM, the block will be treated as a fixed factor. 

#For GLS/GEE place the block in the error structure. 

#For LMM/GLMM treat the block as a random effect. 

#The six models are: GLM (lm in R), GLIM (glm in R), GLS (gls in library nlme, 
#use the corCompSymm to create the correlation structure), 
#GEE (geeglm in geepack use corstr=”exchangeable”), LMM (lme in nlme), 
#and GLMM (glmer in lme4). 2) 

m_lm<-lm(rich~precip+disturb+site, data=df)
m_glm<-glm(rich~precip+disturb+site, data=df, family=poisson)

library(nlme) #contains gls command
m_gls<-gls(rich~precip+disturb, data=df,
           cor=corCompSymm(form=~1|site),method="REML")
library(geepack)
m_gee<-geeglm(rich~precip+disturb, data=df,
              family=poisson,
              id=site,
              corstr="exchangeable")
library(lme4)
m_lme<-lme(rich~precip+disturb, random=~1|site, data=df)
m_glmer<-glmer(rich~precip+disturb+(1|site), data=df, family=poisson)

summary(m_lm)
plot(m_lm)
summary(m_glm)
plot(m_glm)
summary(m_gls)
summary(m_gee)
summary(m_lme)
summary(m_glmer)

AIC(m_lm, m_glm, m_gls, m_lme, m_glmer)
min(AIC(m_lm, m_glm, m_gls, m_lme, m_glmer)[,2])

#Obtain the AIC score for the five models that have AIC (one model doesn’t – which one?)
#and determine the best fit. How do the effect sizes and significances vary between models
#(you can’t get p-values for GLMM)? 

#the GLM has the best AIC
# The directionallity of all of the effect sizes are the same, however the estimated effect size's vary 
# by a wide margin.


#2 Mixed – Nested data and HLM ####
df2<-read.csv("http://130.111.193.22/stats/birds.csv")
str(df2)

m<-lme(sqrt(MaxAb)~1,random=~1|Order/Family/Genus,data=df2)
library(ape)
varcomp(m,scale=1) #scale makes add up to 100% 

m<-lme(sqrt(NumRts)~1,random=~1|Order/Family/Genus,data=df2)
varcomp(m,scale=1) 

m<-lme(sqrt(Mass)~1,random=~1|Order/Family/Genus,data=df2)
varcomp(m,scale=1) 

m<-lme(sqrt(Diet)~1,random=~1|Order,data=na.omit(df2))
varcomp(m,scale=1) 

#3) How much variance is at the species/genus/family/order level in max abundance? How
#about range size, body mass and diet? Are these labile or conserved traits? Where is the
#variance for diet – careful – this is coded at the family level so don’t include species or
#genus in the analysis (or you will get an error) and remember the lowest level is implied
#rather than explicit. 

# 8%, 19% and 18% of the variance in max abundance occurs at the Order, Family, and Genus level respectively, 
# so 54% of the variance occurs between individual species
# 90.57% of the variance in Range size occurs at the species level
# Variance in Mass is dominated by Order (65%), and Genus (25%), with only ~5% at both the Family and Species Level
# Diet variance is 71% in the Order, with 23% in Family

xyplot(sqrt(MaxAb)~log10(Mass)|Family, data=df2)

#4) Do they look like there are different slopes in different families? Can you identify 
#one family that looks clearly positive? clearly negative?

#Slopes very greatly by family, for example Icteridea is clearly strongly positive, while Anatidea is negitive

#Using lm (and the same variable transformations as question #9) run a) a flat (species-level only)
#regression, then using lmer (in lme4) run hierarchical models at the species/family levels for b) a
#fixed slope, variable intercept model, c) a variable slope, variable intercept model, and d) use
#model c) and include the level II effect of diet at the family level (no interaction of diet with
#mass). 

m_lmspp<-lm(sqrt(MaxAb)~log10(Mass), data = df2)
summary(m_lmspp)
m_b <- lmer(sqrt(MaxAb)~log10(Mass) + (1 | Family), data = df2, REML = FALSE)
summary(m_b)    # shows fixed effect, random intercept variance
m_c <- lmer(sqrt(MaxAb)~log10(Mass) + (1 + log10(Mass) | Family), data = df2, REML = FALSE)
summary(m_c)
m_d <- lmer(sqrt(MaxAb) ~ log10(Mass) + Diet + (1 + log10(Mass) | Family), data = df2, REML = FALSE)
summary(m_d)

AIC(m_lmspp, m_b, m_c, m_d)
min(AIC(m_lmspp, m_b, m_c, m_d)[,2])
summary(m_b)

# 5) Use model selection to pick the best model. What are the effect sizes and
# significances in this model?

#The fixed effect hierarchical model and the hierarchical model with Diet have a delta AIC of 1,
# as such I am selecting the fixed effect hierarchical model as it is more parsimonious
# Maxab increased by 0.493 for each unit increase in log10(Mass)



#3 - Bootstrapping  ####

df3<-read.csv("http://130.111.193.22/stats/dickcissel.csv")
str(df3)

df3$AI<-df3$clDD/df3$clP
median(df3$AI)
max(df3$AI)
hist(df3$AI, breaks = 50)

#6) AI is very heavily centered around ~40, with a long tail distribution on the high end of the values (max =171.2)

mean(df3$AI)
#7) the mean of AI is 47.56

library(boot)
set.seed(42)   # for reproducibility
# Example: Function to calculate the mean
mean_statistic <- function(data, indices) {
  return(mean(data[indices]))
}
boot_out <- boot(data = df3$AI, statistic = mean_statistic, R = 1000)

boot_out
boot.ci(boot_out, type = "norm")

df3[500,]

plot(boot_out)

# 47.56 with a 95% confidence interval of 45.83 to 49.30

#Yes