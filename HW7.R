setwd("/home/aly/Classes/Ecobiometry")
library(MASS)
crabs<-crabs
str(crabs)
head(crabs)
summary(crabs)

crabs2<-crabs[crabs$sex=="F",-(2:3)] 

summary(crabs2)
summary(crabs[crabs$sex=="M",-(2:3)] )

library(dplyr)
crabs %>% group_by(sp) %>%
  summarise(
    across(c(FL,RW,CL,CW,BD), list(mean = mean, sd = sd), na.rm = TRUE)
  )

manova(cbind(FL,RW,CL,CW,BD)~sp, data=crabs)
summary(manova(cbind(FL,RW,CL,CW,BD)~sp, data=crabs))

# 1) Calculate the means and standard deviations for all five traits for 
#each species. Does it look like there are significant differences? 
# From a univariate approach, the means of some traits have small 
# differences, with some differences in means being as much as 1 SD away
# from the other mean.
# The MANOVA indicates signigicant difference between groups

# 2) Report the approximate F and the p for this test and whether this 
#means the species are morphologically distinct (even if overlapping). 
# approx F is 267.22 p-value < 2.2e-16

library(vegan)
data(varespec)
data(varechem)
str(varechem)
str(varespec)

#Perform an RDA using only the three non-chemistry columns 
#(pH, humdepth, varesoil) to see how the abundances depend 
#on the soil type. Hint: Use the formula version of the rda 
#command (library vegan). Do a triplot.

m<-rda(varespec~pH+Humdepth+Baresoil,varechem)
plot(m)
summary(m)
library(vegan3d)
ordiplot3d(m)

#. 3) Interpret the loadings and triplot. Write a short paragraph on 
#conclusions you can draw. Hint: there is a common problem that shows
#up in this plot. Be sure to mention it.
#The loadings of pH Humdepth and Baresoil all appear to be loading on RDA 1
# and RDA 2, with none of them moving into the third dimension (RDA3)
#The loading indicate very little variance explained by 
# RDA3 (0.7% of variance explained)


##PCA on environment####

#Run a PCA on the environmental (env.csv from my website) data. 
env<-read.csv("./env.csv")
str(env)

m<-princomp(env,cor=T)
summary(m)
biplot(m)

#4) How many components do you need to cover 75% of the variance and 
#how many components would you include in further analyses (and why?) 
# NB no one right answer so explain your choice. 
# You need 6 components to cover 75% of the variation. I this case I 
# would use the first 2 PCs given that the Proportion of Variance
# explained drops off heavily in PC3 (~9.5%). Additionally, the 
#environmental vectors largely load heavily in PC1 or PC2 with a 
# high degree of interchangeability given the spread of the 
#vectors at near parallel loading with either PC. 

#Look at the loadings. 
m$loadings[,1:5]
corrplot(m$loadings[,1:5], method="circle")


#5) What is the loading on Component 1 for clP and give a verbal 
#interpretation of the first 5 components.
# Component 1 for clP is 0.150
# Comp 1: Scaling from colder less seasonal temperatures (low values) to hot more seasonal temperatures (high values)
# Comp 2: Tropics to Desert, scaling from (low values) High, consistent precip and productivity to
# low precip low productivity with higher degrees of variability -- Tropics to Desert
# Comp 3: Forest vs Agriculture
# Comp 4: Messy, Broad leaf or grassland compared to Conifer or cropland
# Comp 5: Messy: Grass or wetland compared to Urban of forest

#Now try a factor analysis. Omit the landcover variables 
#(use d[,1:10] to get the first 10 variables).
#Run a factor analysis for 2 factors with no rotation (rot=”none”),
#promax rotation and varimax rotation. 

#Factor analysis=PCA w/rotation
fact<-factanal(env[,1:10],2,scores=c("regression"),rotation="none")
loadings(fact)
corrplot(loadings(fact), method="circle")
factPro<-factanal(env[,1:10],2,scores=c("regression"),rotation="promax")
loadings(factPro)
l<-loadings(factPro)[,c(1:2)]
l[7,1]<-1
corrplot(l, method="circle")
factVari<-factanal(env[,1:10],2,scores=c("regression"),rotation="varimax")
loadings(factVari)
corrplot(loadings(factVari), method="circle")


#6) Which of these is easiest to interpret? 
#What interpretation would you give it? 
#factPro has the best seperation of loadings across factors
#Factor 1 is a composit of temerature and seasonality scaling from year round warm temp to
# Seasonally cold temps
#Factor 2 is Growing season length and intensity from short lower temperature
# to long higher temperature



df<-read.csv("http://130.111.193.22/stats/dickcissel.csv")
str(df)
df$PA<-as.numeric(ifelse(df$Present=="Present", paste0(1), paste0(0)))

m<-princomp(df[,3:9],cor=T)
summary(m)
biplot(m)

m_lmPca<-glm(df$PA~m$scores[,1],family = binomial)
summary(m_lmPca)

#7)What is the regression equation and what is the AIC?
#df$PA~pca$scores[,1], AIC=892.16

##PCA on Crabs####
plot(crabs2)

crabs_pca<-princomp(crabs2[,2:6])
summary(crabs_pca)
screeplot(crabs_pca)
corrplot(crabs_pca$loadings, method="circle")
biplot(crabs_pca)
df<-as.data.frame(scores(crabs_pca))
df$sp<-crabs2$sp
plot(df$Comp.1, df$Comp.2, col=df$sp)

# 8) What percentage of variance is covered by the first two principal
#components, give verbal interpretations of the first four components 
#and describe verbally how (and if) the first two components succeed in 
#dividing the species. 
#The first 2 components account for 99.8$ of variance. PC1 is overall body size,
#PC2 is head to carapace ratio
#PC3 is Rear Width size
#PC4 is head to body depth ratio
# The first two PCs are very successful in dividing species. 

str(crabs)
crabs$lab<-factor(c("B","b","O","o")[rep(1:4,each=50)])
head(crabs)

crabs_pca<-princomp(crabs[,4:8])
summary(crabs_pca)
screeplot(crabs_pca)
corrplot(crabs_pca$loadings, method="circle")
biplot(crabs_pca)

table(crabs$sp,crabs$lab)
table(crabs$sex,crabs$lab)


#9) Look at lab and describe in words what these labels mean
#Lab is a composit of species and sex, where the leter denotes
#species, and the capitalization is sex (Captial for Male, lower
#case for female)

df<-as.data.frame(scores(crabs_pca))
df$lab<-crabs$lab
plot(df$Comp.1, df$Comp.2, col=df$lab)
legend("bottomleft",
       legend = levels(df$lab),
       col = seq_along(levels(df$lab)),
       pch = 1)

#10) Describe in words how the first two principal components separate 
#species AND gender.
#The first two PCs do a pretty good job dividing both sex and species
#There is some overlap in "b" with other groups

####3 – Ordination ####
data(mite)
miteab<-mite[,-16]

mite_pca<-princomp(miteab)
summary(mite_pca)
screeplot(mite_pca)
corrplot(mite_pca$loadings, method="circle")
biplot(mite_pca)

#11) Write a paragraph interpreting this ordination. Hint: What species 
#structure the first axis? What species structure the second axis? 
#What sites are extreme? 
#The first and second axis both heavily load on TVEL, ONOV, and LRUG. The
#primary difference in the axis appear to be that the first axis also is negatively
#loading on SUCT, and the directionalitly of the loadings is opposite between 
#1 and 2 on ONOV and to a lesser degree on HMIN
set.seed(345)
par(mfrow=c(2,2))
biplot(mite_pca)
mite_NMDS<-metaMDS(miteab, distance="bray", k=2)
plot(mite_NMDS)
text(mite_NMDS)
mite_pcoa<-cmdscale(vegdist(miteab, method="bray"), k=2)
plot(mite_pcoa)
text(mite_pcoa)


#12) Compare and contrast the results from these two techniques with the PCA.
#Each test gave very different results, PCoA was the least uniformly distributed
#with most of the spread on the first dimension. Different sites appeared
# as divergent between NMDS and PCA with no clear aggreement across tests. 

####4 – Multivariate - Clustering ####
library(cluster)
par(mfrow=c(1,2))
let<-crabs$lab
cl=diana(crabs[,4:8])
plot(cl)
cutree(cl,k=4)
par(mfrow=c(1,1))
plot(cl,lab=let,w=2,cex=0.8)

#13) Do four distinct clusters appear?
#No, the clusters do not map on to the species 

lab<-crabs$lab

d=data.frame(assigned=cutree(cl,k=4),class=lab)
table(d)
plot(cl,lab=lab, w=2, cex=0.5) 

#14) Describe in words what these three commands did (i.e. write a 
#1-2 sentence methods statement) and then describe in words your 
# interpretation of the results. 
#Here we mapped the cluster analysis output groups for a definied k (number of ground)
#of 4 and compared those clusters to the classes (aggregate of sex and species)
#The table shows a lack of agreement between the clusters and classes, 
#and the same can be seen in the tree diagram 
