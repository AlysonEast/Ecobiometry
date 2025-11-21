library(MASS)

str(nottem)
plot(nottem)

t3<-stl(nottem,s.window='periodic')
str(t3)
plot(t3$time.series)
head(t3$time.series)
which.min(t3$time.series[1:12,1])
which.max(t3$time.series[1:12,1])
min(t3$time.series[1:12,1])
max(t3$time.series[1:12,1])


#1)What is the range in seasonal temperature effects 
#(how many degrees betweenwarmest and coldest month) 
#and what is the warmest and coldest month. 

#The range of seasonal varation is -9.85 to 12.84 with the min in February and the max in July

extremes<-t3$time.series[,2]+t3$time.series[,2]
head(extremes)
which.min(extremes)
47/12 
.916667*12 #Nov 1923
which.max(extremes)
which.max(extremes)/12
1920+round(which.max(extremes)/12)
.91667/12 # Nov 1935

#2) Now look at the trend and remainder data. Adding trend and remainder, what is
#the most unseasonably warm month across the whole time span? The most unseasonably cold month?
# unseasonably warm was Nov 1935, unseasonably cold was Nov 1923

nttrend<-t3$time.series[,'trend']
ntseasonal<-t3$time.series[,'seasonal']
nttresid<-t3$time.series[,'remainder'] 

acf(nttrend, lag.max = 12)
acf(nttrend, lag.max = (12*12))
#3) Excepting the first year (hint look at the lag.max parameter on acf to look at 12 years),
#what is the lag (time span) associated with the largest negative correlation? largest positive
#correlation? Are either significant? Any guess what might cause these? 
# The correlation has large possitve assications in the first years that are all significant, and 
# negitive assostiation in year 11 that are all signigicant. I would assume that this is due to
# golbal cliamte ossialtions

df<-read.csv("http://130.111.193.22//stats/dickspatial.csv")
head(df)
str(df)
library(sf)
spdf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
spdf$tab<-spdf$abund^.25
plot(spdf)
#4) Is this data lattice, geospatial or point process? 
#geospatial data
library(maps)
map("state", fill = FALSE)
plot(spdf[,7], add=TRUE)


#5) Describe verbally what you notice about the abundance surface. Would you
#guess no trend, a linear trend or a quadratic trend would best describe the trend? 
#The abundance is centralized around a point and decreases moving away from it. 
#for this reason I would guess it is quadratic
spdf$lat<-df$lat
spdf$lon<-df$lon

m1<-lm(tab~1, data = spdf)
m2<-lm(tab~lat+lon, data = spdf)
m3<-lm(tab~lat+lon+I(lat^2)+I(lon^2), data = spdf)
AIC(m1, m2, m3)

#6) Use AIC to decide which trend is the best and report
#The quadratic model is the best model according to AIC (AIC=1595.334, Delta=259.517)