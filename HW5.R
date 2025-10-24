df<-read.csv("http://130.111.193.22/stats/rich.csv")

str(df)
head(df)
table(df$precip)
table(df$disturb)
table(df$rich)

cor(df[,-1])
library(lattice)
xyplot(rich~precip|site, data=df)
xyplot(rich~disturb|site, data=df)
