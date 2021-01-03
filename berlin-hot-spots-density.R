rm(list=ls())
setwd("~/Documents/ds/sem3/Felix/transport/mycode/data-working-with-it")

wants <- c("dplyr")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

berlin.map <- read.csv('berlin-map-res-001.csv')
dim(berlin.map) # 644 x 293 = 188,692

n <- 6
l1 <- as.vector(berlin.map[berlin.map>n])
barplot(table(l1), xlab='nr of accidents per cell', ylab='nr of cells / frequency',main='most spots are cold')


# elegant transformation
l2 <- as.data.frame(unlist(l1), use.names=FALSE)

###################################
a1 <- as.vector(berlin.map[berlin.map>-1])
length(a1)
incr <- function(x){x+1}
df0 <- data.frame(x=rep(0,times=21))
for (j in 1:incr(length(a1))){
  #print(j)
  df0[a1[j]+1,'x'] <- incr(df0[a1[j]+1,'x'])
}

df1<- df0
plot.new()

par(mfrow=c(1,4), cex.main=1.5)
barplot(df1[,'x'],main='mostly 0 accidents', yaxt='n')
scaler<-10000
axis(2, at=c(0,floor(df1[1,'x']/scaler)*scaler), cex.axis=1.5)

df1[1,'x']<-0
barplot(df1[,'x'], main='if 1+ accidents, then mostly 1',yaxt='n')
scaler<-100
axis(2, at=c(0,floor(df1[2,'x']/scaler)*scaler), cex.axis=1.5)

df1[2:5,'x']<- 0
barplot(df1[,'x'], main='if 6+ accidents, then mostly 6',yaxt='n')
scaler<-10
axis(2, at=c(0,floor(df1[6,'x']/scaler)*scaler), cex.axis=1.5)

df1[6:10,'x']<-0
barplot(df1[,'x'],main='tail, including hot spots',yaxt='n')
axis(2, at=c(0,8), cex.axis=1.5)
par(mfrow=c(1,1))
###################################

