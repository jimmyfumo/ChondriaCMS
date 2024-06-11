rm(list=ls())
setwd("/mnt/lustre/koa/scratch/jfumo/test")
df=read.csv("df.csv")
df$outcome=0
df$outcome[df$ExitCode.y=="-4"]=1
df$ReleasePOSIX=as.Date(df$ReleaseDate, origin = structure(-2440588, class = "Date"))
df$month=as.numeric(substr(df$ReleasePOSIX,6,7))
df$outcome[(which(df$SettlementLong>183.5 & df$outcome==1))]=0
df$outcome[(which(df$SettlementLong<182.1 & df$outcome==1))]=0
df$z[which(df$z>1)]=0
df$dir=df$dir+180
df$dir[df$dir>=360]=df$dir[df$dir>=360]-360



library(mgcv)
#library(mgcViz)
#library(lubridate)
options(bitmapType='cairo')

h=gam(outcome~s(Long,Lat)+s(Density)+s(Diameter)+s(Depth)+s(dir,z),data=df,family=binomial,select=T,na.action=na.fail,method="REML")

summary(h)
gam.check(h,type='response')
concurvity(h,full=T)

