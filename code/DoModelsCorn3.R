#The goal here is to see if there are random effects by field on corn yield.
#
#Reuman
#2024 03 03

#**setup

rm(list=ls())
graphics.off()

library(lme4)

datloc<-"../data/"
resloc<-"../results/DoModelsCorn3/"

if (!dir.exists(resloc))
{
  dir.create(resloc,recursive = TRUE)
}

#**functions

source("residanal.R")

#**read in the data

d<-readRDS(file=paste0(datloc,"Data_1p1_ByMUWithEarlierYears.Rds"))

#**prep for a corn analysis

#remove the fields for which corn was only grown once

goodinds<-c()
for (counter in 1:length(d))
{
  if (sum(d[[counter]]$Crop=="Corn")>=2)
  {
    goodinds<-c(goodinds,counter)
  }
}
d<-d[goodinds]

#make a single data frame for all corn years in all remaining fields

d_corn<-d[[1]][d[[1]]$Crop=="Corn",]
for (counter in 2:length(d))
{
  d_corn<-rbind(d_corn,d[[counter]][d[[counter]]$Crop=="Corn",])
}
dim(d_corn)

saveRDS(d_corn,paste0(resloc,"CornData.Rds"))

#take out the singletons

summary(as.factor(d_corn$Prev.crop))
d_corn<-d_corn[d_corn$Prev.crop %in% c("Corn","Fallow","Milo","NoFarm","Wheat"),]
summary(as.factor(d_corn$Prev.crop))

#add a couple things you need

d_corn$PreplantSq<-d_corn$Preplant^2

h<-as.POSIXlt(d_corn$Plant.date,format = "%m/%d/%Y")
d_corn$PlantDay<-h$yday

h<-d_corn$Farm.Field.ID
h2<-sapply(FUN=function(x){strsplit(x,".",TRUE)[[1]][1]},X=as.list(h))
d_corn$VorK<-as.factor(h2)

#take out the Yield NAs

d_corn<-d_corn[!is.na(d_corn$YieldNum),]

#**now do some linear regressions to try to answer the question of whether there is a random
#effect for field

#start by fitting one of your best models from prior analyses

meLots2<-lmer(data=d_corn,
              formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
                C.GDD.1+C.GDD.0.5+
                Prev.crop+
                PercentGte0p5+Longitude+
                PlantDay+(1|Year),REML=FALSE)
residanal(m=meLots2,lat=d_corn$Latitude,lon=d_corn$Longitude) #there may be some spatial autocorr remaining, but let's not worry about it
1-sum((residuals(meLots2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2) #0.668, wow!

#now add random effects for field

meLots2p1<-lmer(data=d_corn,
              formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
                C.GDD.1+C.GDD.0.5+
                Prev.crop+
                PercentGte0p5+Longitude+
                PlantDay+(1|Year)+(1|Farm.Field.ID),REML=FALSE)
anova(meLots2,meLots2p1) #very highly significant
1-sum((residuals(meLots2p1))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2) #0.788, wow wow!
residanal(m=meLots2p1,lat=d_corn$Latitude,lon=d_corn$Longitude) #spatial autocorr pretty much all tanekn care of
h<-coef(meLots2p1)
names(h)
dim(h$Farm.Field.ID)
dim(d_corn)
head(h$Farm.Field.ID)
h$Year

saveRDS(meLots2p1,file=paste0(resloc,"meLots2p1.Rds"))

#**explore some consequences

#examine the year random effects

h$Year
hist(h$Year[[1]])
diff(range(h$Year[[1]])) #So year effects make a massive difference, most of yield
plot(as.numeric(rownames(h$Year)),h$Year[[1]],type="l") #Plot of quality of the year for corn yields, controlling for 
#everything else. 2017 was best, 2013 was worst.

#examine the field random effects

head(h$Farm.Field.ID)
#dig out the lat and lon of all these fields and 
pd<-data.frame(Farm.Field.ID=rownames(h$Farm.Field.ID),Intercept=h$Farm.Field.ID[[1]],Lat=NA,Lon=NA)
for (counter in 1:(dim(pd)[1]))
{
  inds<-which(d_corn$Farm.Field.ID==pd[counter,"Farm.Field.ID"])
  pd[counter,"Lat"]<-d_corn[inds[1],"Latitude"]
  pd[counter,"Lon"]<-d_corn[inds[1],"Longitude"]
}

diff(range(pd$Intercept)) #So the field random effects can range by 49, which is really big!
#Seems like some of these fields are maybe not worth the inputs?
rm<-pd$Intercept
rm<-rm-mean(rm)
hist(rm) #What if we identified the best and worst fields (after controlling for everything else) 
#for a farmer this way? Would that be useful?

#go ahead and map how good/bad a field is
colmap_pos<-rgb(1,seq(from=1,to=0,length.out=100),seq(from=1,to=0,length.out=100))
colmap_neg<-rgb(seq(from=0,to=1,length.out=100),seq(from=0,to=1,length.out=100),1)
colmap<-c(colmap_neg[1:(length(colmap_neg)-1)],colmap_pos)
ab<-max(abs(rm))
colind<-round(198*(rm+ab)/(2*ab))+1
cols<-colmap[colind]
par(bg="gray")
plot(pd$Lon,pd$Lat,col=cols,pch=20,cex=0.5)
par(bg="white")
#A lot of time the shittiest fields are right next to fields that are good.
#We could easily supply a table showing the names and locations and ID of the 10%
#worst fields. 




