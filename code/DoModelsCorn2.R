#This is a more serious modelling effort than in ExploreCroppingRotations.R, and also 
#follows from DoModelsCorn1.R, but takes things in a slightly different direction. 
#The goal here is to look at lagged effects of weather, and also cropping choices made
#in years prior to the previous year. 
#
#Reuman
#Begun 2024 02 28

#**setup

rm(list=ls())
graphics.off()

library(lme4)

datloc<-"../results/DoModels/"
resloc<-"../results/DoModels2/"

if (!dir.exists(resloc))
{
  dir.create(resloc,recursive = TRUE)
}

#**functions

source("residanal.R")

#**read in the data, it's the corn-specific data frame, since this is a corn analsis

d_corn<-readRDS(file=paste0(datloc,"CornData.Rds"))

#**do various analysis-specific cleaning

#take out year 2013 because it does not have the earlier year data

dim(d_corn)
d_corn<-d_corn[d_corn$Year!=2013,]
dim(d_corn)

#take out rare Prev.crop values

summary(as.factor(d_corn$Prev.crop))
d_corn<-d_corn[d_corn$Prev.crop %in% c("Corn","Fallow","Milo","NoFarm","Wheat"),]
summary(as.factor(d_corn$Prev.crop))
dim(d_corn)

#take out rare Prev.prev.crop values

summary(as.factor(d_corn$Prev.prev.crop))
#they are actually all OK

#add a couple things you need

d_corn$PreplantSq<-d_corn$Preplant^2

h<-as.POSIXlt(d_corn$Plant.date,format = "%m/%d/%Y")
d_corn$PlantDay<-h$yday

h<-d_corn$Farm.Field.ID
h2<-sapply(FUN=function(x){strsplit(x,".",TRUE)[[1]][1]},X=as.list(h))
d_corn$VorK<-as.factor(h2)

#take out the Yield NAs

d_corn<-d_corn[!is.na(d_corn$YieldNum),]
dim(d_corn)

#add a combined variable for last two years crop

d_corn$TwoYears.crop<-paste(d_corn$Prev.crop,d_corn$Prev.prev.crop,sep="")
h<-summary(as.factor(d_corn$TwoYears.crop))
h
for (counter in 1:length(h))
{
  if (h[counter]<10)
  {
    d_corn<-d_corn[d_corn$TwoYears.crop!=names(h)[counter],]
  }
}
dim(d_corn)
summary(as.factor(d_corn$TwoYears.crop))

#**now do some linear models, motivated by those at the end of DoModelsCorn1.R

m1p1<-lmer(data=d_corn,
               formula=YieldNum~VorK+
                 Prev.crop+
                 Longitude+
                 PlantDay+(1|Year),REML=FALSE)
m1p2<-lmer(data=d_corn,
         formula=YieldNum~VorK+
           TwoYears.crop+
           Longitude+
           PlantDay+(1|Year),REML=FALSE)
AIC(m1p1)
AIC(m1p2) #so this one is lower/better
anova(m1p1,m1p2) #highly significant
1-sum((residuals(m1p1))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2) 
1-sum((residuals(m1p2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2) #actually only explains a bit more variation


m2p1<-lmer(data=d_corn,
           formula=YieldNum~VorK+
             Prev.crop+
             Longitude+
             Preplant+
             PlantDay+(1|Year),REML=FALSE)
m2p2<-lmer(data=d_corn,
           formula=YieldNum~VorK+
             TwoYears.crop+
             Longitude+
             Preplant+
             Prev.Preplant+
             Prev.In.season+
             PlantDay+(1|Year),REML=FALSE)
AIC(m2p1)
AIC(m2p2) #so this one is lower/better
anova(m2p1,m2p2) #highly significant
1-sum((residuals(m2p1))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2) 
1-sum((residuals(m2p2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2) #actually only explains a bit more variation


#Actually, these R^2 comparisons might not be the right ones because I think they use
#residuals after random year effects, and we would not know those random year effects. 

#I need to mull this for a while so stop working on it for now.
