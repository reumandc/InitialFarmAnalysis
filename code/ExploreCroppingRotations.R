#This is just an exploration of cropping rotations and effects on yield
#
#Reuman
#Begun 2024 02

#**setup

rm(list=ls())
graphics.off()

library(lme4)

datloc<-"../data/"
resloc<-"../results/ExploreCroppingRotations/"

if (!dir.exists(resloc))
{
  dir.create(resloc,recursive = TRUE)
}

#**read in the data

d<-readRDS(file=paste0(datloc,"Data_1_RFormat.Rds"))

#**organize the data by management unit (use Farm.Field.ID for that, see ImportData.R)

d_mu<-list()
mus<-unique(d$Farm.Field.ID)
for (counter in 1:length(mus))
{
  h<-d[d$Farm.Field.ID==mus[counter],]
  h<-h[order(h$Year),]
  d_mu[[counter]]<-h
}
names(d_mu)<-mus
saveRDS(d_mu,file=paste0(resloc,"Data_1p1_ByManagementUnit.Rds"))

#**Make a table that just has what each field is used for each year, just to look at

rots<-data.frame(Year=c(2013:2020))
for (counter in 1:length(d_mu))
{
  rots[[counter+1]]<-d_mu[[counter]]$Crop
  colnames(rots)[counter+1]<-mus[counter]
}

#**See if corn yields are higher following certain prior field uses than others, as a warmup,
#*#using just very simple models.

d2<-data.frame(MU="",CornYear=NA,PrevCrop="",CornYield=NA)
d2count<-1
for (mcount in 1:length(d_mu)) #go through each management unit (MU)
{
  h<-d_mu[[mcount]] #pull out the MU for easy access
  for (ycount in 1:(dim(h)[1])) #go through each year of the field
  {
    if (h$Crop[ycount]=="Corn") 
    {
      d2[d2count,"MU"]<-h$Farm.Field.ID[ycount]
      d2[d2count,"CornYear"]<-h$Year[ycount]
      d2[d2count,"PrevCrop"]<-h$Prev.crop[ycount]
      d2[d2count,"CornYield"]<-h$YieldNum[ycount]
      d2count<-d2count+1
    }
  }
}

dim(d2)
head(d2)

class(d2[[1]])
class(d2[[2]])
d2[[2]]<-as.factor(d2[[2]])
class(d2[[2]])
class(d2[[3]])
unique(d2[[3]])
summary(as.factor(d2[[3]]))
dim(d2)
d2<-d2[d2$PrevCrop %in% c("Corn","Fallow","Milo","NoFarm","Wheat"),]
dim(d2)
summary(as.factor(d2[[3]]))
d2[[3]]<-as.factor(d2[[3]])
class(d2[[3]])
class(d2[[4]])

#*start with a fixed effects model only
m1fixed<-lm(formula=CornYield~PrevCrop,data=d2)
summary(m1fixed) 
mean(d2$CornYield[d2$PrevCrop=="Corn"])
coef(m1fixed)[1]
mean(d2$CornYield[d2$PrevCrop=="Wheat"])
sum(coef(m1fixed)[c(1,5)])
mean(d2$CornYield[d2$PrevCrop=="Milo"])
sum(coef(m1fixed)[c(1,3)])
mean(d2$CornYield[d2$PrevCrop=="Fallow"]) 
sum(coef(m1fixed)[c(1,2)])
mean(d2$CornYield[d2$PrevCrop=="NoFarm"],na.rm=TRUE) 
sum(coef(m1fixed)[c(1,4)])

#So the "Intercept" value you get from summary is the mean yield when the previous year was corn.
#The others are *additional* yield beyond that that you get when the previous year was something else.
#Don't forget things are really unbalanced - only 10 times was corn grown following fallow.

#Q1: Is corn yield actually any better if the previous year was fallow, compared to milo or wheat?
#Q2: Is corn yield any better if the previous year was wheat compared to milo?
#Q3: Could ask a similar question about NoFarm, but not totally sure what that is. 
#Methods Q: How to best answer these questions. 

#**make a table

prev<-unique(d2$PrevCrop)
tabres<-data.frame(PrevCrop=prev,Mean=NA,SD=NA,Num=NA,Q2p5=NA,Q97p5=NA)
for (counter in 1:length(prev))
{
  h<-d2$CornYield[d2$PrevCrop==prev[counter]]
  tabres[counter,"Mean"]<-mean(h,na.rm=TRUE)
  tabres[counter,"SD"]<-sd(h,na.rm=TRUE)
  tabres[counter,"Num"]<-sum(!is.na(h))
  qs<-quantile(h,probs = c(0.025,0.975),na.rm=TRUE)
  tabres[counter,"Q2p5"]<-qs[1]
  tabres[counter,"Q97p5"]<-qs[2]
}
tabres
boxplot(formula=CornYield~PrevCrop,data=d2)

#Notes:
#
#It looks like basically growing corn the previous year gives a crappy yield but everything else is 
#about the same. You could test under the current framework whether there is any difference
#what you grow the previous year, or fallow, but anyway it's not much different. Instead of belaboring 
#that, press on, just having basically concluded that this analysis says growing corn the previous year
#lowers your corn yield, but growing milo, wheat, "NoFarm" or fallow in the previous year means you'll 
#have about the same corn yield. This analysis is not right, anyway, except as an approximation, because 
#the year effects are going to violate independence assumptions on which conclusions of significance would
#be based, if I were going to make such conclusions. So press on. 
#
#Based on Terry's note, the next move should def be to incorporate precip (and maybe other stuff at the 
#same time). 

#**now include a random effect for year

m1<-lmer(formula=CornYield~PrevCrop+(1|CornYear),data=d2)
summary(m1)
m1rest<-lmer(formula=CornYield~1+(1|CornYear),data=d2)
summary(m1rest)
anova(m1,m1rest) #seems to be saying the previous crop makes a significant difference
#Note: I got a warning saying "refitting model(s) with ML (instead of REML)" I have to review
#that stuff. But it seems like we can conclude previous crop matters. Duh.

summary(m1)

#Notes:
#Similar conclusions to the fixed effects model, you get what appear to be substantial advantages in corn yields
#if the previous crop was fallow or milo or NoFarm or wheat, but need to extract significance and also ask if
#it makes much difference which of those previous "crops" you planted. Fallow may have an advantage over Milo
#but not Wheat, but would need to do some significance testing, and anyway I think I want to have a model
#with precip and other stuff in it first. 
AIC(m1fixed)
AIC(m1) #quite a bit lower, if this comparison is fair

#***Now include precip



#DAN: Below is scrap from before, needs revision or deletion.

#We should probably add random effects for field.
#If some fields were only used once for corn I guess we'll have trouble with adding random effects for field.
numcorn<-data.frame(Field=unique(d2$Field),NumCorn=NA)
for (counter in 1:(dim(numcorn)[1]))
{
  numcorn[counter,"NumCorn"]<-sum(d2$Field==numcorn[counter,"Field"])
}
sum(numcorn$NumCorn==1) #so there are 50 fields in which corn was only grown once



m2<-lmer(fixed=CornYield~PrevCrop,data=d2)
