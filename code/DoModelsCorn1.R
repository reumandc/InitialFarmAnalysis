#This is a more serious modelling effort than in ExploreCroppingRotations.R. In the end 
#what I focused on was constructing models of corn, generally, guided by Terry's models.
#
#Reuman
#Begun 2024 02 27

#**setup

rm(list=ls())
graphics.off()

library(lme4)
library(xtable)

datloc<-"../data/"
resloc<-"../results/DoModelsCorn1/"

if (!dir.exists(resloc))
{
  dir.create(resloc,recursive = TRUE)
}

#**functions

source("residanal.R")

#**read in the data

d<-readRDS(file=paste0(datloc,"Data_1p1_ByMUWithEarlierYears.Rds"))

#**do some prep for a corn analysis

#make a single data frame for all corn years in all fields

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

#**now do some linear regressions and associated plots for a corm analysis

#a base model, just precip effects
m1p1<-lm(formula=YieldNum~Preplant+In.season,data=d_corn)
summary(m1p1)
#residanal(m=m1p1,lat=d_corn$Latitude,lon=d_corn$Longitude)
#Expected signs of coefficients, adjusted Rsq of 0.1891.
#Resids look OK except for some spatial autocor. Might be
#different residuals for one of the farms than the other, 
#might want to add a predictor for that.
AIC(m1p1)

#Add V or K
m1p2<-lm(formula=YieldNum~Preplant+In.season+VorK,data=d_corn)
summary(m1p2)
#Adjusted Rsq of 0.2587, substantially higher
#residanal(m=m1p2,lat=d_corn$Latitude,lon=d_corn$Longitude)
#Resids look much better, though still spatial autocorr at short distances
anova(m1p1,m1p2)
#Yes, you need that new V or K predictor
AIC(m1p2)
#V gets better yields than K

#Add quadratic effects of precip
m1p3<-lm(formula=YieldNum~Preplant+PreplantSq+In.season+VorK,data=d_corn)
summary(m1p3)
#residanal(m1p3,lat=d_corn$Latitude,lon=d_corn$Longitude)
#Residuals still about as good as before.
anova(m1p2,m1p3) 
#Yes, you need the precip sq predictor
AIC(m1p3)
plot(d_corn$Preplant,coef(m1p3)[2]*d_corn$Preplant+coef(m1p3)[3]*d_corn$PreplantSq,type="p")
#basically the response of yield to Preplant water is increasing but saturating

#Add some more climate predictors that Terry's models include
m1p4<-lm(formula=YieldNum~Preplant+PreplantSq+In.season+VorK+C.GDD.1,data=d_corn)
summary(m1p4)
anova(m1p3,m1p4) 
AIC(m1p4)-AIC(m1p3)
#Not needed

#Add longitude
m1p5<-lm(formula=YieldNum~Preplant+PreplantSq+In.season+VorK+Longitude,data=d_corn)
summary(m1p5)
anova(m1p3,m1p5) 
AIC(m1p5)-AIC(m1p3)
#Not needed, really, if I'm only adding beefy improvements

#Add latitude
m1p6<-lm(formula=YieldNum~Preplant+PreplantSq+In.season+VorK+Latitude,data=d_corn)
summary(m1p6)
anova(m1p3,m1p6) 
#needed
AIC(m1p6)-AIC(m1p3)
#residanal(m1p6,lat=d_corn$Latitude,lon=d_corn$Longitude)
#Seems like a small improvement over m1p3 in short-distance spatial autocorr.
#But you can still see clustered residual values on the map.


#Now start a new line of models with previous-year-crop effects
m2p1<-lm(formula=YieldNum~Preplant+PreplantSq+In.season+VorK+Latitude+Prev.crop,data=d_corn)
summary(m2p1)
anova(m1p6,m2p1)
AIC(m2p1)-AIC(m1p6)
#much needed
#residanal(m2p1,lat=d_corn$Latitude,lon=d_corn$Longitude)
#residuals look about as good as before
summary(m2p1)
#This seems to say that Prev.crop=Fallow is statistically the same as corn, Milo is significantly
#better, wheat is marginally significantly worse, and NoFarm is statistically the same
plot(d_corn$Preplant,coef(m2p1)[2]*d_corn$Preplant+coef(m2p1)[3]*d_corn$PreplantSq,type="p")
#The highest water levels Preplant will reduce yields.

#Jump straight to a model motivated by Terry spreadsheets.
mTerryPlus<-lm(data=d_corn,
  formula=YieldNum~Preplant+PreplantSq+In.season+VorK+C.GDD.1+C.GDD.0.5+Prev.crop+PercentGte0p5+Latitude+Longitude+PlantDay)
summary(mTerryPlus)
anova(m2p1,mTerryPlus)
AIC(mTerryPlus)-AIC(m2p1)
#it's a big improvement
#residanal(mTerryPlus,lat=d_corn$Latitude,lon=d_corn$Longitude)
#Resids look pretty good. In fact, the short distance spatial autocorr appears slightly
#further reduced, tho you still see splotches on the map.
summary(mTerryPlus)
#You still get that Prev.crop=Fallow is the same as corn, Milo is better, NoFarm the same,
#wheat is worse.
#This is a bit hard to compare to Terry's result because he did V and K separately, and also
#he handled contrasts differently. If I wanted to compare the effects of Prev.crop systematically
#(and I should) then I should create models where various Prev.crop factor levels are combined
#and compare to models where they are not. 
#V still gets better yields than K
plot(d_corn$Preplant,coef(mTerryPlus)[2]*d_corn$Preplant+coef(mTerryPlus)[3]*d_corn$PreplantSq,type="p")
#The very highest Preplant rainfall still reduces yields a little. 


#Another, simpler analysis looking at V versus K
mVK<-lm(data=d_corn,formula=YieldNum~VorK)
summary(mVK)
#So it appears V gets higher yields than K
mean(d_corn$YieldNum[d_corn$VorK=="V"],na.rm=TRUE)
sd(d_corn$YieldNum[d_corn$VorK=="V"],na.rm=TRUE)
mean(d_corn$YieldNum[d_corn$VorK=="K"],na.rm=TRUE)
sd(d_corn$YieldNum[d_corn$VorK=="K"],na.rm=TRUE)
boxplot(formula=YieldNum~VorK,data=d_corn)
#Yep


#Now do an analysis using Prev.prev.crop, just to see if it might be useful later.
d_corn_w2013<-d_corn
d_corn<-d_corn[d_corn$Year>2013,] #throw out 2013 because missing data in the Prev.prev crop column
mTerryPlus<-lm(data=d_corn,
               formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
                 C.GDD.1+C.GDD.0.5+
                 Prev.crop+
                 PercentGte0p5+Latitude+Longitude+
                 PlantDay)
mLots<-lm(data=d_corn,
           formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
            C.GDD.1+C.GDD.0.5+
             Prev.crop+Prev.prev.crop+
             PercentGte0p5+Latitude+Longitude+
             PlantDay)
anova(mTerryPlus,mLots)
AIC(mLots)-AIC(mTerryPlus)
#a meaningful improvement
#residanal(mLots,lat=d_corn$Latitude,lon=d_corn$Longitude)
#Resids look pretty darn good
summary(mTerryPlus)
summary(mLots)
#V still better than K
#For Prev.crop: Fallow the same as corn, Milo better, NoFarm the same, Wheat worse
#For Prev.prev.crop: Fallow better than corn, Peas better, everything else the same
summary(as.factor(d_corn$Prev.prev.crop))
#Maybe I need to cross those two predictors in a stripped down model and see what crop 
#rotation for 2 years leading up to corn gives the best yields on corn.

#We can drop latitude now
mLots2<-lm(data=d_corn,
          formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
            C.GDD.1+C.GDD.0.5+
            Prev.crop+Prev.prev.crop+
            PercentGte0p5+Longitude+
            PlantDay)
anova(mLots2,mLots)
AIC(mLots2)-AIC(mLots)
#residanal(mLots2,lat=d_corn$Latitude,lon=d_corn$Longitude)
#Resids look pretty darn good
summary(mLots2)
#V still better than K
#For Prev.crop: Fallow the same as corn, Milo better, NoFarm the same, Wheat marginally worse
#For Prev.prev.crop: Fallow better than corn, Peas better, everything else the same
plot(d_corn$Preplant,coef(mLots2)[2]*d_corn$Preplant+coef(mLots2)[3]*d_corn$PreplantSq,type="p")
#The very highest Preplant rainfall still reduces yields, now more than before, though the highest
#values did not happen much.
#The signs of coefficients are pretty similar to Terry's results.


#Add a random effect for year to see if there are weather effects not accounted for
meLots2<-lmer(data=d_corn,
           formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
             C.GDD.1+C.GDD.0.5+
             Prev.crop+Prev.prev.crop+
             PercentGte0p5+Longitude+
             PlantDay+(1|Year),REML=FALSE)
AIC(meLots2)
AIC(mLots2)
AIC(meLots2)-AIC(mLots2)
1-pchisq(-2*as.numeric(logLik(mLots2)-logLik(meLots2)),2) #likelihood ratio test p values
#This seems to say we really do need that random effect! Perhaps not surprising.
summary(meLots2)
#But hard to interpret results because lmer is giving us outputs in a completely different way from lm,
#frustratingly. For instance, I cannot tell from these outputs anything like an R^2, and I cannot 
#tell anything about the importance of crop rotations. Later will have to figure out a way to extract that
#kind of info.
summary(mLots2)
1-sum((residuals(mLots2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2) #these are the same, good
#now try the same approach for the me model
1-sum((residuals(meLots2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2) 
#much higher in the me model than the lm model!
#residanal(m=meLots2,lat=d_corn$Latitude,lon=d_corn$Longitude)

#try a version with no explicit climate, just random effects by year
meNoClim<-lmer(data=d_corn,
               formula=YieldNum~VorK+
                 Prev.crop+Prev.prev.crop+
                 Longitude+
                 PlantDay+(1|Year),REML=FALSE)
AIC(meLots2)
AIC(meNoClim)
AIC(mLots2) #So the mixed effects model with a random effect for year and NO
#climate variables is better than the best lm, which includes lots of climate 
#variables. Though the model with both climate and random effects is best.
1-sum((residuals(meLots2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2) 
1-sum((residuals(meNoClim))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2) 
1-sum((residuals(mLots2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2) 
#And it also has a better R^2.
#So it looks like I can do better with a random effect for year than with actual
#climate variables. Best to use both, but not by a whole lot.
#
#This is the first time my R^2 values have gotten close to Terry's. Maybe Terry's 
#models actually have a random effect for year and it's just not clear from
#his spreadsheets? O/w will be important in a final analysis to find out why
#Terry's Rsq's are so high.
#residanal(m=meNoClim,lat=d_corn$Latitude,lon=d_corn$Longitude)
#Resids not bad, but some spatial autocorr at short distances, more than some of the 
#prior models.


#***Show this to Jude on 2024 03 04 then Terry - directly addresses the effects of previous-year crop
#***Do a comparison with one of the best models so far to look at the effects of the previous crop
#in a formal way.

d_corn<-d_corn_w2013

#do the top model
meLots2<-lmer(data=d_corn,
              formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
                C.GDD.1+C.GDD.0.5+
                Prev.crop+
                PercentGte0p5+Longitude+
                PlantDay+(1|Year),REML=FALSE)
residanal(m=meLots2,lat=d_corn$Latitude,lon=d_corn$Longitude,plotname=paste0(resloc,"Model_meLots2"))

#Put the Fallow and Corn treatments into one single category and seeing if the 
#resulting model is worse.
h<-d_corn$Prev.crop
h[h=="Corn" | h=="Fallow"]<-"CornOrFallow"
d_corn$Prev.crop.temp<-h
meLots2p1<-lmer(data=d_corn,
                formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
                  C.GDD.1+C.GDD.0.5+
                  Prev.crop.temp+
                  PercentGte0p5+Longitude+
                  PlantDay+(1|Year),REML=FALSE)

#anova
anres<-anova(meLots2,meLots2p1)
anres
saveRDS(anres,file=paste0(resloc,"ModelCompare_meLots2_CombineCornAndFallow_anova.Rds"))

#AIC comparison
AIC(meLots2)
AIC(meLots2p1)
AICres<-c(AIC(meLots2),AIC(meLots2p1))
saveRDS(AICres,file=paste0(resloc,"ModelCompare_meLots2_CombineCornAndFallow_AIC.Rds"))

#Rsq comparison
Rsqvals<-c(1-sum((residuals(meLots2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2),
           1-sum((residuals(meLots2p1))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2))
Rsqvals
saveRDS(Rsqvals,
        file=paste0(resloc,"ModelCompare_meLots2_CombineCornAndFallow_Rsq.Rds"))

#OK so it makes a significant diff but does it really make a meaningful diff? Look at effect sizes.
coef(meLots2)$Year
#Yes apparently it does - the coef for Fallow is about 28 higher than that for Corn,
#compared to...
quantile(d_corn$YieldNum,prob=c(0.025,0.25,0.5,0.75,0.975))
mean(d_corn$YieldNum)
#So Fallow gives a pretty big advantage. And seemingly better than Milo, Wheat, and NoFarm
print(xtable(coef(meLots2)$Year),file=paste0(resloc,"Model_meLots2_Coefs.tex"))
saveRDS(quantile(d_corn$YieldNum,prob=c(0.025,0.25,0.5,0.75,0.975)),
        file=paste0(resloc,"CornYieldQuantiles.Rds"))
saveRDS(mean(d_corn$YieldNum),
        file=paste0(resloc,"CornYieldMean.Rds"))

#According to the model coefficients above, the next best Prev.crop was Wheat, so let's see if 
#Fallow is significantly better than Wheat
h<-d_corn$Prev.crop
h[h=="Wheat" | h=="Fallow"]<-"WheatOrFallow"
d_corn$Prev.crop.temp<-h
meLots2p2<-lmer(data=d_corn,
                formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
                  C.GDD.1+C.GDD.0.5+
                  Prev.crop.temp+
                  PercentGte0p5+Longitude+
                  PlantDay+(1|Year),REML=FALSE)

#anova
anres<-anova(meLots2,meLots2p2)
anres
saveRDS(anres,file=paste0(resloc,"ModelCompare_meLots2_CombineWheatAndFallow_anova.Rds"))

#AIC comparison
AIC(meLots2)
AIC(meLots2p2)
AICres<-c(AIC(meLots2),AIC(meLots2p2))
saveRDS(AICres,file=paste0(resloc,"ModelCompare_meLots2_CombineWheatAndFallow_AIC.Rds"))

#Rsq comparison
Rsqvals<-c(1-sum((residuals(meLots2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2),
           1-sum((residuals(meLots2p2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2))
Rsqvals
saveRDS(Rsqvals,
        file=paste0(resloc,"ModelCompare_meLots2_CombineWheatAndFallow_Rsq.Rds"))

#Let's also see if Fallow is significantly better than Milo
h<-d_corn$Prev.crop
h[h=="Milo" | h=="Fallow"]<-"MiloOrFallow"
d_corn$Prev.crop.temp<-h
meLots2p3<-lmer(data=d_corn,
                formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
                  C.GDD.1+C.GDD.0.5+
                  Prev.crop.temp+
                  PercentGte0p5+Longitude+
                  PlantDay+(1|Year),REML=FALSE)

#anova
anres<-anova(meLots2,meLots2p3)
anres
saveRDS(anres,file=paste0(resloc,"ModelCompare_meLots2_CombineMiloAndFallow_anova.Rds"))

#AIC comparison
AIC(meLots2)
AIC(meLots2p3)
AICres<-c(AIC(meLots2),AIC(meLots2p3))
saveRDS(AICres,file=paste0(resloc,"ModelCompare_meLots2_CombineMiloAndFallow_AIC.Rds"))

#Rsq comparison
Rsqvals<-c(1-sum((residuals(meLots2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2),
           1-sum((residuals(meLots2p3))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2))
Rsqvals
saveRDS(Rsqvals,
        file=paste0(resloc,"ModelCompare_meLots2_CombineMiloAndFallow_Rsq.Rds"))

#Summary:
#1) Prev.crop=Fallow is better than Prev.crop=Corn for corn yields, by an average of 28 bushels/acre
#2) But Fallow is not significantly better than Wheat or Milo (and not close), which are, respectively,
#24.2 nd 20.9 better than corn.
#3) So, assuming you can get some meaningful profit from Milo or Wheat, you might as well plant
#those the year before Corn, rather than Fallow. 
#4) But remember there are only 10 field-years with Prev.crop=Fallow, so the differences in 2-3 
#above might emerge as significant if you had more Fallow years followed by Corn years. Remember
#the parameter point estimates say that corn yields ARE slightly higher when Prev.crop=Fallow
#compared to Prev.crop is Wheat or Milo, though it's not significant (and not close). Could be 
#because of few instances of Fallow->Corn. 


#***Show this to Jude on 2024 03 04 then Terry - examines using the best models so far whether V or K is
#getting better corn yields when controlling for everything else
#It looks like V gets higher yields than K. Test significance.
meLots2p5<-lmer(data=d_corn,
              formula=YieldNum~Preplant+PreplantSq+In.season+
                C.GDD.1+C.GDD.0.5+
                Prev.crop+
                PercentGte0p5+Longitude+
                PlantDay+(1|Year),REML=FALSE)

#anova results
anres<-anova(meLots2,meLots2p5)
anres
saveRDS(anres,file=paste0(resloc,"ModelCompare_meLots2_VvsK_anova.Rds"))

#AIC results
AIC(meLots2)
AIC(meLots2p5)
AICres<-c(AIC(meLots2),AIC(meLots2p5))
saveRDS(AICres,file=paste0(resloc,"ModelCompare_meLots2_VvsK_AIC.Rds"))

#Rsq results
Rsqvals<-c(1-sum((residuals(meLots2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2),
           1-sum((residuals(meLots2p5))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2))
Rsqvals
saveRDS(Rsqvals,
        file=paste0(resloc,"ModelCompare_meLots2_VvsK_Rsq.Rds"))

#Summary:
#1) V gets better corn yields than K, marginally significantly, when controlling for everything else
#2) You can see by how much by looking at the earlier coefficient table

#***Show this to Jude on 2024 03 04 then Terry - Similar to the above analyses, but I noticed by
#*#accident that everything gets more significant when you throw out 2013

d_corn<-d_corn[d_corn$Year>2013,]

#do the top model
meLots2<-lmer(data=d_corn,
              formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
                C.GDD.1+C.GDD.0.5+
                Prev.crop+
                PercentGte0p5+Longitude+
                PlantDay+(1|Year),REML=FALSE)
residanal(m=meLots2,lat=d_corn$Latitude,lon=d_corn$Longitude,plotname=paste0(resloc,"Model_meLots2n2013"))

#Put the Fallow and Corn treatments into one single category and seeing if the 
#resulting model is worse.
h<-d_corn$Prev.crop
h[h=="Corn" | h=="Fallow"]<-"CornOrFallow"
d_corn$Prev.crop.temp<-h
meLots2p1<-lmer(data=d_corn,
                formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
                  C.GDD.1+C.GDD.0.5+
                  Prev.crop.temp+
                  PercentGte0p5+Longitude+
                  PlantDay+(1|Year),REML=FALSE)

#anova
anres<-anova(meLots2,meLots2p1)
anres
saveRDS(anres,file=paste0(resloc,"ModelCompare_meLots2n2013_CombineCornAndFallow_anova.Rds"))

#AIC comparison
AIC(meLots2)
AIC(meLots2p1)
AICres<-c(AIC(meLots2),AIC(meLots2p1))
saveRDS(AICres,file=paste0(resloc,"ModelCompare_meLots2n2013_CombineCornAndFallow_AIC.Rds"))

#Rsq comparison
Rsqvals<-c(1-sum((residuals(meLots2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2),
           1-sum((residuals(meLots2p1))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2))
Rsqvals
saveRDS(Rsqvals,
        file=paste0(resloc,"ModelCompare_meLots2n2013_CombineCornAndFallow_Rsq.Rds"))

#OK so it makes a significant diff but does it really make a meaningful diff? Look at effect sizes.
coef(meLots2)$Year
#Yes apparently it does - the coef for Fallow is about 33 higher than that for Corn,
#compared to...
quantile(d_corn$YieldNum,prob=c(0.025,0.25,0.5,0.75,0.975))
mean(d_corn$YieldNum)
#So Fallow gives a pretty big advantage. And seemingly better than Milo, Wheat, and NoFarm
print(xtable(coef(meLots2)$Year),file=paste0(resloc,"Model_meLots2n2013_Coefs.tex"))
saveRDS(quantile(d_corn$YieldNum,prob=c(0.025,0.25,0.5,0.75,0.975)),
        file=paste0(resloc,"CornYieldQuantiles_n2013.Rds"))
saveRDS(mean(d_corn$YieldNum),
        file=paste0(resloc,"CornYieldMean_n2013.Rds"))

#According to the model coefficients above, the next best Prev.crop was Wheat, so let's see if 
#Fallow is significantly better than Wheat
h<-d_corn$Prev.crop
h[h=="Wheat" | h=="Fallow"]<-"WheatOrFallow"
d_corn$Prev.crop.temp<-h
meLots2p2<-lmer(data=d_corn,
                formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
                  C.GDD.1+C.GDD.0.5+
                  Prev.crop.temp+
                  PercentGte0p5+Longitude+
                  PlantDay+(1|Year),REML=FALSE)

#anova
anres<-anova(meLots2,meLots2p2)
anres
saveRDS(anres,file=paste0(resloc,"ModelCompare_meLots2n2013_CombineWheatAndFallow_anova.Rds"))

#AIC comparison
AIC(meLots2)
AIC(meLots2p2)
AICres<-c(AIC(meLots2),AIC(meLots2p2))
saveRDS(AICres,file=paste0(resloc,"ModelCompare_meLots2n2013_CombineWheatAndFallow_AIC.Rds"))

#Rsq comparison
Rsqvals<-c(1-sum((residuals(meLots2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2),
           1-sum((residuals(meLots2p2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2))
Rsqvals
saveRDS(Rsqvals,
        file=paste0(resloc,"ModelCompare_meLots2n2013_CombineWheatAndFallow_Rsq.Rds"))

#Let's also see if Fallow is significantly better than Milo
h<-d_corn$Prev.crop
h[h=="Milo" | h=="Fallow"]<-"MiloOrFallow"
d_corn$Prev.crop.temp<-h
meLots2p3<-lmer(data=d_corn,
                formula=YieldNum~Preplant+PreplantSq+In.season+VorK+
                  C.GDD.1+C.GDD.0.5+
                  Prev.crop.temp+
                  PercentGte0p5+Longitude+
                  PlantDay+(1|Year),REML=FALSE)

#anova
anres<-anova(meLots2,meLots2p3)
anres
saveRDS(anres,file=paste0(resloc,"ModelCompare_meLots2n2013_CombineMiloAndFallow_anova.Rds"))

#AIC comparison
AIC(meLots2)
AIC(meLots2p3)
AICres<-c(AIC(meLots2),AIC(meLots2p3))
saveRDS(AICres,file=paste0(resloc,"ModelCompare_meLots2n2013_CombineMiloAndFallow_AIC.Rds"))

#Rsq comparison
Rsqvals<-c(1-sum((residuals(meLots2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2),
           1-sum((residuals(meLots2p3))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2))
Rsqvals
saveRDS(Rsqvals,
        file=paste0(resloc,"ModelCompare_meLots2n2013_CombineMiloAndFallow_Rsq.Rds"))

#Summary (same as before, but some results are more significant):
#1) Prev.crop=Fallow is better than Prev.crop=Corn for corn yields, by an average of 33 bushels/acre
#2) But Fallow is not significantly better than Wheat or Milo (and not close), which are, respectively,
#25.6 nd 20.7 better than corn.
#3) So, assuming you can get some meaningful profit from Milo or Wheat, you might as well plant
#those the year before Corn, rather than Fallow. 
#4) But remember there are only 10 field-years with Prev.crop=Fallow, so the differences in 2-3 
#above might emerge as significant if you had more Fallow years followed by Corn years. Remember
#the parameter point estimates say that corn yields ARE slightly higher when Prev.crop=Fallow
#compared to Prev.crop is Wheat or Milo, though it's not significant (and not close). Could be 
#because of few instances of Fallow->Corn. 


#***Show this to Jude on 2024 03 04 then Terry - examines using the best models so far whether V or K is
#getting better corn yields when controlling for everything else
#It looks like V gets higher yields than K. Test significance.
#Much like an above analysis but without 2013
meLots2p5<-lmer(data=d_corn,
                formula=YieldNum~Preplant+PreplantSq+In.season+
                  C.GDD.1+C.GDD.0.5+
                  Prev.crop+
                  PercentGte0p5+Longitude+
                  PlantDay+(1|Year),REML=FALSE)

#anova results
anres<-anova(meLots2,meLots2p5)
anres
saveRDS(anres,file=paste0(resloc,"ModelCompare_meLots2n2013_VvsK_anova.Rds"))

#AIC results
AIC(meLots2)
AIC(meLots2p5)
AICres<-c(AIC(meLots2),AIC(meLots2p5))
saveRDS(AICres,file=paste0(resloc,"ModelCompare_meLots2n2013_VvsK_AIC.Rds"))

#Rsq results
Rsqvals<-c(1-sum((residuals(meLots2))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2),
           1-sum((residuals(meLots2p5))^2)/sum((d_corn$YieldNum-mean(d_corn$YieldNum))^2))
Rsqvals
saveRDS(Rsqvals,
        file=paste0(resloc,"ModelCompare_meLots2n2013_VvsK_Rsq.Rds"))

#Summary:
#1) V gets better corn yields than K, marginally significantly, when controlling for everything else
#2) You can see by how much by looking at the earlier coefficient table.


#OK, overall, the difference of exclusing 2013 or not is not really big or meaningful. Might as well
#leave it in when you present results.


#Thoughts for further work
#1) Use random year effects to argue that we can add climate and other predictors to our lms? 
#What would we try? This might be the old problem of NAO being better than any specific measure
#thing, so that it is hard to actually find what specific measure to use because it can be anything,
#and different things in different years. 
#2) Dig into 3-year crop rotations, and perhaps year-lagged precip and temp effects, if any?
#3) Why are the R^2 values of Terry so much higher than mine are when I don't add a random effect
#for year? Terry's spreadsheet does not explicitly indicate a random effect for year (or any random 
#effects, but perhaps they are there?)
#4) Can maps of residuals and related maps help us understand poor-performing individual fields?
#5) By using these and/or related methods, could we make field-level yield predictions based on what
#was planted there in previous years, and other covariates which are known at the time the predictions
#are needed? The idea would be to have a model like this for corn, for wheat, for other stuff,
#and then based on past planting history and maybe pre-season weather you could say "if you
#plant X then your yield pdf looks like this, if you plant Y it looks like this," individually by field.
#If we could do that then it's another product our company could sell. The farmers could use it
#to decide what to plant. Maybe a Bayesian approach
#to all this would be most appropriate if we are making conditional predictions? We could maybe
#even sell a piece of software where the farmer can try scenarios about future events, e.g.,
#they could play around and test things like "if I plant wheat and if it rains X amount between
#date d1 and date d2, then what is my yield distribution predicted to be?"
#5.5) Q for Terry. Does he already make decisions in that way, somehow? Suppose we had rolling, 
#field-specific yield predictions. For pre-season, it would be based on past crops in the field, 
#and last year's weather, (probably I should also incorporate previous year's yield of the same
#crop into my predictions), etc. The farmer would make yield predictions conditional on what they decide 
#to plant, and this would assist in their decisions.
#After planting, the yield predictions would use NDVI as well as all the stuff that was used for
#pre-planting predictions. 
#6) If we did this, then we would not just be making predictions based on the data of the individual
#farmer, we'd want to make predictions based on all the farms in the area which are using similar farming methods. 
#Which raises issues of data ownership and extent of comparability across farms.
#7) An interesting aspect of this is, a lot of these data go through the same choke point, which
#is the combine company - all the yield data, crop planted, etc., all goes through their systems. 
#And then you can easily get the climate data to layer on top of that. Imagine having not just the data
#from a few farms, but the data from all the combines across the country.
#8) If that data is available real time, then the combine company knows how much corn (say) is
#being harvested real time. If they can process that quickly then they can take a really strong
#position in corn (say) futures markets, because they would have very early data on yield. 
#Wow, it seems like that could be worth a lot? Though we ourselves might be able to do as much
#if our models are good and we are just trying to predict yield a few weeks out? Dunno. 
#8) Another way forward may be to sell our services directly to John Deer or something? Pitch this
#to them? Better might (or might not) be to build it a bit with the hope of maybe selling to them
#in a few years. Or selling to Monsanto or something? If we pitched it to them and they liked it
#that does not necessarily mean they would pay me to do it. They might just do it themselves.
#So there is some risk here coming from the fact that all the data I am using here is from the 
#same company or is easily obtainable. Does John Deer have data analysts, or do they just see themselves
#as in the business of selling equipment? What do they currently do with the huge amount of data
#they take in?
#9) And there is always the risk they are already working on stuff like this. Their advantage is,
#they have all the data since their combines are producing it! 
#10) Here is an out-there idea. Suppose we had field level data like Jude makes and suppose Monsanto made
#different varieties with different degrees of water tolerance and suppose someone invented a planter 
#which could plant different varieties in different parts of the field easily. Then you could plant
#water tolerant varieties in the playas, for instance, and the regular variety everywhere else.
#11) None of the models developed in this document uses any NDVI data or the other types of
#data Jude uses to predict yields. We are predicting kinda from a different vantage point. Is there 
#mileage in combing the two approaches? For instance, I bet Jude's predictions do not use prior
#planting history, which my models show can be quite important. I also want to see whether lagged
#weather effects are a thing or not.

#Additional thoughts emailed to myself 2024 03 03
#Is more of the remaining residual variation happening between fields or within fields?
#  
#In other words, is the remaining residual variation about bad fields versus good ones and that’s it? Or is it mostly remaining temporal variation (seems less likely because I’ve added a random year effect, though it would be good to try to explain that mechanistixally). Or is there an interaction between year and field? 
#  
#This kind of investigation might help narrow down what to add to the model 
#
#I can imagine that some low-lying fields would do better is medium-rain years and some high-lying fields might do better in much-rain years. Likewise with aspect. 
#
#We already know playas produce such an effect. 
#
#So maybe add that stuff?
#  
#Would maybe have the added advantage of providing info on what fields to acquire? 
  
