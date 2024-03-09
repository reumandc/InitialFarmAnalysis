#This is just for loading the data, organizing it as needed, and then saving in an R format. 
#I just load the data that already follows many steps of cleaning and assembly by Terry Kastens. 
#And also the VFF data. And I combined them.
#
#Reuman
#Begun 2/24

#**setup

rm(list=ls())
graphics.off()

datloc<-"../data/"
resloc<-"../data/"

if (!dir.exists(resloc))
{
  dir.create(resloc,recursive = TRUE)
}

#**Read in the Kastens csv and basic cleaning. I think the sheets to start with are the FieldByYear ones.

dK<-read.csv(file=paste0(datloc,"FieldLevelData_Kastens_04.02.21_FieldByYear_TopCropped.csv"))
dK[3]<-data.frame(rep("Kastens",dim(dK)[1]))
colnames(dK)[3]<-"Farm"
names(dK)[9]<-"OM.percent"
dK[[9]]<-as.numeric(dK[[9]])
names(dK)[22]<-"PercentGte0p1"
names(dK)[23]<-"PercentGte0p25"
names(dK)[24]<-"PercentGte0p5"
names(dK)[25]<-"PercentGte1"
dK[[26]]<-as.numeric(dK[[26]])
dK[[27]]<-as.numeric(dK[[27]])
dK[[28]]<-as.numeric(dK[[28]])
dK[[29]]<-as.numeric(gsub(",","",dK[[29]]))

summary(as.factor(dK$Prev.crop))

dim(unique(dK[,c(2,3,4,5,6,7,8)])) #272 unique values of the 7-tuple consisting of all information
#which one expects should not change through time ("Field.ID","Farm","Field","County","Acres","Latitude",
#"Longitude"). Call that the 7-ID.
length(unique(dK[,2])) #the same number of unique Field.ID pairs, so that's good
dim(unique(dK[,c(2,4)])) #the same number of unique Field.ID,Field pairs.
length(unique(dK$Field)) #The same number of unique Field names. 

#**Read in the VFF csv in a similar way

dV<-read.csv(file=paste0(datloc,"FieldLevelData_VFF_05.23.2021_FieldByYear_TopCropped.csv"))
names(dV)[9]<-"OM.percent"
names(dV)[22]<-"PercentGte0p1"
names(dV)[23]<-"PercentGte0p25"
names(dV)[24]<-"PercentGte0p5"
names(dV)[25]<-"PercentGte1"
dV[[26]]<-as.numeric(gsub(",","",dV[[26]]))
dV[[27]]<-as.numeric(gsub(",","",dV[[27]]))
dV[[28]]<-as.numeric(gsub(",","",dV[[28]]))
dV[[29]]<-as.numeric(gsub(",","",dV[[29]]))

summary(as.factor(dV$Prev.crop))
dV[dV$Prev.crop=="fallow","Prev.crop"]<-"Fallow"
dV[dV$Prev.crop=="milo","Prev.crop"]<-"Milo"
dV[dV$Prev.crop=="Wheta","Prev.crop"]<-"Wheat"
summary(as.factor(dV$Prev.crop))

dV7<-unique(dV[,c(2,3,4,5,6,7,8)])
dim(dV7) #298 rows with unique 7-ID
length(unique(dV[,2])) #297 unique Field.ID values, so figure out what field ID has 2 different 7-ID values
fID<-unique(dV[,2])
for (counter in 1:length(fID))
{
  inds<-which(dV7$Field.ID==fID[counter])
  if (length(inds)>1)
  {
    print(dV7[inds,])
  }
} #Field.ID 233 changes Acres, the two values are 18.0 and 18.2
dV[dV$Field.ID==233,] #Apparently the 18.0 value only happens once in 2020, so just set that value to 18.2.
dV[dV$Field.ID==233,"Acres"]<-18.2

dV7<-unique(dV[,c(2,3,4,5,6,7,8)])
dim(dV7) #Now there are 297 rows with unique 7-ID
length(unique(dV[,2])) #and still 297 unique values of Field.ID, so that's good

length(unique(dV7$Field)) #only 257 unique Field names, so figure this discrepancy between Field and Field.ID out
#This function lists all cases where Field.ID is different and Field is the same and management appears to be
#the same across the two Field.ID values.
GetTheProblems<-function()
{
  Fields<-unique(dV7$Field)
  
  TheProblems<-list()
  TPcount<-1
  for (counter in 1:length(Fields))
  {
    indsdV7<-which(dV7$Field==Fields[counter])
    if (length(indsdV7)>1) #only if there are multiple rows in dV7 with the same Field, it might be a problem
    {
      IDs<-unique(dV7[indsdV7,"Field.ID"])
      h1<-dV[dV$Field.ID==IDs[1],c("Year","Crop","Yield")]
      h1<-h1[order(h1$Year),]
      rownames(h1)<-NULL
      is_same<-TRUE #keeps track of whether we have detected differences between the management of different Field.IDs with the same Field value yet or not 
      for (IDcounter in 2:length(IDs))
      {
        h<-dV[dV$Field.ID==IDs[IDcounter],c("Year","Crop","Yield")]
        h<-h[order(h$Year),]
        rownames(h)<-NULL
        if (!setequal(h1,h))
        {
          is_same<-FALSE
        }
      }
      if (is_same) #it's only problematic case if the management has always been the same
      {
        TheProblems[[TPcount]]<-dV7[indsdV7,]
        TPcount<-TPcount+1
      }
    }
  }
  return(TheProblems)
}
TP<-GetTheProblems()
TP 
#So there is nothing left! So basically what we've discovered here is that, aside from Field.ID==233,
#which is addressed above, when Field.ID is different for multiple records with the same Field,
#what we have is two different fields managed differently with the same name. Probably cases where
#a field is split in half and managed differently. So basically this means we want to use Field.ID
#and ignore Field.

#**now combine the two datasets, which requires some checking first for compatibility of format

dim(dK)[2]==dim(dV)[2]
dim(dK)[2]
sum(names(dV)==names(dK)) #make sure the column names are the same

classK<-c()
classV<-c()
for (counter in 1:(dim(dK)[2]))
{
  classK<-c(classK,class(dK[[counter]]))
  classV<-c(classV,class(dV[[counter]]))
}
which(classK!=classV)
cbind(classK,classV) #make sure the column classes are the same

#make a unique field ID that applies in the combined dataset
dK$Farm.Field.ID<-paste("K",dK$Field.ID,sep=".")
dV$Farm.Field.ID<-paste("V",dV$Field.ID,sep=".")

d<-rbind(dK,dV)

#the yield column has some non-numeric info, add another column with the numeric conversion
d$YieldNum<-as.numeric(d$Yield)

saveRDS(d,file=paste0(resloc,"Data_1_RFormat.Rds"))

#**do various further consistency checks and get to know the data a bit

dim(d)

unique(d$Year) #all years 2013-2020

length(unique(d$Farm.Field.ID)) #569 fields

unique(d$County) #fields are across 13 counties
summary(as.factor(d$County)) #1568 in Rawlins and 1368 in Scott, but lots in other counties as well

sum(paste(d$Crop,d$Prev.crop,sep="")==d$Crop.Prev.crop)==dim(d)[1] #So Crop concatenated with Prev.crop really is Crop.Prev.crop

hist(d$Acres,50)

#**organize the data by management unit (MU) and add columns for info from 2 years ago

d_MU<-list()
MUs<-unique(d$Farm.Field.ID)
for (counter in 1:length(MUs))
{
  #extract the data for this MU and order it by year
  h<-d[d$Farm.Field.ID==MUs[counter],]
  h<-h[order(h$Year),]
  
  #error check
  if (!all.equal(h$Year,2013:2020))
  {
    stop("Error in ImportData.R: checkpoint 1")
  }
  
  #put in a column for crops two years ago
  h$Prev.prev.crop<-c(NA,h$Prev.crop[1:(dim(h)[1]-1)])
  
  #put in columns for last year's precip
  h$Prev.Preplant<-c(NA,h$Preplant[1:(dim(h)[1]-1)])
  h$Prev.In.season<-c(NA,h$In.season[1:(dim(h)[1]-1)])
  
  #put the resulting data subset in the receptacle list
  d_MU[[counter]]<-h
}
names(d_MU)<-MUs
saveRDS(d_MU,file=paste0(resloc,"Data_1p1_ByMUWithEarlierYears.Rds"))

#**cleanup

rm(list=ls())
graphics.off()
