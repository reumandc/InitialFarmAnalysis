#The model m is output of lm, and then we do various residual analyses
#
#The argument plotname is a math together with a precursor to prepend to several
#plot names (the code generates a few different plots). NA means use the default
#plotting device.
#
residanal<-function(m,lat,lon,plotname=NA)
{
  pm<-predict(m)
  rm<-residuals(m)
  
  #residuals against fitted values
  if (!is.na(plotname))
  {
    jpeg(filename=paste0(plotname,"_ResidVsPredicted.jpg"))
  }
  plot(pm,rm,type="p",xlab="Prediction",ylab="Residual")
  if (!is.na(plotname))
  {
    dev.off()    
  }

  #histogram and qq plot of residuals
  if (!is.na(plotname))
  {
    jpeg(filename=paste0(plotname,"_Hist.jpg"))
  }
  hist(rm,50,xlab="Residuals")
  if (!is.na(plotname))
  {
    dev.off()    
  }

  if (!is.na(plotname))
  {
    jpeg(filename=paste0(plotname,"_QQplot.jpg"))
  }
  qqnorm(rm)
  if (!is.na(plotname))
  {
    dev.off()    
  }

  #just map the residuals
  if (!is.na(plotname))
  {
    jpeg(filename=paste0(plotname,"_ResidMap.jpg"))
  }
  colmap_pos<-rgb(1,seq(from=1,to=0,length.out=100),seq(from=1,to=0,length.out=100))
  colmap_neg<-rgb(seq(from=0,to=1,length.out=100),seq(from=0,to=1,length.out=100),1)
  colmap<-c(colmap_neg[1:(length(colmap_neg)-1)],colmap_pos)
  ab<-max(abs(rm))
  colind<-round(198*(rm+ab)/(2*ab))+1
  cols<-colmap[colind]
  par(bg="gray")
  plot(lon,lat,col=cols,pch=20,cex=0.5)
  par(bg="white")
  if (!is.na(plotname))
  {
    dev.off()    
  }
  
  #Attempt 1 at spatial autocor
  #dist<-ncf::gcdist(lon,lat)
  #residdis<-outer(rm,rm,FUN=function(x,y){abs(x-y)})
  #plot(dist,residdis,type="p")
  #return(ecodist::MRM(as.dist(residdis)~as.dist(dist)))
  
  #Attempt 2 at spatial autocor
  #dist<-ncf::gcdist(lon,lat)
  #W<-1/dist
  #diag(W)<-0
  #return(spfilteR::MI.resid(rm,W=W))
  
  #Attempt 3
  if (!is.na(plotname))
  {
    jpeg(filename=paste0(plotname,"_SpatialAutocorr.jpg"))
  }
  sc<-ncf::spline.correlog(lon,lat,rm,latlon=TRUE,resamp=100)
  plot(sc)
  if (!is.na(plotname))
  {
    dev.off()    
  }
}
