makePlot <- function(run,varX,siteX){
  nSp <- run$maxNlayers
  if(nSp==1){
    plot(run$multiOut[siteX,,varX,1,1],type='l',
         main=paste("site",siteX),ylab = varNames[varX])
  } else{
    ylimX=range(run$multiOut[siteX,,varX,,1],na.rm=T)
    plot(run$multiOut[siteX,,varX,1,1],type='l',ylim = ylimX,
         main=paste("site",siteX),ylab = varNames[varX])
    lines(run$multiOut[siteX,,varX,2,1],col=2)
    lines(run$multiOut[siteX,,varX,3,1],col=3)
    legend("right",c("pi","sp","bir"),col=1:3,lty=1,bg="transparent")
  }
  
}
