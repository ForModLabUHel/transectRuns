fAparPlot <- function(out,siteX){
  fapars <- data.table(value=c(out$fAPAR[siteX,],out$GVout[siteX,,1],
                               out$fAPAR[siteX,]+out$GVout[siteX,,1]),
                       year=1:length(out$fAPAR[siteX,]),
                       fAPAR=c(rep(c("stand","gv","tot"),each=length(out$fAPAR[siteX,]))))
  
  pX <- ggplot(fapars,mapping = aes(x=year,y=value,col=fAPAR))+
    geom_line()
}


nwLitPlot <- function(out,siteX){
  nwlit_trees <- apply(out$multiOut[siteX,,26:27,,1],1,sum)
  nwlit_gv <- out$GVout[siteX,,2]
  nwlit <- nwlit_gv + nwlit_trees
  
  lit <- data.table(value=c(nwlit_trees,nwlit_gv,nwlit),
                    year=1:length(nwlit),
                    litter=c(rep(c("trees","gv","tot"),
                                 each=length(nwlit))))
  
  pX <- ggplot(lit,mapping = aes(x=year,y=value,col=litter))+
    geom_line()
}


photoPlot <- function(out,siteX){
  if(length(dim(out$multiOut[siteX,,44,,1]))>0){
    stand <- apply(out$multiOut[siteX,,44,,1],1,sum)
  }else{
    stand <- out$multiOut[siteX,,44,,1]
  }
  gv <- out$GVout[siteX,,3]
  tot <- stand + gv
  
  tab <- data.table(value=c(stand,gv,tot),
                    year=1:length(stand),
                    vegX=c(rep(c("stand","gv","tot"),
                               each=length(stand))))
  
  pX <- ggplot(tab,mapping = aes(x=year,y=value,col=vegX))+
    geom_line()
}

resPlot <- function(out,siteX){
  if(length(dim(out$multiOut[siteX,,9,,1]))>0){
    stand <- apply(out$multiOut[siteX,,9,,1],1,sum)
  }else{
    stand <- out$multiOut[siteX,,9,,1]
  }
  gv <- out$GVout[siteX,,3] *0.5
  tot <- stand + gv
  
  tab <- data.table(value=c(stand,gv,tot),
                    year=1:length(stand),
                    vegX=c(rep(c("stand","gv","tot"),
                               each=length(stand))))
  
  pX <- ggplot(tab,mapping = aes(x=year,y=value,col=vegX))+
    geom_line()
}
neePlot <- function(out_gv,out_ngv,siteX){
  if(length(dim(out_gv$multiOut[siteX,,46,,1]))>0){
    nee_gv <- apply(out_gv$multiOut[siteX,,46,,1],1,sum)
  }else{
    nee_gv <- out_gv$multiOut[siteX,,46,,1]
  }
  if(length(dim(out_ngv$multiOut[siteX,,46,,1]))>0){
    nee_ngv <- apply(out_ngv$multiOut[siteX,,46,,1],1,sum)
  }else{
    nee_ngv <- out_ngv$multiOut[siteX,,46,,1]
  }
  
  tab <- data.table(value=c(nee_gv,nee_ngv),
                    year=1:length(nee_gv),
                    vegX=c(rep(c("nee_gv","nee_ngv"),
                               each=length(nee_gv))))
  
  pX <- ggplot(tab,mapping = aes(x=year,y=value,col=vegX))+
    geom_line()
}

makePlotXage <- function(out,varX,siteX,yrange){
  nLay <- dim(out$multiOut)[4]
  layers <- paste0("layer",1:nLay)
  # yrange <- range(out$multiOut[siteX,,varX,,1])
  plot(out$multiOut[siteX,,7,1,1],out$multiOut[siteX,,varX,1,1],type='l', col=1,ylab='',main=varNames[varX],ylim=yrange,xlab="age")
  if(nLay>1) for(i in 2:nLay) lines(out$multiOut[siteX,,7,i,1],out$multiOut[siteX,,varX,i,1],col=i)
}
makePlot <- function(out,varX,siteX){
  nLay <- dim(out$multiOut)[4]
  layers <- paste0("layer",1:nLay)
  yrange <- range(out$multiOut[siteX,,varX,,1])
  plot(out$multiOut[siteX,,varX,1,1],type='l', col=1,ylab='',main=varNames[varX],ylim=yrange)
  if(nLay>1) for(i in 2:nLay) lines(out$multiOut[siteX,,varX,i,1],col=i)
}



# test <- monthlyFluxes(modOut)
# plot(test$mNEP[1,1:1200],type='l')
# points(((1:max(modOut$nYears))*12-6),apply(modOut$multiOut[1,,46,,1]/12,1,sum),col=2,pch=20)
