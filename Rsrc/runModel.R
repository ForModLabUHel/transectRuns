library(Rprebasso) 
setwd("~/research/PREBAS/Rprebas_examples/transectRun")
source("utils.r")
nSites=7
# source("procData.r")
load("inputs/initPreas.rdata")
# initPrebasAll$pCROBAS[17,] <- 2000
# initPrebasSp$pCROBAS[17,] <- 2000
# initPrebasP$pCROBAS[17,] <- 2000

# initPrebasAll$pPRELES[5] <- 0.95
# initPrebasSp$pPRELES[5] <- 0.95
# initPrebasP$pPRELES[5] <- 0.95
# initPrebasAll$pCROBAS <- pCROB
###change parameters
# pars <- read.csv("inputs/parameters.csv",header=T)
# initPrebasAll$pCROBAS <- initPrebasP$pCROBAS <- initPrebasSp$pCROBAS <- as.matrix(pars[,2:4])

# pX <- pCROB; pX[12,] <- pX[12,] -1
# pX[c(5,6,8,9),2] <- c(9.7,2.5,0.202,0.337)
# 
# # #####old parameters for new version
# pX <- pCROB; pX[12,] <- pCROB[12,]-1
# pX[31,] <- 0.
# pX[c(8,9),1] <- c(0.4,0.5)
# pX[c(8,9),2] <- c(0.4,0.5)
# pX[c(8,9),3] <- c(0.4,0.5)
# pX[21,1] <- c(0.4) #alfar1 pine
# pX[22,1] <- c(0.44) #alfar2 pine
# alfar3                      0.47000000  3.800000e-01    0.64000000
# alfar4                      0.64000000  4.800000e-01    0.75000000
# alfar5                      0.84000000  5.800000e-01    0.94000000
# #####old parameters for new version END

# initPrebasAll$pCROBAS <- initPrebasP$pCROBAS <- initPrebasSp$pCROBAS <- pX

# change site type
siteType = 3
initPrebasP$siteInfo[,3] <- initPrebasSp$siteInfo[,3] <- initPrebasAll$siteInfo[,3] <- siteType 

system.time(spruceRun <- multiPrebas(initPrebasSp))
system.time(pineRun <- multiPrebas(initPrebasP))
system.time(allSpRun <- regionPrebas(initPrebasAll))



makePlotXage <- function(out,varX,siteX){
  nLay <- dim(out$multiOut)[4]
  layers <- paste0("layer",1:nLay)
  yrange <- range(out$multiOut[siteX,,varX,,1])
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

siteX=6
varX=13
makePlot(spruceRun,varX,siteX)  

# plot stad charactheritsics
par(mfrow=c(2,3))
for(varX in c(11:13,17:18,30)){
  makePlot(allSpRun,varX,siteX)  
}
# plot biomasses
par(mfrow=c(2,3))
for(varX in c(24:25,31:33,43)){
  makePlot(allSpRun,varX,siteX)  
}


# Plot pine

# plot stad charactheritsics
par(mfrow=c(2,3))
for(varX in c(11:13,30,17:18)){
  makePlot(pineRun,varX,siteX)  
}
# plot biomasses
par(mfrow=c(2,3))
for(varX in c(24:25,31:33,43)){
  makePlot(pineRun,varX,siteX)  
}



# Plot spruce
# plot stad charactheritsics
par(mfrow=c(2,3))
for(varX in c(11:14,17:18)){
  makePlot(spruceRun,varX,siteX)  
}
# plot biomasses
par(mfrow=c(2,3))
for(varX in c(24:25,31:33,43)){
  makePlot(spruceRun,varX,siteX)  
}



par(mfrow=c(2,3))
for(varX in c(11:14,17:18)){
  makePlotXage(spruceRun,varX,siteX)  
}
