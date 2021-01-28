source("Rsrc/settings.r")

nSites <- 7
siteInfo <- matrix(c(NA,NA,3,160,0,0,20,3,3,413,0.45,0.118),nSites,12,byrow = T)

initVarAll <- read.csv("inputs/initVarAll.csv",header = T, row.names = 1)
# initVarAll[5,] <- initVarAll[5,] * 2
# if(oldVer)
# siteInfo <- matrix(c(NA,NA,3,160,0,0,20,3,3),nSites,9,byrow = T)
# siteInfoP <- siteInfoSp <- siteInfo; siteInfoP[,8:9] <- siteInfoSp[,8:9] <- 1
initVarAll <- aperm(array(as.matrix(initVarAll),dim=c(7,3,7)),c(3,1,2))
                    
initVarP <- read.csv("inputs/initVarP.csv",header = T, row.names = 1)
initVarP <- aperm(array(as.matrix(initVarP),dim=c(7,1,7)),c(3,1,2))
initVarSp <- read.csv("inputs/initVarSp.csv",header = T, row.names = 1)
initVarSp <- aperm(array(as.matrix(initVarSp),dim=c(7,1,7)),c(3,1,2))

###proc data weather###
# load("coord.rdata")
# load("weatherInputs.rdata")
# PARtran <- PAR[transect,]
# TAirtran <- TAir[transect,]
# VPDtran <- VPD[transect,]
# Preciptran <- Precip[transect,]
# CO2tran <- CO2[transect,]
# save(PARtran,TAirtran,Preciptran,VPDtran,CO2tran,file="inputs/weatherTran.rdata")
load("inputs/weatherTran.rdata")
xx <- c((which(PARtran == -999)[1]-7):(which(PARtran == -999)[1]-1),
        (which(PARtran == -999)[7]+1):(which(PARtran == -999)[7]+7))
Preciptran[which(Preciptran == -999)] <- mean(Preciptran[xx])
VPDtran[which(VPDtran == -999)] <- mean(VPDtran[xx])
TAirtran[which(TAirtran == -999)] <- mean(TAirtran[xx])
PARtran[which(PARtran == -999)] <- mean(PARtran[xx])
# parsAWEN[,2] <- parsAWEN[,3] <- parsAWEN[,1]


nSites <- 7; nYears <- 150#*6
nYearsMS <- rep(nYears,nSites)
# siteInfo = matrix(c(1,1,3,160,0,0,20,3,3),nSites,9,byrow = T)
siteInfo[,2] <- siteInfo[,1] <- 1:nSites
siteInfoP <- siteInfoSp <- siteInfo; siteInfoP[,8:9] <- siteInfoSp[,8:9] <- 1
# initVarP <- array(c(1,10,1.5,0.5,0.0431969,0.,0.),dim=c(7,nSites,1))
# initVarSp <- array(c(2,10,1.5,0.5,0.0431969,0.,0.),dim=c(7,nSites,1))
# initVar <- matrix(c(2,10,1.5,0.5,0.0431969/3,0.,0.),7,3);initVar[1,] <- 1:3
# initVar[5,] <- c(0.0431969*0.45,0.0431969*0.45,0.0431969*0.1)
# initVarAll <- array(initVar,dim=c(7,3,nSites))
# initVarP <- aperm(initVarP,c(2,1,3))
# initVarSp <- aperm(initVarSp,c(2,1,3))
# initVarAll <- aperm(initVarAll,c(3,1,2))
litterSizeP <- array(0,dim=c(nSites,3,1))
litterSizeP[,2,] <- 2;litterSizeP[,1,] <- 30
litterSizeSp <- litterSizeP
litterSizeAll <- array(0,dim=c(nSites,3,3))
litterSizeAll[,1,1:2] <- 30
litterSizeAll[,1,3] <- 10
litterSizeAll[,2,] <- 2

###load parameters
# load("inputs/parameters.rdata")
nRep <- ceiling(nYears/(dim(PARtran)[2]/365))
# ptm <- proc.time()
initPrebasSp <- InitMultiSite(nYearsMS = rep(nYears,7),siteInfo=siteInfoSp,
                             # pCROBAS = pCROBAS,
                             # litterSize = litterSizeSp,pAWEN = parsAWEN,
                               # defaultThin=0,ClCut = 0.,
                               multiInitVar = as.array(initVarSp),
                               PAR = do.call(cbind, replicate(nRep, PARtran, simplify=FALSE)),
                               TAir=do.call(cbind, replicate(nRep, TAirtran, simplify=FALSE)),
                               VPD=do.call(cbind, replicate(nRep, VPDtran, simplify=FALSE)),
                               Precip=do.call(cbind, replicate(nRep, Preciptran, simplify=FALSE)),
                               CO2=do.call(cbind, replicate(nRep, CO2tran, simplify=FALSE)),
                               yassoRun = 1)#,lukeRuns = 1)

initPrebasP <- InitMultiSite(nYearsMS = rep(nYears,7),siteInfo=siteInfoP,
                              # pCROBAS = pCROBAS,
                              # litterSize = litterSizeP,pAWEN = parsAWEN,
                              # defaultThin=0,ClCut = 0.,
                              multiInitVar = as.array(initVarP),
                              PAR = cbind(PARtran,PARtran,PARtran,PARtran,PARtran,PARtran),
                              TAir=cbind(TAirtran,TAirtran,TAirtran,TAirtran,TAirtran,TAirtran),
                              VPD=cbind(VPDtran,VPDtran,VPDtran,VPDtran,VPDtran,VPDtran),
                              Precip=cbind(Preciptran,Preciptran,Preciptran,Preciptran,Preciptran,Preciptran),
                              CO2=cbind(CO2tran,CO2tran,CO2tran,CO2tran,CO2tran,CO2tran),
                              yassoRun = 1)#,lukeRuns = 1)

initPrebasAll <- InitMultiSite(nYearsMS = rep(nYears,7),siteInfo=siteInfo,
                              # pCROBAS = pCROBAS,
                              # litterSize = litterSizeAll,pAWEN = parsAWEN,
                              # defaultThin=0,ClCut = 0.,
                              multiInitVar = as.array(initVarAll),
                              PAR = cbind(PARtran,PARtran,PARtran,PARtran,PARtran,PARtran),
                              TAir=cbind(TAirtran,TAirtran,TAirtran,TAirtran,TAirtran,TAirtran),
                              VPD=cbind(VPDtran,VPDtran,VPDtran,VPDtran,VPDtran,VPDtran),
                              Precip=cbind(Preciptran,Preciptran,Preciptran,Preciptran,Preciptran,Preciptran),
                              CO2=cbind(CO2tran,CO2tran,CO2tran,CO2tran,CO2tran,CO2tran),
                              yassoRun = 1)#,lukeRuns = 1)

save(initPrebasAll,initPrebasP,initPrebasSp,file="inputs/initPreas.rdata")
