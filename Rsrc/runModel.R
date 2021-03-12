source("Rsrc/settings.r")
# setwd("~/research/PREBAS/Rprebas_examples/transectRun")
source("Rsrc/utils.r")
# source("procData.r")
load("inputs/initPreas.rdata")

nSites=initPrebasAll$nSites

###Create lists for outputs
prebRuns <- soilC <- list()



# change site type in loop
# GVrun=1
initPrebasP$GVrun <- initPrebasSp$GVrun <- initPrebasAll$GVrun <- 1

for(siteType in 1:5){
  # siteType = 3
  initPrebasP$siteInfo[,3] <- initPrebasSp$siteInfo[,3] <- initPrebasAll$siteInfo[,3] <- siteType 
  initPrebasP$multiInitVar[,2,] <- initPrebasSp$multiInitVar[,2,] <-
    initPrebasAll$multiInitVar[,2,] <- round(6 + 2* siteType - 0.005*rowMeans(initPrebasP$ETSy) + 2.25) 
  
  prebRuns$spruce_GV[[siteType]] <- multiPrebas(initPrebasSp)
  soilC$spruce_GV[[siteType]] <- stXX_GV(prebRuns$spruce_GV[[siteType]],GVrun=1)
  prebRuns$pine_GV[[siteType]] <- multiPrebas(initPrebasP)
  soilC$pine_GV[[siteType]] <- stXX_GV(prebRuns$pine_GV[[siteType]],GVrun=1)
  prebRuns$all_GV[[siteType]] <- multiPrebas(initPrebasAll)
  soilC$all_GV[[siteType]] <- stXX_GV(prebRuns$all_GV[[siteType]],GVrun=1)
  
  initPrebasSp$soilC[,1,,,] <- soilC$spruce_GV[[siteType]]
  prebRuns$spruce_GV[[siteType]] <- multiPrebas(initPrebasSp)
  initPrebasP$soilC[,1,,,] <- soilC$pine_GV[[siteType]]
  prebRuns$pine_GV[[siteType]] <- multiPrebas(initPrebasP)
  initPrebasAll$soilC[,1,,,] <- soilC$all_GV[[siteType]]
  prebRuns$all_GV[[siteType]] <- multiPrebas(initPrebasAll)
  
  print(siteType)
}


siteX=1
varX=11
siteType <- 3
modOut <- prebRuns$all_GV[[siteType]] 
testWeath1 <- monthlyFluxes(modOut,weatherOption = 1)
testWeath2 <- monthlyFluxes(modOut,weatherOption = 2)
testWeath3 <- monthlyFluxes(modOut,weatherOption = 3)

siteX=4
plot(testWeath1$soilC[siteX,])
points(testWeath2$soilC[siteX,],col=3)
points(testWeath3$soilC[siteX,],col=4)
points(seq(1,(150*12),by=12),modOut$soilCtot[siteX,],pch=20,col=2)


annualGPPsiteX <- apply(modOut$multiOut[siteX,,44,,1],1,sum) + modOut$GVout[siteX,,3]
plot(test$mGPP[siteX,1:120],type='l')
points(((1:120)*12-6),annualGPPsiteX[1:120]/12,pch=20,col=2)
plot(test$soilC[siteX,1:120],type='l')

dimnames(modOut$multiOut)


makePlot(modOut,varX,siteX)  



####Yield table example for Xianglin!
modOut <- prebRuns$pine_GV[[siteType]] 
par(mfrow=c(2,2))
varXs <- c(11,17,13,30)
yieldVars <- c(2,3,4,6)
source("Rsrc/compute.pine.growth.by.VV.R")
for(i in 1:length(varXs)){
  varX <- varXs[i]
  ylim=max(c(modOut$multiOut[siteX,,varX,1,1],
             VV.growth[,yieldVars[i]] ))
  makePlotXage(modOut,varX,siteX,yrange = c(0,ylim))  
  lines(VV.growth[,1],VV.growth[,yieldVars[i]],col=4)
}



###CUE
mean(prebRuns$pine[[siteType]]$multiOut[siteX,1:50,18,1,1])/
  mean(prebRuns$pine[[siteType]]$multiOut[siteX,1:50,44,1,1])
plot(prebRuns$pine[[1]]$multiOut[siteX,,18,1,1]/
       prebRuns$pine[[1]]$multiOut[siteX,,44,1,1])
points(prebRuns$pine[[2]]$multiOut[siteX,,18,1,1]/
         prebRuns$pine[[2]]$multiOut[siteX,,44,1,1],col=2)
points(prebRuns$pine[[3]]$multiOut[siteX,,18,1,1]/
         prebRuns$pine[[3]]$multiOut[siteX,,44,1,1],col=3)
points(prebRuns$pine[[4]]$multiOut[siteX,,18,1,1]/
         prebRuns$pine[[4]]$multiOut[siteX,,44,1,1],col=4)
points(prebRuns$pine[[5]]$multiOut[siteX,,18,1,1]/
         prebRuns$pine[[5]]$multiOut[siteX,,44,1,1],col=5)

# plot stad charactheritsics
par(mfrow=c(2,3))
for(varX in c(11:13,17:18,43)){
  makePlotXage(prebRuns$spruce[[siteType]],varX,siteX)  
}
# plot biomasses
par(mfrow=c(2,3))
for(varX in c(24:25,31:33,43)){
  makePlot(prebRuns$all[[siteType]],varX,siteX)  
}


# Plot pine
p_fAPAR <- p_Litter <- p_gpp <- p_Ra <- p_nee <- list()

for(i in 1:5){
  for(siteX in 1:7){
    stX <- paste0("st",i)
    site <- paste0("site",siteX)
    p_fAPAR$GV$spruce[[paste0("st",i)]][[paste0("site",siteX)]] <- fAparPlot(prebRuns$spruce_GV[[i]],siteX)
    p_fAPAR$GV$pine[[paste0("st",i)]][[paste0("site",siteX)]] <- fAparPlot(prebRuns$pine_GV[[i]],siteX)
    p_fAPAR$GV$all[[paste0("st",i)]][[paste0("site",siteX)]] <- fAparPlot(prebRuns$all_GV[[i]],siteX)
    p_Litter$GV$spruce[[paste0("st",i)]][[paste0("site",siteX)]] <- nwLitPlot(prebRuns$spruce_GV[[i]],siteX)
    p_Litter$GV$pine[[paste0("st",i)]][[paste0("site",siteX)]] <- nwLitPlot(prebRuns$pine_GV[[i]],siteX)
    p_Litter$GV$all[[paste0("st",i)]][[paste0("site",siteX)]] <- nwLitPlot(prebRuns$all_GV[[i]],siteX)
    
    p_gpp$GV$spruce[[paste0("st",i)]][[paste0("site",siteX)]] <- photoPlot(prebRuns$spruce_GV[[i]],siteX)
    p_gpp$GV$pine[[paste0("st",i)]][[paste0("site",siteX)]] <- photoPlot(prebRuns$pine_GV[[i]],siteX)
    p_gpp$GV$all[[paste0("st",i)]][[paste0("site",siteX)]] <- photoPlot(prebRuns$all_GV[[i]],siteX)
    
    p_Ra$GV$spruce[[paste0("st",i)]][[paste0("site",siteX)]] <- resPlot(prebRuns$spruce_GV[[i]],siteX)
    p_Ra$GV$pine[[paste0("st",i)]][[paste0("site",siteX)]] <- resPlot(prebRuns$pine_GV[[i]],siteX)
    p_Ra$GV$all[[paste0("st",i)]][[paste0("site",siteX)]] <- resPlot(prebRuns$all_GV[[i]],siteX)
    
    p_nee$GV$spruce[[paste0("st",i)]][[paste0("site",siteX)]] <- neePlot(prebRuns$spruce_GV[[i]],prebRuns$spruce[[i]],siteX)
    p_nee$GV$pine[[paste0("st",i)]][[paste0("site",siteX)]] <- neePlot(prebRuns$pine_GV[[i]],prebRuns$pine[[i]],siteX)
    p_nee$GV$all[[paste0("st",i)]][[paste0("site",siteX)]] <- neePlot(prebRuns$all_GV[[i]],prebRuns$all[[i]],siteX)
  }
}

for(ij in 1:7){
  
  specX <- 1
  ggarrange(p_fAPAR$GV[[specX]]$st1[[ij]] + ggtitle("fAPAR"),
            p_Litter$GV[[specX]]$st1[[ij]] + ggtitle("litter"),
            p_gpp$GV[[specX]]$st1[[ij]]  + ggtitle("photos"),
            p_Ra$GV[[specX]]$st1[[ij]] + ggtitle("respiration"),
            p_nee$GV[[specX]]$st1[[ij]] + ggtitle("nee"),
            p_fAPAR$GV[[specX]]$st2[[ij]],p_Litter$GV[[specX]]$st2[[ij]],p_gpp$GV[[specX]]$st2[[ij]],p_Ra$GV[[specX]]$st2[[ij]],p_nee$GV[[specX]]$st2[[ij]],
            p_fAPAR$GV[[specX]]$st3[[ij]],p_Litter$GV[[specX]]$st3[[ij]],p_gpp$GV[[specX]]$st3[[ij]],p_Ra$GV[[specX]]$st3[[ij]],p_nee$GV[[specX]]$st3[[ij]],
            p_fAPAR$GV[[specX]]$st4[[ij]],p_Litter$GV[[specX]]$st4[[ij]],p_gpp$GV[[specX]]$st4[[ij]],p_Ra$GV[[specX]]$st4[[ij]],p_nee$GV[[specX]]$st4[[ij]],
            p_fAPAR$GV[[specX]]$st5[[ij]],p_Litter$GV[[specX]]$st5[[ij]],p_gpp$GV[[specX]]$st5[[ij]],p_Ra$GV[[specX]]$st5[[ij]],p_nee$GV[[specX]]$st5[[ij]],
            nrow = 5,ncol = 5,common.legend = T) %>%
    ggexport(filename = paste0("plots/Site",ij,"spruce_GV.png"),width = 1200,height = 1200)
  # dev.off()
  specX <- 2
  ggarrange(p_fAPAR$GV[[specX]]$st1[[ij]] + ggtitle("fAPAR"),
            p_Litter$GV[[specX]]$st1[[ij]] + ggtitle("litter"),
            p_gpp$GV[[specX]]$st1[[ij]]  + ggtitle("photos"),
            p_Ra$GV[[specX]]$st1[[ij]] + ggtitle("respiration"),
            p_nee$GV[[specX]]$st1[[ij]] + ggtitle("nee"),
            p_fAPAR$GV[[specX]]$st2[[ij]],p_Litter$GV[[specX]]$st2[[ij]],p_gpp$GV[[specX]]$st2[[ij]],p_Ra$GV[[specX]]$st2[[ij]],p_nee$GV[[specX]]$st2[[ij]],
            p_fAPAR$GV[[specX]]$st3[[ij]],p_Litter$GV[[specX]]$st3[[ij]],p_gpp$GV[[specX]]$st3[[ij]],p_Ra$GV[[specX]]$st3[[ij]],p_nee$GV[[specX]]$st3[[ij]],
            p_fAPAR$GV[[specX]]$st4[[ij]],p_Litter$GV[[specX]]$st4[[ij]],p_gpp$GV[[specX]]$st4[[ij]],p_Ra$GV[[specX]]$st4[[ij]],p_nee$GV[[specX]]$st4[[ij]],
            p_fAPAR$GV[[specX]]$st5[[ij]],p_Litter$GV[[specX]]$st5[[ij]],p_gpp$GV[[specX]]$st5[[ij]],p_Ra$GV[[specX]]$st5[[ij]],p_nee$GV[[specX]]$st5[[ij]],
            nrow = 5,ncol = 5,common.legend = T) %>%
    ggexport(filename = paste0("plots/Site",ij,"pine_GV.png"),width = 1200,height = 1200)
  
  
  specX <- 3
  ggarrange(p_fAPAR$GV[[specX]]$st1[[ij]] + ggtitle("fAPAR"),
            p_Litter$GV[[specX]]$st1[[ij]] + ggtitle("litter"),
            p_gpp$GV[[specX]]$st1[[ij]]  + ggtitle("photos"),
            p_Ra$GV[[specX]]$st1[[ij]] + ggtitle("respiration"),
            p_nee$GV[[specX]]$st1[[ij]] + ggtitle("nee"),
            p_fAPAR$GV[[specX]]$st2[[ij]],p_Litter$GV[[specX]]$st2[[ij]],p_gpp$GV[[specX]]$st2[[ij]],p_Ra$GV[[specX]]$st2[[ij]],p_nee$GV[[specX]]$st2[[ij]],
            p_fAPAR$GV[[specX]]$st3[[ij]],p_Litter$GV[[specX]]$st3[[ij]],p_gpp$GV[[specX]]$st3[[ij]],p_Ra$GV[[specX]]$st3[[ij]],p_nee$GV[[specX]]$st3[[ij]],
            p_fAPAR$GV[[specX]]$st4[[ij]],p_Litter$GV[[specX]]$st4[[ij]],p_gpp$GV[[specX]]$st4[[ij]],p_Ra$GV[[specX]]$st4[[ij]],p_nee$GV[[specX]]$st4[[ij]],
            p_fAPAR$GV[[specX]]$st5[[ij]],p_Litter$GV[[specX]]$st5[[ij]],p_gpp$GV[[specX]]$st5[[ij]],p_Ra$GV[[specX]]$st5[[ij]],p_nee$GV[[specX]]$st5[[ij]],
            nrow = 5,ncol = 5,common.legend = T) %>%
    ggexport(filename = paste0("plots/Site",ij,"all_GV.png"),width = 1200,height = 1200)
  
  
  print(ij)
}


extractSoilC <- function(inp){
  soilCtot <- apply(inp,1, sum)
  return(soilCtot)
}

soilCtot <- list()
soilCtot$spruce_ngv <- lapply(soilC$spruce,extractSoilC)
soilCtot$spruce_gv <- lapply(soilC$spruce_GV,extractSoilC)
soilCtot$pine_ngv <- lapply(soilC$pine,extractSoilC)
soilCtot$pine_gv <- lapply(soilC$pine_GV,extractSoilC)
soilCtot$all_ngv <- lapply(soilC$all,extractSoilC)
soilCtot$all_gv <- lapply(soilC$all_GV,extractSoilC)

soilX <- soilXx <- data.table()

siteTypex=1;GVx=0
soilX[,value:= soilCtot$spruce_ngv[[siteTypex]]]
soilX[,site := paste0("site",1:7)]
soilX[,siteType := siteTypex]
soilX[,GV := GVx]
soilX[,species := "spruce"]
soilAll = soilX

soil2 <- data.table()
siteTypex=1;GVx=1
soil2[,value:= soilCtot$spruce_gv[[siteTypex]]]
soil2[,site := paste0("site",1:7)]
soil2[,siteType := siteTypex]
soil2[,GV := GVx]
soil2[,species := "spruce"]

soilX <- rbind(soilAll,soil2)
soil1 <- data.table()
for(siteTypex in 2:5){
  GVx=0
  soil1[,value:= soilCtot$spruce_ngv[[siteTypex]]]
  soil1[,site := paste0("site",1:7)]
  soil1[,siteType := siteTypex]
  soil1[,GV := GVx]
  soil1[,species := "spruce"]
  
  soilX <- rbind(soilX,soil1)
  
  GVx=1
  soil2[,value:= soilCtot$spruce_gv[[siteTypex]]]
  soil2[,site := paste0("site",1:7)]
  soil2[,siteType := siteTypex]
  soil2[,GV := GVx]
  soil2[,species := "spruce"]
  
  soilX <- rbind(soilX,soil2)
  
}


for(siteTypex in 1:5){
  GVx=0
  soil1[,value:= soilCtot$pine_ngv[[siteTypex]]]
  soil1[,site := paste0("site",1:7)]
  soil1[,siteType := siteTypex]
  soil1[,GV := GVx]
  soil1[,species := "pine"]
  
  soilX <- rbind(soilX,soil1)
  
  GVx=1
  soil2[,value:= soilCtot$pine_gv[[siteTypex]]]
  soil2[,site := paste0("site",1:7)]
  soil2[,siteType := siteTypex]
  soil2[,GV := GVx]
  soil2[,species := "pine"]
  
  soilX <- rbind(soilX,soil2)
  
}

for(siteTypex in 1:5){
  GVx=0
  soil1[,value:= soilCtot$all_ngv[[siteTypex]]]
  soil1[,site := paste0("site",1:7)]
  soil1[,siteType := siteTypex]
  soil1[,GV := GVx]
  soil1[,species := "all"]
  
  soilX <- rbind(soilX,soil1)
  
  GVx=1
  soil2[,value:= soilCtot$all_gv[[siteTypex]]]
  soil2[,site := paste0("site",1:7)]
  soil2[,siteType := siteTypex]
  soil2[,GV := GVx]
  soil2[,species := "all"]
  
  soilX <- rbind(soilX,soil2)
  
}

pStstC <- list()
pStstC$spruce <- ggplot(soilX[species=="spruce"],mapping = aes(y=value,x=site,col=factor(siteType),shape=factor(GV))) + 
  geom_point()+ggtitle("spruce")
pStstC$pine <- ggplot(soilX[species=="pine"],mapping = aes(y=value,x=site,col=factor(siteType),shape=factor(GV))) + 
  geom_point()+ggtitle("pine")
pStstC$all <- ggplot(soilX[species=="all"],mapping = aes(y=value,x=site,col=factor(siteType),shape=factor(GV))) + 
  geom_point()+ggtitle("all")

ggarrange(pStstC$spruce,pStstC$pine,pStstC$all,common.legend = T) %>%
  ggexport(filename = "plots/soilCstst.png")#,width = 1200,height = 1200)

