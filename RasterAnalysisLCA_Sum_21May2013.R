## This code is for applying - here in NY - the "Landscape Condition" methods developed 
## by Colorado NHP/Co State. See SHRP 2 C21A April 2012. 
## Aissa Feldmann, modifying Tim Howard's code from June 2012. Date 21 May 2013, NYNHP.

## FINAL RUN

library(raster)

#change the path below to your working directory
setwd("C:/Aissa/__EPA_wetlands/LCA2/tifs2")
    
#set a mask; change the path below to the snap raster in your working directory
msk <- raster("C:/Aissa/__EPA_wetlands/LCA2/tifs2/snapras30met.tif")

#set up the distance decay function and weight (a, b, scalar=100 (distance of 2000 m divided by 20), w, decdist) 
#for each layer. These do not need to change.

rules <- matrix( data= c(
        "ALIS_PrimHwyLimAcc_dist2k.tif", 5, 1, 100, 500, 1000,
        "ALIS_PrimHwyWOLimAcc_dist2k.tif", 5, 1, 100, 500, 1000,
		    "ALIS_SecondConnectSpecial_dist2k.tif", 2.5, 2, 100, 500, 500,
		    "ALIS_LocNeighRur_dist2k.tif", 1, 5, 100, 300, 200,
		    "ALIS_VehicTr_dist2k.tif", 0.25, 20, 100, 100, 50,
		    "NYSRail_Active_dist2k.tif", 0.5, 10, 100, 500, 100,
        "CCAP_02_HighIntenDev_dist2k.tif", 10, 0.5, 100, 500, 2000,
        "CCAP_03_MedIntenDev_dist2k.tif", 0.25, 2, 100, 400, 300,
        "CCAP_04_LowIntenDev_dist2k.tif", 0.25, 2, 100, 300, 300,
		    "VNTX_ElectTrnsAll_dist2k.tif", 0.5, 10, 100, 300, 100,
		    "VNTX_NatGasAll_dist2k.tif", 0.5, 10, 100, 300, 100,
		    "cs_allag2_dist2k.tif", 1, 5, 100, 300, 200,
		    "CCAP_05_OpenSpaces_dist2k.tif", 1, 5, 100, 300, 200			
        ), ncol = 6, byrow = TRUE)

rules <- as.data.frame(rules)
names(rules) <-  c("filename", "a", "b", "scalar", "weight", "decdist")
rules$a <- as.numeric(levels(rules$a))[rules$a]
rules$b <- as.numeric(levels(rules$b))[rules$b]
rules$scalar <- as.numeric(levels(rules$scalar))[rules$scalar]
rules$weight <- as.numeric(levels(rules$weight))[rules$weight]
rules$decdist <- as.numeric(levels(rules$decdist))[rules$decdist]

#2013 sigmoid function for calculations	 
fun <- function(x) { 
    x[x>dist] <- NA  #clear out the irrelevant vals
    if(dist == 0){ 
        x[x==0] <- wt
    }else{
        x <- (1/(1+(exp(((x/scal)-shift)*spread))))*wt
    }
    return(x)
}     
     
#blank stack to add to within loop
stk <- stack()      
#cycle through all layers      
for(i in 1:nrow(rules)){
    flnm <- as.character(rules$filename[[i]])
	shift <- as.numeric(rules [i,2])
	spread <- as.numeric(rules [i,3])
	dist <- as.numeric(rules [i,6])
    scal <- as.numeric(rules[i,4])
    wt <- as.numeric(rules[i,5])
    flnmNoX <- gsub("_dist2k.tif","", flnm)
    ras <- raster(flnm)
    ras2 <- calc(ras, fun)
	ras2@data@names <- flnmNoX
    stk <- addLayer(stk, ras2)
}

#write out stack layers so we don't have to calculate again
for(i in 1:nlayers(stk)){
    writeRaster(stk[[i]], filename = paste(stk[[i]]@data@names, "_impct2",sep="") , format = "GTiff", overwrite=TRUE)
}                

#### save the stack
# stk <- stackSave(stk, "ImpactRasters")
# stk <- stackOpen("ImpactRasters.stk")
#####

rasSum <- sum(stk[[1]], stk[[2]], stk[[3]], stk[[4]], stk[[5]], 
				stk[[6]], stk[[7]], stk[[8]], stk[[9]], stk[[10]], 
				stk[[11]], stk[[12]], stk[[13]], 
                 na.rm = TRUE)

## Change the value of this filename to reflect the number of layers (here=13) and the 
## number of runs (here=the 4th run).
rasSum1 <- writeRaster(rasSum, filename = "SumValOn13_4", format = "GTiff", overwrite=TRUE)
projection(rasSum1) <- projection(ras)   
   
#Insert 0 in NA, but the remove it for outside NYs
               
fun2 <- function(x) {x[is.na(x)] <- 0; return(x)}
rasSum2 <- calc(rasSum1, fun2)

## Change the value of this filename to reflect the number of layers (here=13) and the 
## number of runs (here=the 4th run).
projection(msk) <- projection(ras)
rasSumMsk <- mask(rasSum2, msk, filename = "LandscapeConditionSum13_4", format = "GTiff", overwrite=TRUE	)



