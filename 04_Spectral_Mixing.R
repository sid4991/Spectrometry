## ---------------------------
##
## Script name: Create_Linear_Model
##
## Purpose of script:Spectral mixing using linearcombination
##
## Author: Siddharth Chaudhary
##
## Date Created: 2022-08-26
##
## Copyright (c) Siddharth Chaudhary, 2022
## Email: siddharth.chaudhary@wsu.edu
##
## ---------------------------
##
## Notes:
## Using the formula RWC = RWCs*(1-Fr) + RWCr*(Fr)
##
## ---------------------------

conven <- 0.07 ##0-15% residue cover 7% mean
med <- 0.22   ## 15-30 % residue cover 22% mean
conser <- 0.65 ## 30-100 % residue cover 65% mean

CropList <- unique(Residue_08_18$Crop)
SoilList <- unique(Soil_08_18$Crop)
LinearMix <- data.frame()
for (i in (CropList)) {
  Cropi_i <- dplyr::filter(Residue_08_18,Crop == i)
  names(Cropi_i)[6] <- "RWC_Residue"
  TempCrop <- Cropi_i
    for (j in (SoilList)) {
      TempSoil <- data.frame()
      Soil_j <- dplyr::filter(Soil_08_18,Crop == j)
      names(Soil_j)[6] <- "RWC_Soil"
      TempSoil <- Soil_j
      # TempSoilRes <- merge(TempCrop,TempSoil,by = c("Wvl"),all = T)
      TempSoilRes <- merge(TempCrop,TempSoil,by = c("Wvl","Scan"),all = T)
      LinearMix <- rbind(LinearMix,TempSoilRes)

  }
}

LinearMix$RWC_conven <- LinearMix$RWC_Soil*(1-conven) + LinearMix$RWC_Residue*(conven)
LinearMix$RWC_med <- LinearMix$RWC_Soil*(1-med) + LinearMix$RWC_Residue*(med)
LinearMix$RWC_conserv <- LinearMix$RWC_Soil*(1-conser) + LinearMix$RWC_Residue*(conser)
LinearMix <- na.omit(LinearMix)
