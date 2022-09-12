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

CropList <- unique(Residue_08_18$Crop)
SoilList <- unique(Soil_08_18$Crop)
Temp <- data.frame()
for (i in (CropList)) {
  Cropi_i <- dplyr::filter(Residue_08_18,Crop == i)
  names(Cropi_i)[6] <- "RWC_Residue"
  TempCrop <- Cropi_i
    for (j in (SoilList)) {
      TempSoil <- data.frame()
      Soil_j <- dplyr::filter(Soil_08_18,Crop == j)
      names(Soil_j)[6] <- "RWC_Soil"
      TempSoil <- Soil_j
      TempSoilRes <- merge(TempCrop,TempSoil,by = c("Wvl","Scan"),all = T)
      Temp <- rbind(Temp,TempSoilRes)

  }
}

Temp$RWC_conven <- Temp$RWC_Soil*(1-0.07) + Temp$RWC_Residue*(0.07)
Temp$RWC_med <- Temp$RWC_Soil*(1-0.20) + Temp$RWC_Residue*(0.20)
Temp$RWC_conserv <- Temp$RWC_Soil*(1-0.60) + Temp$RWC_Residue*(0.60)

