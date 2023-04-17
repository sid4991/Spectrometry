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

conven_lower <- 0.0 ## 0-15% residue cover
conven_upper <- 0.10
med_lower <- 0.17 ## 15-30 % residue cover 22% mean
med_upper <- 0.25 ## 15-30 % residue cover 22% mean
conser_lower <- 0.40 ## 30-100 % residue cover 65% mean
conser_upper <- 1 ## 30-100 % residue cover 65% mean

CropList <- unique(Residue_Median$Crop)
SoilList <- unique(Soil_Median$Crop)
LinearMix <- data.frame()
for (i in (CropList)) {
  Cropi_i <- dplyr::filter(Residue_Median, Crop == i)
  names(Cropi_i)[6] <- "RWC_Residue"
  names(Cropi_i)[5] <- "Ref_Residue"
  TempCrop <- Cropi_i
  for (j in (SoilList)) {
    TempSoil <- data.frame()
    Soil_j <- dplyr::filter(Soil_Median, Crop == j)
    names(Soil_j)[6] <- "RWC_Soil"
    names(Soil_j)[5] <- "Ref_Soil"
    TempSoil <- Soil_j
    # TempSoilRes <- merge(TempCrop,TempSoil,by = c("Wvl"),all = T)
    TempSoilRes <- merge(TempCrop, TempSoil, by = c("Wvl", "Scan"), all = T)
    LinearMix <- rbind(LinearMix, TempSoilRes)
  }
}

LinearMix$RWC_conven_lower <- LinearMix$RWC_Soil * (1 - conven_lower) + LinearMix$RWC_Residue * (conven_lower)
LinearMix$RWC_conven_upper <- LinearMix$RWC_Soil * (1 - conven_upper) + LinearMix$RWC_Residue * (conven_upper)
LinearMix$RWC_med_lower <- LinearMix$RWC_Soil * (1 - med_lower) + LinearMix$RWC_Residue * (med_lower)
LinearMix$RWC_med_upper <- LinearMix$RWC_Soil * (1 - med_upper) + LinearMix$RWC_Residue * (med_upper)
LinearMix$RWC_conser_lower <- LinearMix$RWC_Soil * (1 - conser_lower) + LinearMix$RWC_Residue * (conser_lower)
LinearMix$RWC_conser_upper <- LinearMix$RWC_Soil * (1 - conser_upper) + LinearMix$RWC_Residue * (conser_upper)

LinearMix$Ref_conven_lower <- LinearMix$Ref_Soil * (1 - conven_lower) + LinearMix$Ref_Residue * (conven_lower)
LinearMix$Ref_conven_upper <- LinearMix$Ref_Soil * (1 - conven_upper) + LinearMix$Ref_Residue * (conven_upper)
LinearMix$Ref_med_lower <- LinearMix$Ref_Soil * (1 - med_lower) + LinearMix$Ref_Residue * (med_lower)
LinearMix$Ref_med_upper <- LinearMix$Ref_Soil * (1 - med_upper) + LinearMix$Ref_Residue * (med_upper)
LinearMix$Ref_conser_lower <- LinearMix$Ref_Soil * (1 - conser_lower) + LinearMix$Ref_Residue * (conser_lower)
LinearMix$Ref_conser_upper <- LinearMix$Ref_Soil * (1 - conser_upper) + LinearMix$Ref_Residue * (conser_upper)

LinearMix <- na.omit(LinearMix)

write.csv(LinearMix, "LinearMix_onetomany.csv", row.names = F)
write.csv(LinearMix, "LinearMix_onetoone.csv", row.names = F)
