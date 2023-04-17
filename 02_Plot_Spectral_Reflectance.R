## ---------------------------
##
## Script name: Plot Spectral Reflectance
##
## Purpose of script: Plot Spectral Reflectance of Crop residues and soil
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
##
##
## ---------------------------

library(dplyr)
library(ggplot2)
Residue_Median$RWC <- as.character(Residue_Median$RWC)
for (i in unique(Residue_Median$Crop)) {
  g <- ggplot(dplyr::filter(Residue_Median, Crop == i & Wvl %in% (400:2400)), aes(x = Wvl, y = `Reflect. %`, group = RWC, color = RWC)) +
    geom_line() +
    scale_x_continuous(breaks = seq(400, 2400, 100), guide = guide_axis(angle = 90)) +
    scale_color_manual(values = c(
      "red", "green", "blue", "orange", "black", "grey","purple", "yellow", "brown")) +
    # geom_vline(xintercept = 2100, linetype="solid",
    #           color = "blue", size=1)+
    # geom_vline(xintercept = 2200, linetype="solid",
    #           color = "red", size=1)+
    # geom_vline(xintercept = 2000, linetype="solid",
    #           color = "green", size=1)+
    labs(x = "Wavelength (nm)", y = "Spectral Reflectance (%)") +
    theme(text = element_text(size = 20)) +
    facet_wrap(~Crop, ncol = 1)
    ggsave(g, file=paste0("SpecRef_Crop_", i,".png"), width = 24, height = 10, units = "cm")

}

Soil_Median$RWC <- as.character(Soil_Median$RWC)
for (i in unique(Soil_Median$Crop)) {
g <- ggplot(dplyr::filter(Soil_Median, Crop == i & Wvl %in% (400:2400)), aes(x = Wvl, y = `Reflect. %`, group = RWC, color = RWC)) +
  geom_line() +
  scale_x_continuous(breaks = seq(400, 2400, 100), guide = guide_axis(angle = 90)) +
  scale_color_manual(values = c(
    "red", "green", "blue", "orange", "black", "grey",
    "purple", "yellow", "brown"
  )) +
  labs(x = "Wavelength (nm)", y = "Spectral Reflectance (%)") +
  theme(text = element_text(size = 20))+
  facet_wrap(~Crop, ncol = 1)
  ggsave(g, file=paste0("SpecRef_Soil_", i,".png"), width = 24, height = 10, units = "cm")
}
