## ---------------------------
##
## Script name: Read files generated from spectrometer, file format is .sed
##
## The SED format is a flexible specification for representing one-dimensional spectra (distributions of reflectance vs wavelength)
## The SED is structured as a table with one row per wavelength and columns for the reflectance
## The .sed files are ASCII files so they can be imported directly into software that accepts ASCII files, for example Excel, ENVI, TSG, and more.

## Purpose of script:Reading spectral reflectance of crop residues and soil for different water content
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

## Required packages

library(tidyverse)
library(magrittr)
library(plyr)

## Reading spectral refleactance of crop resideus
## set working directory, in this case we set it as the folder where Spectrometry scans were saved
## Structure of the folder is "/Spectrometry/Results/

setwd("E:/Spectrometry/Results/")

## Reading all the scan files with .sed format in the Residue folder. We need to specify the pattern because the
## folder contain other files with .raw format which is not needed to be read

Data.in <- (list.files(
  path = "Residue",
  pattern = "*.sed", recursive = TRUE, full.names = TRUE
))

## Creating a function read_csv_filename to read all the files stored in Data.in the previous step
## skip = 26 is used to skip the first 26 lines in each of the files since they store metadata for each scan
## saving the file name as one of the column

read_csv_filename <- function(filename) {
  ret <- read_delim(filename, delim = "\t", skip = 26)
  ret$Source <- filename
  ret
}

Residue <- ldply(Data.in, read_csv_filename)
## Separating the Source column and saving the relevant information like Sample,Scan and Crop as new columns
Residue <- separate(data = Residue, col = Source, into = c(NA, NA, NA, NA, "Sample", "Scan", "Crop", NA, NA), sep = "/")
Residue$`Reflect. %` <- as.double(Residue$`Reflect. %`)
Residue$Wvl <- as.double(Residue$Wvl)

## Reading the details about relative water content for crop residues for each scan

CropMoisture <- read.csv("/CropMoisture.csv")
CropMoisture$RWC <- ifelse(CropMoisture$RWC > 1, 1, CropMoisture$RWC)

Residue <- merge(Residue, CropMoisture, by = c("Scan", "Crop"))

## If the you want to smooth the reflectance value you can use the savgol function with different parameters 
Crop_Residue <- Residue %>%
  group_by(Wvl, Sample, Scan, Crop) %>%
  # mutate(SmoothRef = savgol((Residue$`Reflect. %`), 51, 2,0)) %>%
  summarise_all(.funs = c("median"))

## Saving the output as a .csv file
write.csv(Crop_Residue, "Crop_Residue.csv", row.names = FALSE)

## Reading spectral reflectance of soil samples
## Reading all the scan files with .sed format in the Residue folder. We need to specify the pattern because the
## folder contain other files with .raw format which is not needed to be read

Data.in <- (list.files(
  path = "/Soil",
  pattern = "*.sed", recursive = TRUE, full.names = TRUE
))

## Creating a function read_csv_filename to read all the files stored in Data.in the previous step
## skip = 26 is used to skip the first 26 lines in each of the files since they store metadata for each scan

read_csv_filename <- function(filename) {
  ret <- read_delim(filename, delim = "\t", skip = 26)
  ret$Source <- filename
  ret
}

Soil <- ldply(Data.in, read_csv_filename)
## Separating the Source column and saving the relevant information like Sample,Scan and Crop as new columns
Soil <- separate(data = Soil, col = Source, into = c(NA, NA, NA, NA, "Sample", "Scan", "Crop", NA, NA), sep = "/")
Soil$`Reflect. %` <- as.double(Soil$`Reflect. %`)
Soil$Wvl <- as.double(Soil$Wvl)

## Reading the details about relative water content for soil for each scan

SoilMoisture <- read.csv("/SoilMoisture.csv")
Soil <- merge(Soil, SoilMoisture, by = c("Scan", "Crop"))

## If the you want to smooth the reflectance value you can use the savgol function with different parameters 
Soil <- Soil %>%
  group_by(Wvl, Sample, Scan, Crop) %>%
  # mutate(SmoothRef = savgol((Soil$`Reflect. %`), 51, 2,0)) %>%
  summarise_all(.funs = c("median"))

## Saving the output as a .csv file
write.csv(Soil, "Soil.csv", row.names = FALSE)
