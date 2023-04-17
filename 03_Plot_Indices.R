## ---------------------------
##
## Script name: Create water indices
##
## Purpose of script: Filter wavelength which are used for indices like CAI, SINDRI, NDTI and create plots
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

library(tidyverse)
library(dplyr)
library(ggplot2)

## Renaming the column to Reflectance
colnames(Residue_Median)[5] <- "Reflectance"

CAI <- Residue_Median %>%
  dplyr::filter(Wvl == 2200 | Wvl == 2000 | Wvl == 2100 | Wvl == 2260 | Wvl == 2205 | Wvl == 1660 |Wvl == 1600 | Wvl == 2330)

CAI <- CAI %>%
  spread(Wvl, Reflectance) %>%
  mutate(CAI = 2200 / 2000) %>%
  mutate(SINDRI = 2200 / 2000) %>%
  mutate(NDTI = 2200 / 2000) %>%
  mutate(R2220 = 2200 / 2000)%>%
  mutate(R1620 = 2200 / 2000)%>%
  mutate(RSWIR = 2200 / 2000)%>%
  mutate(ROLI = 2200 / 2000)


CAI$CAI <- (0.5 * (CAI$`2000` + CAI$`2200`) - CAI$`2100`)
CAI$SINDRI <- 100 * (CAI$`2205` - CAI$`2260`) / (CAI$`2205` + CAI$`2260`)
CAI$NDTI <- (CAI$`1660` - CAI$`2330`) / (CAI$`1660` + CAI$`2330`)
CAI$R2220 <- CAI$`2200`/CAI$`2000`
CAI$R1620 <- CAI$`1600`/CAI$`2200`
CAI$RSWIR <- CAI$`1660`/CAI$`2260`
CAI$ROLI <- CAI$`1600`/CAI$`2200`

ggplot(CAI, aes(x = RWC, y = CAI, group = Crop, color = Crop)) +
  geom_point() +
  geom_line()
facet_wrap(~Crop, ncol = 2)

ggplot(CAI, aes(x = RWC, y = SINDRI, group = Crop, color = Crop)) +
  geom_point() +
  geom_line()
facet_wrap(~Crop, ncol = 2)

ggplot(CAI, aes(x = RWC, y = NDTI, group = Crop, color = Crop)) +
  geom_point() +
  geom_line()
facet_wrap(~Crop, ncol = 2)

colnames(Soil_Median)[5] <- "Reflectance"
CAI1 <- Soil_Median %>%
  dplyr::filter(Wvl == 2200 | Wvl == 2000 | Wvl == 2100 | Wvl == 2260 | Wvl == 2205 | Wvl == 1660 |Wvl == 1600 | Wvl == 2330)

CAI1 <- CAI1 %>%
  spread(Wvl, Reflectance) %>%
  mutate(CAI = 2200 / 2000) %>%
  mutate(SINDRI = 2200 / 2000) %>%
  mutate(NDTI = 2200 / 2000)%>%
  mutate(R2220 = 2200 / 2000)%>%
  mutate(R1620 = 2200 / 2000)%>%
  mutate(RSWIR = 2200 / 2000)%>%
  mutate(ROLI = 2200 / 2000)

CAI1$CAI <- (0.5 * (CAI1$`2000` + CAI1$`2200`) - CAI1$`2100`)
CAI1$SINDRI <- 100 * (CAI1$`2205` - CAI1$`2260`) / (CAI1$`2205` + CAI1$`2260`)
CAI1$NDTI <- (CAI1$`1660` - CAI1$`2330`) / (CAI1$`1660` + CAI1$`2330`)
CAI1$R2220 <- CAI1$`2200`/CAI1$`2000`
CAI1$R1620 <- CAI1$`1600`/CAI1$`2200`
CAI1$RSWIR <- CAI1$`1660`/CAI1$`2260`
CAI1$ROLI <- CAI1$`1600`/CAI1$`2200`

CAI <- rbind(CAI1, CAI)
CAI$Scan <- gsub(" ", "", CAI$Scan)

ggplot(CAI, aes(x = RWC, y = CAI, group = Crop, color = Sample)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "CAI") +
  theme(text = element_text(size = 20))

scale_x_discrete(limits = rev(levels(as.factor(CAI$Scan))), guide = guide_axis(angle = 90))

ggplot(CAI, aes(x = RWC, y = R2220, group = Sample, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "R2.2/R2.0") +
  theme(text = element_text(size = 20))+
  coord_flip()

ggplot(CAI, aes(x = RWC, y = R2220, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(0.8, 2), breaks = seq(0.8, 2, by = 0.2)) +
  labs(x = "RWC", y = "R2.2/R2.0") +
  theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
        legend.title=element_blank(),
        legend.margin=margin(c(1,5,5,5)))+
  coord_flip()

ggplot(CAI, aes(x = RWC, y = R1620, group = Sample, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "R1.6/R2.0") +
  theme(text = element_text(size = 20))+
  coord_flip()

ggplot(CAI, aes(x = RWC, y = RSWIR, group = Sample, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "SWIR6/SWIR7") +
  theme(text = element_text(size = 20))+
  coord_flip()

ggplot(CAI, aes(x = RWC, y = ROLI, group = Sample, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "OLI6/OLI7") +
  theme(text = element_text(size = 20))+
  coord_flip()

ggplot(dplyr::filter(CAI, Crop == "Wheat Duet" | Crop == "Pomeroy_top"), aes(x = RWC, y = SINDRI, group = Crop, color = Sample)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "SINDRI") +
  theme(axis.text.y = element_blank(), text = element_text(size = 20))

ggplot(dplyr::filter(CAI, Crop == "Wheat Duet" | Crop == "Pomeroy_top"), aes(x = RWC, y = CAI, group = Crop, color = Sample)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "CAI") +
  theme(axis.text.y = element_blank(), text = element_text(size = 20))

ggplot(dplyr::filter(CAI, Crop == "Wheat Duet" | Crop == "Pomeroy_top"), aes(x = RWC, y = NDTI, group = Crop, color = Sample)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "NDTI") +
  theme(axis.text.y = element_blank(), text = element_text(size = 20))

