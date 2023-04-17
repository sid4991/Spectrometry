Samples %>%
  filter(!grepl('top', Crop)) %>%
  mutate(Bands = ifelse(Wvl>= 450 & Wvl < 510, "B2",
                        ifelse(Wvl>= 530 & Wvl < 590, "B3",
                               ifelse(Wvl>= 640 & Wvl < 670, "B4",
                                      ifelse(Wvl>= 850 & Wvl < 880, "B5",
                                             ifelse(Wvl>= 1570 & Wvl < 1650, "B6",
                                                    ifelse(Wvl>= 2110 & Wvl < 2290, "B7","No")))))))%>%
  filter(Bands != "No")%>%
  filter(Bands %in% c("B6","B7"))%>%
  group_by(Scan,Crop,Sample,Bands)%>%
  dplyr::summarise(Reflectance = median(Reflectance),
                   RWC = median(RWC))%>%
  pivot_wider(names_from = Bands,values_from = Reflectance)%>%
  mutate(NDTI = (B6-B7)/(B6+B7))%>% -> Landsat_Group
Landsat_Group <- Landsat_Group[-c(5,6)]


Samples %>%
  filter(!grepl('top', Crop)) %>%
  mutate(Bands = ifelse(Wvl>= 450 & Wvl < 510, "B2",
                        ifelse(Wvl>= 510 & Wvl < 580, "B3",
                               ifelse(Wvl>= 585 & Wvl < 625, "B4",
                                      ifelse(Wvl>= 630 & Wvl < 690, "B5",
                                             ifelse(Wvl>= 705 & Wvl < 745, "B6",
                                                    ifelse(Wvl>= 770 & Wvl < 895, "B7",
                                                           ifelse(Wvl>= 860 & Wvl < 1040, "B8",
                                                                  ifelse(Wvl>= 1195 & Wvl < 1225, "SWIR1",
                                                                         ifelse(Wvl>= 1550 & Wvl < 1590, "SWIR2",
                                                                                ifelse(Wvl>= 1640 & Wvl < 1680, "SWIR3",
                                                                                       ifelse(Wvl>= 1710 & Wvl < 1750, "SWIR4",
                                                                                              ifelse(Wvl>= 2145 & Wvl < 2185, "SWIR5",
                                                                                                     ifelse(Wvl>= 2185 & Wvl < 2225, "SWIR6",
                                                                                                            ifelse(Wvl>= 2235 & Wvl < 2285, "SWIR7",
                                                                                                                   ifelse(Wvl>= 2295 & Wvl < 2365, "SWIR8","No"))))))))))))))))%>%
  filter(Bands != "No")%>%
  filter(Bands %in% c("SWIR3","SWIR6","SWIR7"))%>%
  group_by(Scan,Crop,Sample,Bands)%>%
  dplyr::summarise(Reflectance = median(Reflectance),
                   RWC = median(RWC))%>%
  pivot_wider(names_from = Bands,values_from = Reflectance)%>%
  mutate(SINDRI = 100*(SWIR6-SWIR7)/(SWIR6+SWIR7))-> WV_Group
WV_Group <- WV_Group[-c(5:7)]

Samples %>%
  filter(!grepl('top', Crop)) %>%
  mutate(Bands = ifelse(Wvl>= 2025 & Wvl < 2035, "B20",
                        ifelse(Wvl>= 2095 & Wvl < 2105, "B21",
                               ifelse(Wvl>= 2200 & Wvl < 2210, "B22","No"))))%>%  filter(Bands != "No")%>%
  filter(Bands %in% c("B20","B21","B22"))%>%
  group_by(Scan,Crop,Sample,Bands)%>%
  dplyr::summarise(Reflectance = median(Reflectance),
                   RWC = median(RWC))%>%
  pivot_wider(names_from = Bands,values_from = Reflectance)%>%
  mutate(CAI = (0.5*(B20+B22)-B21))-> Hyper_Group
Hyper_Group <- Hyper_Group[-c(5:7)]

test <- merge(Landsat_Group,WV_Group,by = c("Scan","Crop","RWC","Sample"))
SatelliteBands <- merge(test,Hyper_Group,by = c("Scan","Crop","RWC","Sample"))

##########

a <- data.frame()
b <- data.frame()
c <- data.frame()
d <- data.frame()
e <- data.frame()
f <- data.frame()


for (i in length(unique(SatelliteBands$Scan))) {
  a <- dplyr::filter(SatelliteBands, Sample == "Soil")
  b <- dplyr::filter(SatelliteBands, Sample == "Residue")
  for (j in unique(a$Crop)) {
    c <- dplyr::filter(filter(a, Crop == j))
    d <- merge(c, b, by.x = "Scan", by.y = "Scan")
    e <- rbind(e, d)
  }
  f <- rbind(f, e)
}


conven_lower <- 0.0 ## 0-15% residue cover
conven_upper <- 0.10
med_lower <- 0.17 ## 15-30 % residue cover 22% mean
med_upper <- 0.25 ## 15-30 % residue cover 22% mean
conser_lower <- 0.40 ## 30-100 % residue cover 65% mean
conser_upper <- 1 ## 30-100 % residue cover 65% mean

test1 <- f

test1$CAI_conven_lower <- test1$CAI.x * (1 - conven_lower) + test1$CAI.y * (conven_lower)
test1$CAI_conven_upper <- test1$CAI.x * (1 - conven_upper) + test1$CAI.y * (conven_upper)
test1$CAI_med_lower <- test1$CAI.x * (1 - med_lower) + test1$CAI.y * (med_lower)
test1$CAI_med_upper <- test1$CAI.x * (1 - med_upper) + test1$CAI.y * (med_upper)
test1$CAI_conser_lower <- test1$CAI.x * (1 - conser_lower) + test1$CAI.y * (conser_lower)
test1$CAI_conser_upper <- test1$CAI.x * (1 - conser_upper) + test1$CAI.y * (conser_upper)

test2 <- dplyr::filter(test1, Crop.x == "Almira_bottom" & Crop.y == "Wheat Pritchett")

test3 <- test2[c(3,9,14:19)] ## select the conven,med,conser colums
test3$RWC.x <- round(rowMeans(test3[,c("RWC.x","RWC.y")], na.rm=TRUE),2)
test3 <- test3[-c(2)]
test3 <- reshape2::melt(test3, id = "RWC.x")

test3$variable <- gsub("CAI_conven_lower", conven_lower, test3$variable)
test3$variable <- gsub("CAI_conven_upper", conven_upper, test3$variable)
test3$variable <- gsub("CAI_med_lower", med_lower, test3$variable)
test3$variable <- gsub("CAI_med_upper", med_upper, test3$variable)
test3$variable <- gsub("CAI_conser_lower", conser_lower, test3$variable)
test3$variable <- gsub("CAI_conser_upper", conser_upper, test3$variable)
test3$variable <- as.numeric(test3$variable)

names(test3)[2] <- "Fraction_Residue_Cover"
names(test3)[3] <- "CAI"
ggplot(test3, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.x))) +
  geom_line(aes(color = factor(RWC.x)))

##Fig5
ggplot(test3, aes(CAI, Fraction_Residue_Cover, group = factor(RWC.x))) +
  geom_line(aes(color = factor(RWC.x))) +
  geom_point(aes(shape = factor(RWC.x)))+
  labs(y = "Fraction Residue Cover", x = "CAI") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
        legend.title=element_blank(),
        legend.margin=margin(c(1,5,5,5)))

####
y <- (test3$Fraction_Residue_Cover)
x <- (test3$CAI)
model <- lm((y)~x)
summary(model)
