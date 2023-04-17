Samples <- CAI
Samples <- rbind(Residue_Median,Soil_Median)
colnames(Samples)[5] <- "Reflectance"


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
  mutate(NDTI = (B6-B7)/(B6+B7)) %>%
  mutate(OLI = (B6/B7))-> Landsat_Group


Landsat_Group$Sample <- replace(Landsat_Group$Sample,Landsat_Group$Sample=="Residue","Crop Residue")

ggplot(Landsat_Group,aes(x= RWC,y=NDTI,group = Crop, color = Sample))+
  #geom_line() +
  geom_smooth(method = "loess",se=FALSE,size=1) +
  labs(x = "Relative Water Content", y = "NDTI")+
  theme_bw()+
  scale_color_npg() #ggsci

ggplot(Landsat_Group,aes(x= RWC,y=OLI, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(0.8, 2), breaks = seq(0.8, 2, by = 0.2)) +
  labs(x = "RWC", y = "OLI6/OLI7") +
  theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
        legend.title=element_blank(),
        legend.margin=margin(c(1,5,5,5)))+
  coord_flip()
  


ggplot(Landsat_Group,aes(x= RWC,y=NDTI, group = Crop, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(-0.1, 0.4), breaks = seq(-0.1, 0.4, by = 0.1)) +
  labs(x = "RWC", y = "NDTI") 


  

Landsat_Group_subset <- dplyr::filter(Landsat_Group,Sample == "Soil", OLI>0 & OLI < 1)
Landsat_Group_subset <- dplyr::filter(Landsat_Group,Sample == "Crop Residue", OLI>1 & OLI < 1.65)
y <- (Landsat_Group_subset$RWC)
x <- (Landsat_Group_subset$OLI)
model <- lm((y)~x)
summary(model)
confint(model, level=.95)

## World View

Samples <- rbind(Residue_Median,Soil_Median)

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
  mutate(SWIR = (SWIR3/SWIR6))%>%
  mutate(SINDRI = 100*(SWIR6-SWIR7)/(SWIR6+SWIR7))-> WV_Group

ggplot(WV_Group,aes(x= RWC,y=SINDRI,group = Crop, color = Sample))+
  #geom_line() +
  geom_smooth(method = "loess",se=FALSE,size=1) +
  labs(x = "Relative Water Content", y = "SINDRI")+
  theme_bw()+
  scale_color_npg()


ggplot(WV_Group,aes(x= RWC,y=SWIR, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(0.8, 2), breaks = seq(0.8, 2, by = 0.2)) +
  labs(x = "RWC", y = "SWIR3/SWIR6") +
  theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
        legend.title=element_blank(),
        legend.margin=margin(c(1,5,5,5)))+
  coord_flip()

ggplot(WV_Group,aes(x= RWC,y=SINDRI, group = Crop, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(-15, -3), breaks = seq(-15, -3, by = 1)) +
  labs(x = "RWC", y = "SINDRI") 


WV_Group_subset <- dplyr::filter(WV_Group,Sample == "Soil")
WV_Group_subset <- dplyr::filter(WV_Group,Sample == "Residue",SWIR>1 & SWIR < 1.62)

y <- (WV_Group_subset$RWC)
x <- (WV_Group_subset$SWIR)
model <- lm((y)~x)
summary(model)
confint(model, level=.95)


##Hyperion EnMap

Samples <- rbind(Residue_Median,Soil_Median)

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
  mutate(WI = (B22/B20))%>%
  mutate(CAI = (0.5*(B20+B22)-B21))-> Hyper_Group

ggplot(Hyper_Group,aes(x= RWC,y=CAI,group = Crop, color = Sample))+
  #geom_line() +
  geom_smooth(method = "loess",se=FALSE,size=1) +
  labs(x = "Relative Water Content", y = "CAI")+
  theme_bw()+
  scale_color_npg()

ggplot(Hyper_Group,aes(x= RWC,y=CAI, group = Crop, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(-2, 6), breaks = seq(-2, 6, by = 1)) +
  labs(x = "RWC", y = "CAI") 


ggplot(Hyper_Group,aes(x= RWC,y=WI, color = Sample)) +
  geom_point() +
  geom_smooth(method = "loess",se=FALSE,span = TRUE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(0.8, 2), breaks = seq(0.8, 2, by = 0.2)) +
  labs(x = "RWC", y = "B2.2/B2.0") +
  theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
        legend.title=element_blank(),
        legend.margin=margin(c(1,5,5,5)))+
  coord_flip()

Hyper_Group_subset <- dplyr::filter(Hyper_Group,Sample == "Soil",WI>1 & WI < 1.4)
Hyper_Group_subset <- dplyr::filter(Hyper_Group,Sample == "Residue",WI>1 & WI < 1.8)

y <- (Hyper_Group_subset$RWC)
x <- (Hyper_Group_subset$WI)
model <- lm((y)~x)
summary(model)
confint(model, level=.95)

#####
## Mixing from 05 file

LinearMix_sub <- dplyr::filter(LinearMix,Crop.x=="Canola"&Crop.y=="Almira_bottom")


CAI <- LinearMix_sub %>%
  dplyr::filter(Wvl == 2200 | Wvl == 2000 | Wvl == 2100 | Wvl == 2260 | Wvl == 2205 | Wvl == 1660 | Wvl == 2330)


CAI <- CAI[c(1,4,8,11,17)]


CAI <- CAI %>%
  spread(Wvl,Ref_conven_lower) %>%
  mutate(CAI = 2200 / 2000) %>%
  mutate(SINDRI = 2200 / 2000) %>%
  mutate(NDTI = 2200 / 2000)

CAI$CAI <- (0.5 * (CAI$`2000` + CAI$`2200`) - CAI$`2100`)
CAI$SINDRI <- 100 * (CAI$`2205` - CAI$`2260`) / (CAI$`2205` + CAI$`2260`)
CAI$NDTI <- (CAI$`1660` - CAI$`2330`) / (CAI$`1660` + CAI$`2330`)

