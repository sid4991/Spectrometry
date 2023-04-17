## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Siddharth Chaudhary
##
## Date Created: 2022-09-20
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

## creating some empty dataframes
Residue_Median <- read_csv("Residue_08_18.csv")
Soil_Median <- read_csv("Soil_08_18.csv")
## Run 03 to create CAI it might have got altered or saved as soil_residue_combined.csv
CAI <- read_csv("soil_residue_combined.csv")

a <- data.frame()
b <- data.frame()
c <- data.frame()
d <- data.frame()
e <- data.frame()
f <- data.frame()


for (i in length(unique(CAI$Scan))) {
  a <- dplyr::filter(CAI, Sample == "Soil")
  b <- dplyr::filter(CAI, Sample == "Residue")
  for (j in unique(a$Crop)) {
    c <- dplyr::filter(filter(a, Crop == j))
    d <- merge(c, b, by.x = "Scan", by.y = "Scan")
    e <- rbind(e, d)
  }
  f <- rbind(f, e)
}


conven_lower <- 0.05 ## 0-15% residue cover
conven_upper <- 0.10
med_lower <- 0.17 ## 15-30 % residue cover 22% mean
med_upper <- 0.25 ## 15-30 % residue cover 22% mean
conser_lower <- 0.40 ## 30-100 % residue cover 65% mean
conser_upper <- 0.65 ## 30-100 % residue cover 65% mean

test1 <- f

test1$CAI_conven_lower <- test1$NDTI.x * (1 - conven_lower) + test1$NDTI.y * (conven_lower)
test1$CAI_conven_upper <- test1$NDTI.x * (1 - conven_upper) + test1$NDTI.y * (conven_upper)
test1$CAI_med_lower <- test1$NDTI.x * (1 - med_lower) + test1$NDTI.y * (med_lower)
test1$CAI_med_upper <- test1$NDTI.x * (1 - med_upper) + test1$NDTI.y * (med_upper)
test1$CAI_conser_lower <- test1$NDTI.x * (1 - conser_lower) + test1$NDTI.y * (conser_lower)
test1$CAI_conser_upper <- test1$NDTI.x * (1 - conser_upper) + test1$NDTI.y * (conser_upper)

test2 <- dplyr::filter(test1, Crop.x == "Almira_bottom" & Crop.y == "Wheat Pritchett")

test3 <- test2[c(4,38:43)] ## select the conven,med,conser colums

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
  labs(y = "Fraction Residue Cover", x = "NDTI") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  theme(text = element_text(size = 20),legend.position = c(0.8, 0.2),
        legend.title=element_blank(),
        legend.margin=margin(c(1,5,5,5)))


##
Intercept_Slope <- data.frame()
for (i in unique(test3$RWC.x)) {
  z <- dplyr::filter(test3, RWC.x == i)
  RWC <- z$RWC.x
  ResidueCover <- z$Fraction_Residue_Cover
  CAI <- z$CAI
  linear <- lm(ResidueCover ~ CAI)
  Value <- cbind(RWC, ResidueCover, CAI, linear$coefficients[1], linear$coefficients[2])
  Intercept_Slope <- rbind(Intercept_Slope, Value)
}

names(Intercept_Slope)[4] <- "Intercept"
names(Intercept_Slope)[5] <- "Slope"
Intercept_Slope <- Intercept_Slope %>%
  mutate_at(c(1:5), as.numeric)
Intercept_Slope$CAI <- round(Intercept_Slope$CAI, digit = 2)
Intercept_Slope$Intercept <- round(Intercept_Slope$Intercept, digit = 2)
Intercept_Slope$Slope <- round(Intercept_Slope$Slope, digit = 2)


Intercept_Slope$Class <- ifelse(Intercept_Slope$RWC <= 0.40, "A", ifelse(Intercept_Slope$RWC > 0.40 & Intercept_Slope$RWC <= 0.72, "B", "C"))

Intercept_Slope_Result <- Intercept_Slope %>%
  group_by(ResidueCover, Class) %>%
  summarise_all(.funs = c("median"))
Intercept_Slope_Result <- Intercept_Slope_Result[-c(3)]
