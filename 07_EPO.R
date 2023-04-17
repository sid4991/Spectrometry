eposvd <- function(D, nlv) {
  D <- .mat(D) #2151 *8 p*n
  zdim <- dim(D)
  m <- zdim[1]
  p <- zdim[2]
  nlv <- min(nlv, m, p)
  I <- diag(1, nrow = p, ncol = p)
  if(nlv == 0) {
    M <- I
    P <- NULL
  }
  else {
    P <- svd(D)$v[, 1:nlv, drop = FALSE] ## n*k
    M <- I - tcrossprod(P)
  }
  list(M = M, P = P)
}

## Calculate the difference matrix D.
PeasResidue <- Residue_Median %>%
                 filter(Crop=="Peas") %>% select(-starts_with("RWC")) %>%
                 pivot_wider(names_from = "Scan", values_from  = "Reflectance")

PeasResidue <- na.omit(PeasResidue)

A <- as.matrix(PeasResidue[4:12])
X <- as.matrix(PeasResidue[4:11]) ## spectra with moisture


D <-(sweep(A, MARGIN = 1, STATS = A[,9], FUN = "-"))
D <- D[,1:8] ## Differential matrix [Scan9-Scan(1:8)]
D <- -(D)


## Perform singular value decomposition on matrix D to obtain matrix V.
nlv <- 2
res <- eposvd(D, nlv = nlv)
V <- res$M       # orthogonalization matrix
P <- res$P       # detrimental directions matrix (loadings of D = columns of P)

V
P

X_corr <- X %*% V
X_corr %*% P

X_corr1 <- cbind(PeasResidue$Wvl,as.data.frame(X_corr))
X_corr1 <- cbind(X_corr1,PeasResidue$Scan9)
colnames(X_corr1)<- c("Wvl","Scan1","Scan2","Scan3","Scan4","Scan5","Scan6","Scan7","Scan8","Dry")

X_corr1 <- melt(X_corr1 ,  id.vars = 'Wvl', variable.name = 'Scan')

ggplot(X_corr1, aes(Wvl, value)) +
  geom_line(aes(colour = Scan))+
  scale_x_continuous(breaks = seq(400, 2400, 100), guide = guide_axis(angle = 90)) +
  geom_vline(xintercept = 1400, linetype="dashed",
             color = "blue", size=1)+
  geom_vline(xintercept = 1700, linetype="dashed",
            color = "red", size=1)+
  geom_vline(xintercept = 1950, linetype="dashed",
            color = "green", size=1)+
  labs(x = "Wavelength (nm)", y = "Spectral Reflectance (%)") +
  theme(text = element_text(size = 20))+theme_bw()


### Soil Merged together
CanolaResidue <- Soil_Median[-c(2,4,6)] %>%
  group_by(Wvl,Scan) %>%
  summarise_all(.funs = c("median"))%>%
  pivot_wider(names_from = "Scan", values_from  = "Reflectance")

CanolaResidue <- na.omit(CanolaResidue)

A <- as.matrix(CanolaResidue[2:10])
X <- as.matrix(CanolaResidue[2:9]) ## spectra with moisture


D <-(sweep(A, MARGIN = 1, STATS = A[,9], FUN = "-"))
D <- D[,1:8] ## Differential matrix [Scan9-Scan(1:8)]
D <- -(D)


## Perform singular value decomposition on matrix D to obtain matrix V.
nlv <- 2
res <- eposvd(D, nlv = nlv)
V <- res$M       # orthogonalization matrix
P <- res$P       # detrimental directions matrix (loadings of D = columns of P)

V
P

X_corr <- X %*% V
X_corr %*% P

X_corr1 <- cbind(CanolaResidue$Wvl,as.data.frame(X_corr))
X_corr1 <- cbind(X_corr1,CanolaResidue$Scan9)
colnames(X_corr1)<- c("Wvl","Scan1","Scan2","Scan3","Scan4","Scan5","Scan6","Scan7","Scan8","Dry")

X_corr1 <- melt(X_corr1 ,  id.vars = 'Wvl', variable.name = 'Scan')

ggplot(X_corr1, aes(Wvl, value)) +
  geom_line(aes(colour = Scan))+
  geom_label(aes(label = as.factor(Scan)),data = X_corr1 %>% filter(Wvl == max(Wvl)),nudge_x = 0.05,size = 4) +
  scale_x_continuous(breaks = seq(400, 2400, 100), guide = guide_axis(angle = 90)) +
  geom_vline(xintercept = 1400, linetype="dashed",
             color = "blue", size=1)+
  geom_vline(xintercept = 1700, linetype="dashed",
             color = "red", size=1)+
  geom_vline(xintercept = 1950, linetype="dashed",
             color = "green", size=1)+
  labs(x = "Wavelength (nm)", y = "Spectral Reflectance (%)",title = "Soil EPO") +
  theme(text = element_text(size = 20))+theme_bw()

### Crop Merged together
CanolaResidue <- Residue_Median[-c(2,4,6)] %>%
  group_by(Wvl,Scan) %>%
  summarise_all(.funs = c("median"))%>%
  pivot_wider(names_from = "Scan", values_from  = "Reflectance")

CanolaResidue <- na.omit(CanolaResidue)

A <- as.matrix(CanolaResidue[2:10])
X <- as.matrix(CanolaResidue[2:9]) ## spectra with moisture


D <-(sweep(A, MARGIN = 1, STATS = A[,9], FUN = "-"))
D <- D[,1:8] ## Differential matrix [Scan9-Scan(1:8)]
D <- -(D)


## Perform singular value decomposition on matrix D to obtain matrix V.
nlv <- 2
res <- eposvd(D, nlv = nlv)
V <- res$M       # orthogonalization matrix
P <- res$P       # detrimental directions matrix (loadings of D = columns of P)

V
P

X_corr <- X %*% V
X_corr %*% P

X_corr1 <- cbind(CanolaResidue$Wvl,as.data.frame(X_corr))
X_corr1 <- cbind(X_corr1,CanolaResidue$Scan9)
colnames(X_corr1)<- c("Wvl","Scan1","Scan2","Scan3","Scan4","Scan5","Scan6","Scan7","Scan8","Dry")

colnames(X_corr1)<- c("Wvl","Scan4","Scan5","Scan6","Dry")

X_corr1 <- melt(X_corr1 ,  id.vars = 'Wvl', variable.name = 'Scan')

ggplot(X_corr1, aes(Wvl, value)) +
  geom_line(aes(colour = Scan))+
  geom_label(aes(label = as.factor(Scan)),data = X_corr1 %>% filter(Wvl == max(Wvl)),parse=T,nudge_x = 0.55,size = 4) +
  scale_x_continuous(breaks = seq(400, 2400, 100), guide = guide_axis(angle = 90)) +
  geom_vline(xintercept = 1400, linetype="dashed",
             color = "blue", size=1)+
  geom_vline(xintercept = 1700, linetype="dashed",
             color = "red", size=1)+
  geom_vline(xintercept = 1950, linetype="dashed",
             color = "green", size=1)+
  labs(x = "Wavelength (nm)", y = "Spectral Reflectance (%)",title = "Crop Residue EPO") +
  theme(text = element_text(size = 20))+theme_bw()

