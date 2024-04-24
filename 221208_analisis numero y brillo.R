##################### libraries ##################
library(tiff)
library(fields)
library(FCSlib)

##### Functions #####
imMean <- function(img){
  di<-dim(img)
  if(length(di)!=3) return(img)
  if(di[3]<2) return(img)
  X<-di[1]
  Y<-di[2]
  Imean<-array(NA, dim = c(X,Y))
  for(i in 1:(X)){
    for(j in 1:(Y)){
      Imean[i,j]<-mean(img[i,j,], na.rm = T)
    }
  }
  return(Imean)
}
imVar <- function(img){
  di<-dim(img)
  X<-di[1]
  Y<-di[2]
  Ivar<-array(NA, dim = c(X,Y))
  for(i in 1:(X)){
    for(j in 1:(Y)){
      Ivar[i,j]<-var(img[i,j,], na.rm = T)
    }
  }
  return(Ivar)
}
NB <- function(img, var0 = 0, ofst = 0, S = 0){
  img.m <- imMean(img)
  img.v <- imVar(img)
  V  <- img.v - var0
  M  <- img.m - ofst
  V.min <- min(V)
  M.min <- min(M)
  if (V.min < 0) V <- V + abs(V.min)
  if (M.min < 0) M <- M + abs(M.min)
  B <- V/M
  N <- (M^2)/V
  B[which(B == Inf)] <- NA
  N[which(N == Inf)] <- NA
  if (S>0){
    epsilon <- B / S - 1
    n <- (N * (epsilon + 1)) / epsilon
    vsm <- V - S*M
    vsm.min <- min(vsm)
    if (vsm.min < 0) vsm <- vsm + abs(vsm.min)
    epsilon <- vsm / M
    n <- (M^2)/vsm
    epsilon[which(epsilon == Inf)] <- NA
    n[which(n == Inf)] <-NA
    return(list(n = n, epsilon = epsilon, I_Ioffset = M))
  } else {
    return(list(N = N, B = B, I_Ioffset = M))
  }
}

##### Leer los archivos #####
#setwd("G:/Mi unidad/2022/Estudiantes/Laura Martinez/220630_NBjackaton/dataset/")
setwd("G:/Mi unidad/2022/Estudiantes/Laura Martinez/220630_NBjackaton/dataset/sources rotated")


fNames<- list.files(pattern = ".tif")
dir.create("AVG")
dir.create("THR")
dir.create("N")
dir.create("B")
dir.create("epsilon")

for (f in fNames){
  img<- readFileTiff(f)
  m<- imMean(img = img)
  analisis <- NB(img)
  N <-as.matrix(analisis$N)
  B <-as.matrix(analisis$B)
  writeTIFF(m/2^16,where = paste("AVG","//AVG_",f, sep = ""),bits.per.sample = 16)
  writeTIFF(N/2^32,where = paste("N","/N_",f, sep = ""),bits.per.sample = 32)
  writeTIFF(B/2^32,where = paste("B","/B_",f, sep = ""),bits.per.sample = 32)
  print (f)
}

S<- sigma0 <- ofst <- NULL # to compute the response function of detector, see Migueles-Ramirez R. Master thesis, UNAM
mm<-nn<-bb<-NULL
mmv<-nnv<-bbv<-NULL
for (f in fNames){
  img<- readFileTiff(f)
  a<-readTIFF(paste("AVG","/AVG_",f, sep = ""), as.is = T)
   h<-readTIFF(paste("THR","/THR_",f, sep = ""), as.is = T)
  N<- readTIFF(paste("N","/N_",f, sep = ""), as.is = T)
  B<- readTIFF(paste("B","/B_",f, sep = ""), as.is = T)
   idx<-which(h == 0) # para quedarme con los indices de los valores del 'fondo'
   IVar<-imVar(img = img)
   S<- mean(B[idx], na.rm = T) #### Computes the detector's gain
   sigma0 <-mean(IVar[idx], na.rm = T)
   ofst <- 0 # mean(N[idx], na.rm = T) #### Computes the  detector's offset
   analisis <- NB(img = img, var0 = sigma0, ofst = ofst,S = S)
    
  mm<-c(mm,mean(a[which(h !=0)], na.rm = T)) #### Computes de mean of cytosol fluorescence
  nn<-c(nn,mean(analisis$n[which(h !=0)], na.rm = T)) #### Computes de mean of cytosol N
  bb<-c(bb,mean(analisis$epsilon[which(h !=0)], na.rm = T)) #### Computes de mean of cytosol N
 
  mmv<- c(mmv,mean(a[which(h ==0)], na.rm = T)) #### Computes de mean of vacuolae fluorescence
  nnv<-c(nnv,mean(analisis$n[which(h ==0)], na.rm = T)) #### Computes de mean of vacuolae N
  bbv<-c(bbv,mean(analisis$epsilon[which(h ==0)], na.rm = T)) #### Computes de mean of vacuolae N
  # # bb<-c(bb,mean(B[which(h !=0)], na.rm = T)) #### Computes de mean of cytosol N
 #  
   analisis$epsilon[idx]=0
   #B[idx]=0
   writeTIFF( analisis$epsilon/2^32,where = paste("epsilon","/epsilon_",f, sep = ""),bits.per.sample = 32)
   print (f)
}

names(mm)<-fNames
names(nn)<-fNames
names(bb)<-fNames

names(mmv)<-fNames
names(nnv)<-fNames
names(bbv)<-fNames
####################### Mean cytosolic fluorescence
fN<-fNames
####################################
mm45<-mm4H<-mmctr<-mmrc<-NULL
mm45<-mm[grep("45", fN)]
mm4H<-mm[grep("4h", fN)]
mmctr<-mm[grep("ct", fN)]
mmrc<-mm[grep("rc", fN)]
# boxplot(m45, m4H, mctr, mrc, notch = T)
# t.test(m45, m4H)
####################### Mean cytosolic B
bb45<-bb4H<-bbctr<-bbrc<-NULL
bb45<-bb[grep("45", fN)]
bb4H<-bb[grep("4h", fN)]
bbctr<-bb[grep("ct", fN)]
bbrc<-bb[grep("rc", fN)]
# boxplot(b45, b4H, bctr, brc, bdetect, notch = T)
# t.test(b45, b4H)

######################################
####################################
mmv45<-mmv4H<-mmvctr<-mmvrc<-NULL
mmv45<-mmv[grep("45", fN)]
mmv4H<-mmv[grep("4h", fN)]
mmvctr<-mmv[grep("ct", fN)]
mmvrc<-mmv[grep("rc", fN)]
# boxplot(m45, m4H, mctr, mrc, notch = T)
# t.test(m45, m4H)
####################### Mean vacuolae B
bbv45<-bbv4H<-bbvctr<-bbvrc<-NULL
bbv45<-bbv[grep("45", fN)]
bbv4H<-bbv[grep("4h", fN)]
bbvctr<-bbv[grep("ct", fN)]
bbvrc<-bbv[grep("rc", fN)]
# boxplot(b45, b4H, bctr, brc, bdetect, notch = T)
# t.test(b45, b4H)

################################
#(mfrow = c(1,2))#
layout(1)
plot(mm45, bb45, xlim = c(0,2200), ylim = c(3, 16),
     col = "#1409CC", pch = 19, cex = 1.2, cex.lab = 1.2, cex.axis = 1.2,
     xlab = "F (a.u.)", ylab = expression (epsilon))
points(mm4H, bb4H, pch = 19,cex = 1.2, col = "#AD0000")
points(mmrc, bbrc, pch = 19,cex = 1.2, col = "#007D70")
points(mmctr, bbctr, pch = 19,cex = 1.2, col = "#AFA8BA")
abline(h = 40, col = "grey", lty = 2)
 # abline(h = 150, col = "grey", lty = 2)
abline(v =3000, col = "grey", lty = 2)
legend("topright", c("45", "4H", "rc", "ctr"),
       cex = 0.8, pch = 19,
       col = c("#1409CC","#AD0000", "#007D70","#AFA8BA"))


######

t.test(bb45, bb4H)
wilcox.test(bb45, bb4H)

t.test(bb4H, bbrc)
wilcox.test(bb4H, bbrc)


t.test( bbrc, bbctr)
wilcox.test( bbrc, bbctr)



plot(density(bb45), col = "#1409CC", main = "brightness", xlab = expression(epsilon),
     xlim = c(0, 20), ylim = c(0,2), lwd = 2)
lines(density(bb4H), col = "#AD0000", lwd = 2)
lines(density(bbrc), col = "#007D70", lwd = 2)
lines(density(bbctr), col = "#AFA8BA", lwd = 2)


library("vioplot")

vioplot(bb45, bb4H, bbrc, bbctr, main = "brightness",xlab = "", ylab = expression(epsilon),
        col = c("#1409CC", "#AD0000", "#007D70",  "#AFA8BA"))
legend("topright", c("45", "4H", "rc", "ctr"),
       cex = 0.8, pch = 19,
       col = c("#1409CC","#AD0000", "#007D70","#AFA8BA"))


vioplot(mm45, mm4H, mmrc, mmctr, main = "Fluorescence",xlab = "", ylab = "a.u.f",
        col = c("#1409CC", "#AD0000", "#007D70",  "#AFA8BA"))
legend("topright", c("45", "4H", "rc", "ctr"),
       cex = 0.8, pch = 19,
       col = c("#1409CC","#AD0000", "#007D70","#AFA8BA"))

