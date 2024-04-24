################## Libraries ##################
library(tiff)
################## Functions #################
computeMeanVals<-function(fName, NAME, H){
  imgStack<-readTIFF(fName,all = T, as.is = T);
  n<-length(imgStack)
  #  image(imgStack[[1]],col = gray.colors(255))
  hist(unlist(imgStack), main = NAME)
  abline(v = H, col = "blue")
 A.m<-NULL
  A.m.h<-NULL
  for(i in 1:n){
    idxNA<-which(imgStack[[i]]== Hsat)
    imgStack[[i]][idxNA]<-NaN
    A.m[i]<-mean(imgStack[[i]], na.rm = T)
    idx<-which(imgStack[[i]]> H)
    A.m.h[i]<-mean(imgStack[[i]][idx], na.rm = T)
  }
  return(data.frame(m =A.m, m.h = A.m.h  ))
}
################## dir #######################
wd<-"C:\\Users\\adang\\OneDrive\\Documentos\\LEAs\\experimento4/"
################## Constants ##################
Hsat<- 2^14
HPROB<-0.99
################## DATA ##################
setwd(wd)

nams<-list.files(path = wd, pattern = ".tif" )

NM<-gsub(".tif", "", nams)
dat<-list(NULL)
count<-1
for(nm in nams){
  print(nm)
  dat[[count]]<-computeMeanVals(nm, NAME = nm, H=0)
  count<-count + 1
}

names(dat)<-NM

a<-NULL
for( nm in NM){
 a<- cbind(a, dat[[nm]]$m)
}

colnames(a)<-NM
head(a)
boxplot(a, las = 2, notch = T)

 save(a,file = "exp4.rData")
# load("exp3.rData")

