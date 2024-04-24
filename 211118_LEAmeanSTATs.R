########## Functions ###############
myDotchart<-function(Et, E.ctr.m){
  a.m<-apply(Et, 2, mean, na.rm =T)/E.ctr.m
  a.sd<-apply(Et, 2, sd, na.rm =T)/E.ctr.m
  LL <- (a.m-a.sd)
  UL <-  (a.m+a.sd)
  
  dotchart(as.matrix(Et/E.ctr.m), pch  = 19, cex= 1, 
           xlab = "times",   labels="", xlim = c(1,5))
  j<-length(LL)
  
  N<-nrow(Et)
  for(i in seq(N/2, j*(N+2), N+2)){
    lines(x=c(LL[j],UL[j]), y=c(i,i), col = "red",lwd = 2)
    j <- j -1
  }
  j<-length(a.m)
  for(i in seq(N/2, j*(N+2), N+2)){
    lines(x=c(a.m[j],a.m[j]), y=c(i-1,i+1), col = "blue", lwd = 2)
    j <- j -1
  }
}
########## Load Exp 4  ############# 
# par(mfrow=c(2,2))

wdExp4<-"G:\\Mi unidad\\2021\\Colaboraciones\\Alejandra Covarruvias\\LEAs\\experimento4/"
setwd(wdExp4)
load("exp4.rData")  
E4<-apply(a, 2, mean)
E4.45<- E4[1:6]
E4.4H<- E4[7:12] 
E4.ctr<-c(E4[13:16],NA,NA)
E4.RC<- E4[17:22]
E4t<-cbind(E4.45,E4.4H,E4.ctr,E4.RC)
E4.ctr.m <- mean(E4.ctr, na.rm = T)
myDotchart(E4t, E4.ctr.m)

########## Load Exp 3  #############
wdExp3<-"G:\\Mi unidad\\2021\\Colaboraciones\\Alejandra Covarruvias\\LEAs\\experimento3/"
setwd(wdExp3)
load("exp3.rData") 
colnames(a)
E3<-apply(a, 2, mean)
E3.45<- E3[1:10]
E3.4H<- c(E3[11:19],NA) 
E3.ctr<-c(E3[20:28],NA)
E3.RC<- c(E3[29:37], NA)
E3t<-cbind(E3.45,E3.4H,E3.ctr,E3.RC)
E3.ctr.m <- mean(E3.ctr, na.rm = T)
myDotchart(E3t, E3.ctr.m)
nrow(E3t)
t.test(E3.4H, E3.RC)


########## Load Exp 2  #############

wdExp2<-"G:\\Mi unidad\\2021\\Colaboraciones\\Alejandra Covarruvias\\LEAs\\experimento2/"
setwd(wdExp2)
load("exp2.rData") 
colnames(a)
E2<-apply(a, 2, mean)
E2.45<- E2[7:12]
E2.4H<- c(E2[13:14],NA,NA,NA,NA) 
E2.ctr<-E2[18:23]
E2.RC<-E2[25:30]
E2t<-cbind(E2.45,E2.4H,E2.ctr,E2.RC)
E2.ctr.m <- mean(E2.ctr, na.rm = T)
myDotchart(E2t, E2.ctr.m)
t.test(E2.4H, E2.RC)


########## Load Exp 1  #############

wdExp1<-"G:\\Mi unidad\\2021\\Colaboraciones\\Alejandra Covarruvias\\LEAs\\experimento1/"
setwd(wdExp1)
load("exp1.rData") 
colnames(a)
E1<-apply(a, 2, mean)
E1.45<- E1[1:8]
E1.4H<- c(E1[9:11],NA,NA,NA,NA, NA) 
E1.ctr<- c(E1[12:13],NA,NA,NA,NA,NA,NA)
E1.RC<- c(E1[14:19],NA, NA)
E1t<-cbind(E1.45,E1.4H,E1.ctr,E1.RC)
E1.ctr.m <- mean(E1.ctr, na.rm = T)
myDotchart(E1t, E1.ctr.m)
t.test(E1.4H, E1.RC)
################ Comparison between biological replicates ########
LEA45<-c(mean(E1.45,na.rm = T)/ E1.ctr.m,
         mean(E2.45,na.rm = T)/ E2.ctr.m,
         mean(E3.45,na.rm = T)/ E3.ctr.m,
         mean(E4.45,na.rm = T)/ E4.ctr.m)
LEA4H<-c(mean(E1.4H,na.rm = T)/ E1.ctr.m,NA,
       #  mean(E2.4H,na.rm = T)/ E2.ctr.m,
         mean(E3.4H,na.rm = T)/ E3.ctr.m,
         mean(E4.4H,na.rm = T)/ E4.ctr.m)
h<- sd( c( E1.ctr/E1.ctr.m,
                E2.ctr/E2.ctr.m,
                E3.ctr/E3.ctr.m,
                E4.ctr/E4.ctr.m), na.rm = T)
LEARC<-c(mean(E1.RC,na.rm = T)/ E1.ctr.m,
         mean(E2.RC,na.rm = T)/ E2.ctr.m,
         mean(E3.RC,na.rm = T)/ E3.ctr.m,
         mean(E4.RC,na.rm = T)/ E4.ctr.m)

ae <- cbind(LEA45, LEA4H, LEARC)

myDotchart(ae, 1)
#abline(v= 1+h)

rect(xleft = 1-h, xright = 1+h, ybottom = 0, ytop = 18,
     density = 30, col = "gray",
     angle = -30, border = "transparent")

t.test(l4H, lRC)
t.test(l4H, l45)
t.test(lRC, l45)
