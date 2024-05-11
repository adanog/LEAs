########## Functions ###############
myDotchart <- function(Et, E.ctr.m){
  a.m <- apply(Et, 2, mean, na.rm = TRUE) / E.ctr.m  # Calculate normalized mean across columns
  a.sd <- apply(Et, 2, sd, na.rm = TRUE) / E.ctr.m  # Calculate normalized standard deviation across columns
  LL <- (a.m - a.sd)  # Lower limit for error bars
  UL <- (a.m + a.sd)  # Upper limit for error bars
  
  dotchart(as.matrix(Et / E.ctr.m), pch = 19, cex = 1, 
           xlab = "times", labels = "", xlim = c(1, 5))  # Plot normalized data as a dot chart
  j <- length(LL)
  
  N <- nrow(Et)  # Number of rows in data
  for(i in seq(N / 2, j * (N + 2), N + 2)){
    lines(x = c(LL[j], UL[j]), y = c(i, i), col = "red", lwd = 2)  # Draw error bars in red
    j <- j - 1
  }
  j <- length(a.m)
  for(i in seq(N / 2, j * (N + 2), N + 2)){
    lines(x = c(a.m[j], a.m[j]), y = c(i - 1, i + 1), col = "blue", lwd = 2)  # Draw mean value lines in blue
    j <- j - 1
  }
}

########## Load Experiment 4 ############# 
# Set layout for multiple plots (currently commented out)
# par(mfrow = c(2, 2))

wdExp4 <- "G:\\...\\experiment4/"
setwd(wdExp4)  # Set working directory for experiment 4
load("exp4.rData")  # Load experiment 4 data
E4 <- apply(a, 2, mean)  # Calculate column means of data matrix 'a'
E4.45 <- E4[1:6]
E4.4H <- E4[7:12]
E4.ctr <- c(E4[13:16], NA, NA)  # Center control with missing values filled
E4.RC <- E4[17:22]
E4t <- cbind(E4.45, E4.4H, E4.ctr, E4.RC)  # Combine subsets into a new matrix
E4.ctr.m <- mean(E4.ctr, na.rm = TRUE)  # Calculate mean of center control, removing NAs
myDotchart(E4t, E4.ctr.m)  # Generate dot chart for Experiment 4

########## Load Experiment 3 #############
wdExp3 <- "G:\\...\\experiment3/"
setwd(wdExp3)  # Set working directory for experiment 3
load("exp3.rData")
colnames(a)  # Display column names of data matrix 'a'
E3 <- apply(a, 2, mean)
E3.45 <- E3[1:10]
E3.4H <- c(E3[11:19], NA)  # Add missing value to align vector lengths
E3.ctr <- c(E3[20:28], NA)
E3.RC <- c(E3[29:37], NA)
E3t <- cbind(E3.45, E3.4H, E3.ctr, E3.RC)  # Combine subsets into a new matrix
E3.ctr.m <- mean(E3.ctr, na.rm = TRUE)  # Calculate mean of center control, removing NAs
myDotchart(E3t, E3.ctr.m)  # Generate dot chart for Experiment 3
nrow(E3t)  # Display number of rows in E3t
t.test(E3.4H, E3.RC)  # Perform t-test between two groups in Experiment 3

########## Load Experiment 2 #############

wdExp2 <- "G:\\...\\experiment2/"
setwd(wdExp2)  # Set working directory for experiment 2
load("exp2.rData")
colnames(a)
E2 <- apply(a, 2, mean)
E2.45 <- E2[7:12]
E2.4H <- c(E2[13:14], NA, NA, NA, NA)  # Fill with NAs to ensure vector length consistency
E2.ctr <- E2[18:23]
E2.RC <- E2[25:30]
E2t <- cbind(E2.45, E2.4H, E2.ctr, E2.RC)  # Combine subsets into a new matrix
E2.ctr.m <- mean(E2.ctr, na.rm = TRUE)  # Calculate mean of center control, removing NAs
myDotchart(E2t, E2.ctr.m)  # Generate dot chart for Experiment 2
t.test(E2.4H, E2.RC)  # Perform t-test between two groups in Experiment 2

########## Load Experiment 1 #############

wdExp1 <- "G:\\...\\experiment1/"
setwd(wdExp1)  # Set working directory for experiment 1
load("exp1.rData")
colnames(a)
E1 <- apply(a, 2, mean)
E1.45 <- E1[1:8]
E1.4H <- c(E1[9:11], NA, NA, NA, NA, NA)  # Fill with NAs to align vector lengths
E1.ctr <- c(E1[12:13], NA, NA, NA, NA, NA, NA)
E1.RC <- c(E1[14:19], NA, NA)
E1t <- cbind(E1.45, E1.4H, E1.ctr, E1.RC)  # Combine subsets into a new matrix
E1.ctr.m <- mean(E1.ctr, na.rm = TRUE)  # Calculate mean of center control, removing NAs
myDotchart(E1t, E1.ctr.m)  # Generate dot chart for Experiment 1
t.test(E1.4H, E1.RC)  # Perform t-test between two groups in Experiment 1

################ Comparison between biological replicates ########
LEA45 <- c(mean(E1.45, na.rm = TRUE) / E1.ctr.m,
           mean(E2.45, na.rm = TRUE) / E2.ctr.m,
           mean(E3.45, na.rm = TRUE) / E3.ctr.m,
           mean(E4.45, na.rm = TRUE) / E4.ctr.m)
LEA4H <- c(mean(E1.4H, na.rm = TRUE) / E1.ctr.m, NA,
           # mean(E2.4H, na.rm = TRUE) / E2.ctr.m,  # Commented out, perhaps due to missing values
           mean(E3.4H, na.rm = TRUE) / E3.ctr.m,
           mean(E4.4H, na.rm = TRUE) / E4.ctr.m)
h <- sd(c(E1.ctr / E1.ctr.m,
          E2.ctr / E2.ctr.m,
          E3.ctr / E3.ctr.m,
          E4.ctr / E4.ctr.m), na.rm = TRUE)
LEARC <- c(mean(E1.RC, na.rm = TRUE) / E1.ctr.m,
           mean(E2.RC, na.rm = TRUE) / E2.ctr.m,
           mean(E3.RC, na.rm = TRUE) / E3.ctr.m,
           mean(E4.RC, na.rm = TRUE) / E4.ctr.m)

ae <- cbind(LEA45, LEA4H, LEARC)  # Combine LEA data into a matrix

myDotchart(ae, 1)  # Generate dot chart for aggregated experiment data
# Highlight a range around 1 with a gray rectangle (currently commented out)
# abline(v = 1 + h)

rect(xleft = 1 - h, xright = 1 + h, ybottom = 0, ytop = 18,
     density = 30, col = "gray",
     angle = -30, border = "transparent")

t.test(l4H, lRC)  # Perform t-test (variable names may need to be defined or corrected)
t.test(l4H, l45)  # Perform t-test (variable names may need to be defined or corrected)
t.test(lRC, l45)  # Perform t-test (variable names may need to be defined or corrected)
