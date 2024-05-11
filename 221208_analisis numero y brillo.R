##################### Libraries ##################
library(tiff)  # Load the 'tiff' library to handle TIFF image files
library(fields)  # Load the 'fields' library for tools for spatial data
library(FCSlib)  # Load the 'FCSlib' library, potentially for fluorescence correlation spectroscopy

##### Functions #####
imMean <- function(img){
  di <- dim(img)  # Get dimensions of the image
  if(length(di) != 3) return(img)  # Return image if not a 3D array
  if(di[3] < 2) return(img)  # Return image if there is less than two layers in the third dimension
  X <- di[1]  # Number of rows
  Y <- di[2]  # Number of columns
  Imean <- array(NA, dim = c(X, Y))  # Initialize an array for the mean image
  for(i in 1:X){
    for(j in 1:Y){
      Imean[i, j] <- mean(img[i, j, ], na.rm = TRUE)  # Calculate mean for each pixel over layers
    }
  }
  return(Imean)  # Return the mean image array
}

imVar <- function(img){
  di <- dim(img)  # Get dimensions of the image
  X <- di[1]  # Number of rows
  Y <- di[2]  # Number of columns
  Ivar <- array(NA, dim = c(X, Y))  # Initialize an array for the variance image
  for(i in 1:X){
    for(j in 1:Y){
      Ivar[i, j] <- var(img[i, j, ], na.rm = TRUE)  # Calculate variance for each pixel over layers
    }
  }
  return(Ivar)  # Return the variance image array
}

NB <- function(img, var0 = 0, ofst = 0, S = 0){
  img.m <- imMean(img)  # Calculate mean image
  img.v <- imVar(img)  # Calculate variance image
  V <- img.v - var0  # Adjust variance by subtracting baseline variance
  M <- img.m - ofst  # Adjust mean by subtracting baseline offset
  V.min <- min(V)  # Find minimum value of V
  M.min <- min(M)  # Find minimum value of M
  if (V.min < 0) V <- V + abs(V.min)  # Ensure V is non-negative
  if (M.min < 0) M <- M + abs(M.min)  # Ensure M is non-negative
  B <- V / M  # Calculate the ratio of variance to mean
  N <- (M^2) / V  # Calculate N parameter (inverse of coefficient of variation)
  B[which(B == Inf)] <- NA  # Replace Inf values with NA in B
  N[which(N == Inf)] <- NA  # Replace Inf values with NA in N
  if (S > 0){
    epsilon <- B / S - 1  # Calculate epsilon parameter
    n <- (N * (epsilon + 1)) / epsilon  # Recalculate n considering S
    vsm <- V - S * M  # Calculate variance minus S times mean
    vsm.min <- min(vsm)
    if (vsm.min < 0) vsm <- vsm + abs(vsm.min)
    epsilon <- vsm / M
    n <- (M^2) / vsm
    epsilon[which(epsilon == Inf)] <- NA
    n[which(n == Inf)] <- NA
    return(list(n = n, epsilon = epsilon, I_Ioffset = M))
  } else {
    return(list(N = N, B = B, I_Ioffset = M))
  }
}

##### File Handling and Processing #####
setwd("G:/.../dataset/")  # Set the working directory

fNames <- list.files(pattern = ".tif")  # List all TIFF files in the directory
# Create directories for storing output images
dir.create("AVG")
dir.create("THR")
dir.create("N")
dir.create("B")
dir.create("epsilon")

for (f in fNames){
  img <- readFileTiff(f)  # Read the TIFF file
  m <- imMean(img = img)  # Calculate mean image
  analysis <- NB(img)  # Perform N and B analysis
  N <- as.matrix(analysis$N)  # Convert N values to matrix
  B <- as.matrix(analysis$B)  # Convert B values to matrix
  # Save the processed images with specified bit depth
  writeTIFF(m / 2^16, where = paste("AVG", "//AVG_", f, sep = ""), bits.per.sample = 16)
  writeTIFF(N / 2^32, where = paste("N", "/N_", f, sep = ""), bits.per.sample = 32)
  writeTIFF(B / 2^32, where = paste("B", "/B_", f, sep = ""), bits.per.sample = 32)
  print(f)  # Print the file name being processed
}

S <- sigma0 <- ofst <- NULL  # Initialize variables for detector response computation, per Migueles-Ramirez's Master thesis
mm <- nn <- bb <- NULL  # Initialize vectors to store means for cytosol, N, and epsilon
mmv <- nnv <- bbv <- NULL  # Initialize vectors to store means for vacuoles

for (f in fNames){
  img <- readFileTiff(f)  # Read TIFF file
  a <- readTIFF(paste("AVG", "/AVG_", f, sep = ""), as.is = TRUE)  # Read averaged image
  h <- readTIFF(paste("THR", "/THR_", f, sep = ""), as.is = TRUE)  # Read threshold image
  N <- readTIFF(paste("N", "/N_", f, sep = ""), as.is = TRUE)  # Read N image
  B <- readTIFF(paste("B", "/B_", f, sep = ""), as.is = TRUE)  # Read B image
  idx <- which(h == 0)  # Identify indices of background values

  IVar <- imVar(img = img)  # Calculate variance image
  S <- mean(B[idx], na.rm = TRUE)  # Compute the detector's gain from background B values
  sigma0 <- mean(IVar[idx], na.rm = TRUE)  # Compute baseline variance from background
  ofst <- 0  # Set offset to 0 (mean(N[idx], na.rm = TRUE) could be used if non-zero)

  analysis <- NB(img = img, var0 = sigma0, ofst = ofst, S = S)  # Run NB analysis with adjusted parameters
  
  # Compute means for cytosolic and vacuolar fluorescence and store them
  mm <- c(mm, mean(a[which(h != 0)], na.rm = TRUE))
  nn <- c(nn, mean(analysis$n[which(h != 0)], na.rm = TRUE))
  bb <- c(bb, mean(analysis$epsilon[which(h != 0)], na.rm = TRUE))
  
  mmv <- c(mmv, mean(a[which(h == 0)], na.rm = TRUE))
  nnv <- c(nnv, mean(analysis$n[which(h == 0)], na.rm = TRUE))
  bbv <- c(bbv, mean(analysis$epsilon[which(h == 0)], na.rm = TRUE))

  analysis$epsilon[idx] = 0  # Set epsilon to zero at background indices
  writeTIFF(analysis$epsilon / 2^32, where = paste("epsilon", "/epsilon_", f, sep = ""), bits.per.sample = 32)
  print(f)  # Print the file name being processed
}

names(mm) <- fNames  # Assign file names as names for mean vectors
names(nn) <- fNames
names(bb) <- fNames

names(mmv) <- fNames
names(nnv) <- fNames
names(bbv) <- fNames

####################### Mean cytosolic fluorescence
fN <- fNames  # Use file names for analysis
####################################
# Segmenting mean vectors by file name patterns
mm45 <- mm[grep("45", fN)]
mm4H <- mm[grep("4h", fN)]
mmctr <- mm[grep("ct", fN)]
mmrc <- mm[grep("rc", fN)]

####################### Mean cytosolic B
# Similar segmentation for B
bb45 <- bb[grep("45", fN)]
bb4H <- bb[grep("4h", fN)]
bbctr <- bb[grep("ct", fN)]
bbrc <- bb[grep("rc", fN)]

####################################
# Segmenting mean vectors for vacuoles by file name patterns
mmv45 <- mmv[grep("45", fN)]
mmv4H <- mmv[grep("4h", fN)]
mmvctr <- mmv[grep("ct", fN)]
mmvrc <- mmv[grep("rc", fN)]

####################### Mean vacuolae B
bbv45 <- bbv[grep("45", fN)]
bbv4H <- bbv[grep("4h", fN)]
bbvctr <- bbv[grep("ct", fN)]
bbvrc <- bbv[grep("rc", fN)]

################################
#(mfrow = c(1,2))#
layout(1)  # Set the layout for plots
# Plotting points for different conditions with custom settings
plot(mm45, bb45, xlim = c(0,2200), ylim = c(3, 16),
     col = "#1409CC", pch = 19, cex = 1.2, cex.lab = 1.2, cex.axis = 1.2,
     xlab = "F (a.u.)", ylab = expression(epsilon))
points(mm4H, bb4H, pch = 19, cex = 1.2, col = "#AD0000")
points(mmrc, bbrc, pch = 19, cex = 1.2, col = "#007D70")
points(mmctr, bbctr, pch = 19, cex = 1.2, col = "#AFA8BA")
abline(h = 40, col = "grey", lty = 2)  # Horizontal reference line
abline(v = 3000, col = "grey", lty = 2)  # Vertical reference line
legend("topright", c("45", "4H", "rc", "ctr"),
       cex = 0.8, pch = 19,
       col = c("#1409CC","#AD0000", "#007D70","#AFA8BA"))

#### Statistical Tests ####
t.test(bb45, bb4H)
wilcox.test(bb45, bb4H)

t.test(bb4H, bbrc)
wilcox.test(bb4H, bbrc)

t.test(bbrc, bbctr)
wilcox.test(bbrc, bbctr)

# Plotting density plots for B values with different colors
plot(density(bb45), col = "#1409CC", main = "Brightness", xlab = expression(epsilon),
     xlim = c(0, 20), ylim = c(0, 2), lwd = 2)
lines(density(bb4H), col = "#AD0000", lwd = 2)
lines(density(bbrc), col = "#007D70", lwd = 2)
lines(density(bbctr), col = "#AFA8BA", lwd = 2)

library("vioplot")  # Load library for violin plots
# Generating violin plots for brightness comparisons
vioplot(bb45, bb4H, bbrc, bbctr, main = "Brightness", xlab = "", ylab = expression(epsilon),
        col = c("#1409CC", "#AD0000", "#007D70",  "#AFA8BA"))
legend("topright", c("45", "4H", "rc", "ctr"),
       cex = 0.8, pch = 19,
       col = c("#1409CC","#AD0000", "#007D70","#AFA8BA"))

# Generating violin plots for fluorescence comparisons
vioplot(mm45, mm4H, mmrc, mmctr, main = "Fluorescence", xlab = "", ylab = "a.u.f",
        col = c("#1409CC", "#AD0000", "#007D70",  "#AFA8BA"))
legend("topright", c("45", "4H", "rc", "ctr"),
       cex = 0.8, pch = 19,
       col = c("#1409CC","#AD0000", "#007D70","#AFA8BA"))

