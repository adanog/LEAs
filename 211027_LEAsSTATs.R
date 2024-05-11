################## Libraries ##################
library(tiff)  # Load the 'tiff' library to handle TIFF files

################## Functions ##################
computeMeanVals <- function(fName, NAME, H){
  imgStack <- readTIFF(fName, all = TRUE, as.is = TRUE)  # Read all TIFF images from file as they are
  n <- length(imgStack)  # Get the number of images in the stack
  # Plot the first image in grayscale (currently commented out)
  # image(imgStack[[1]], col = gray.colors(255))
  hist(unlist(imgStack), main = NAME)  # Create a histogram of the pixel values across all images
  abline(v = H, col = "blue")  # Add a vertical line on the histogram at value H

  A.m <- NULL  # Initialize empty vector for storing mean values
  A.m.h <- NULL  # Initialize empty vector for storing mean values above threshold H
  for(i in 1:n){
    idxNA <- which(imgStack[[i]] == Hsat)  # Find pixels with value equal to Hsat
    imgStack[[i]][idxNA] <- NaN  # Set these pixels to NaN
    A.m[i] <- mean(imgStack[[i]], na.rm = TRUE)  # Calculate mean ignoring NaNs
    idx <- which(imgStack[[i]] > H)  # Find pixels greater than threshold H
    A.m.h[i] <- mean(imgStack[[i]][idx], na.rm = TRUE)  # Calculate mean of these pixels, ignoring NaNs
  }
  return(data.frame(m = A.m, m.h = A.m.h))  # Return a dataframe of mean values
}

################## Directory ##################
wd <- "C:\\Users\\...\\experimentN/"  # Set working directory to a specific folder

################## Constants ##################
Hsat <- 2^14  # Define saturation threshold (maximum pixel value)
HPROB <- 0.99  # Define a probability threshold (not used in this script)

################## DATA ##################
setwd(wd)  # Change the working directory

nams <- list.files(path = wd, pattern = ".tif")  # List all TIFF files in the working directory

NM <- gsub(".tif", "", nams)  # Remove the '.tif' extension from file names
dat <- list(NULL)  # Initialize a list to store data
count <- 1  # Initialize a counter
for(nm in nams){
  print(nm)  # Print the file name
  dat[[count]] <- computeMeanVals(nm, NAME = nm, H=0)  # Compute mean values for the file and store in list
  count <- count + 1  # Increment the counter
}

names(dat) <- NM  # Set names of the list elements as the file names without extension

a <- NULL  # Initialize matrix to store aggregated data
for(nm in NM){
  a <- cbind(a, dat[[nm]]$m)  # Bind columns of mean values from each image stack
}

colnames(a) <- NM  # Set column names of the matrix to file names
head(a)  # Display the first few rows of the matrix
boxplot(a, las = 2, notch = TRUE)  # Create a boxplot of the data

save(a, file = "exp4.rData")  # Save the data matrix to a file
# load("exp3.rData")  # Load a data file (commented out)
