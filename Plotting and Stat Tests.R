##### Data Plotting #####
# Load required libraries
library(ggplot2)    # for plotting data
library(dplyr)      # for data manipulation
library(forcats)    # for factors management
library(hrbrthemes) # for themes in ggplot
library(viridis)    # for color palettes

# Load data from a text file
data <- read.table("Violin Data.txt")

# Create a violin plot with grouping by a variable called 'leaf'
data %>%
  mutate(V3 = fct_reorder(V3, V1)) %>%      # Reorder V3 based on the values in V1
  mutate(V2 = factor(V2, levels = c("45","4H","RC"))) %>%  # Set factor levels for V2
  mutate(V3 = factor(V3, levels = c("H1","H2","H3","H4","H5"))) %>%  # Set factor levels for V3
  ggplot(aes(fill = V3, y = V1, x = V2)) +  # Map fill, x, and y aesthetics to columns in data
  geom_violin(scale = "width", alpha = 0.6) +  # Add violin plot with transparency
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.2, notch = FALSE, alpha = 0.3) +  # Overlay boxplot
  scale_fill_viridis_d(name = "V3") +  # Use viridis color scale
  theme_ipsum() +  # Apply hrbrthemes theme
  ylab("") +  # Empty y-axis label
  xlab("") +  # Empty x-axis label
  ylim(0,20)  # Set y-axis limits

# Create an ungrouped violin plot
data %>%
  mutate(V2 = fct_reorder(V2, V1)) %>%  # Reorder V2 based on the values in V1
  mutate(V2 = factor(V2, levels = c("45","4H","RC"))) %>%  # Set factor levels for V2
  ggplot(aes(y = V1, x = V2)) +  # Map y and x aesthetics to columns in data
  geom_violin(scale = "width", alpha = 0.6) +  # Add violin plot with transparency
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.2, notch = TRUE, alpha = 0.3) +  # Overlay boxplot with notches
  scale_fill_viridis_d(name = "V1") +  # Use viridis color scale
  theme_ipsum() +  # Apply hrbrthemes theme
  ylab("") +  # Empty y-axis label
  xlab("") +  # Empty x-axis label
  ylim(0,25)  # Set y-axis limits

##### Statistical Testing #####
# Load statistical data from a text file
data <- read.table("Stat Data.txt", header = TRUE)  # Load data with headers
head(data)  # Display the first few rows of the data

# Perform t-tests between different groups
t.test(data$X45, data$X4H)  # T-test between group X45 and X4H
t.test(data$X45, data$RC)   # T-test between group X45 and RC
t.test(data$X4H, data$RC)   # T-test between group X4H and RC

# Perform Wilcoxon tests between different groups
wilcox.test(data$X45, data$X4H)  # Wilcoxon test between group X45 and X4H
wilcox.test(data$X45, data$RC)   # Wilcoxon test between group X45 and RC
wilcox.test(data$X4H, data$RC)   # Wilcoxon test between group X4H and RC
