
ny <- read.delim("E:/MiniProject/New_York.dat")
View(ny)

################################################################################################
#### Applying Mahalanobis Distance ####

# Removing the Place Column
ny1 <- ny[,c(6,2,3,4,5)]
# Calculate Mahalanobis with predictor variables
ny2 <- ny1[,-1] # Remove MALE_FEM variable
m_dist <- mahalanobis(ny2,colMeans(ny2),cov(ny2))
ny1$MD <- round(m_dist, 1)

summary(ny1$MD)
# Here we can see that mean and median values are very low as compared to median
# So in the next step let's try different values of threshold: 20,15,10,5

# Binary Outlier Variable
ny1$outlier <- "No"
ny1$outlier[ny1$MD > 20] <- "Yes"    # Threshold set to 20
# Now let's count the number of 'Yes' in the outlier column
length(which(ny1$outlier=="Yes"))
# Only 11

# Now let's set the threshold to 15
ny1$outlier <- "No"
ny1$outlier[ny1$MD > 15] <- "Yes"
length(which(ny1$outlier=="Yes"))
# 20

# Now let's set the threshold to 10
ny1$outlier <- "No"
ny1$outlier[ny1$MD > 10] <- "Yes"
length(which(ny1$outlier=="Yes"))
# 31

# Now let's set the threshold to 5
ny1$outlier <- "No"
ny1$outlier[ny1$MD > 5] <- "Yes"
length(which(ny1$outlier=="Yes"))
# 132
