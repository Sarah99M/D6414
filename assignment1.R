# Load the dataset
data <- read.csv("C:/Users/HONOR/Desktop/Set1_assignment1.csv")
# Check the data and find that row 106 has missing values in glu and pot, and sod is an outlier
# Delete row 106
data <- data[-106, ]
# Count missing values in each column
colSums(is.na(data))

# 1. Handle missing values in the age column (age variable, using median to fill)
if (any(is.na(data$age))) {
  data$age[is.na(data$age)] <- median(data$age, na.rm = TRUE)
}
# Check missing values in each column
colSums(is.na(data))

# 2. Handle missing values in glu and pot columns (blood glucose variable, using mice package for multiple imputation)
install.packages("mice")
library(mice)
install.packages("dplyr")
library(dplyr)
## Convert the value 47 in the pot column to NA
data$pot[data$pot == 47] <- NA
#Convert data$glu and data$pot to data frames
dataglu<-data.frame(glu=data$glu)
datapot<-data.frame(pot=data$pot)
dataglupot<-cbind(dataglu, datapot)## Combine the two columns into a single data frame
## Use mice for imputation
dataglupot_imputed<-mice::mice(dataglupot)
##Extract the completed dataset
dataglupot_complete<-mice::complete(dataglupot_imputed)
## View the imputed data
head(dataglupot_complete)
#Update the original data
data$glu<-dataglupot_complete$glu
data$pot<-dataglupot_complete$pot
##Check the processed data to confirm no NA values
summary(data)

#subset class0 and class1
class0<-data%>%filter(class==0)
class1<-data%>%filter(class==1)

#First process class0
##1.Using z-score
zscore<-function(x){
  out<-(x-mean(x))/sd(x)
  return(out)
}
#class0_transform<-cbind(zscore(class0[,2]),zscore(class0[,3]),zscore(class0[,4]))
#summary(class0_transform)
#class0_cleaned <- class0[abs(class0_transform) <= 2, ]
#summary(class0)
#  Standardize the glu, sod, pot three columns using Z-score
z_scores_glu <- scale(class0$glu)
z_scores_sod <- scale(class0$sod)
z_scores_pot <- scale(class0$pot)

# et Z-score threshold and remove outliers with absolute Z-score greater than 2
# Keep only the rows where all three columns have absolute Z-scores less than or equal to 2
class0_cleaned <- class0[
  abs(z_scores_glu) <= 2 & 
    abs(z_scores_sod) <= 2 & 
    abs(z_scores_pot) <= 2, 
]
# View the cleaned data
summary(class0_cleaned)

##2.Draw boxplots for each numeric variable
par(mfrow = c(2, 2))  #Set layout
boxplot(class0$age, main = "Boxplot of Age", ylab = "Years")
boxplot(class0$glu, main = "Boxplot of Glucose", ylab = "mg/dL")
boxplot(class0$sod, main = "Boxplot of Sodium", ylab = "mEq/L")
boxplot(class0$pot, main = "Boxplot of Potassium", ylab = "mEq/L")
##Use boxplot method (IQR) to identify outliers
# Define the numeric columns to analyze
num0_columns <- c("glu", "sod", "pot")

# Use IQR method to detect outliers
for (col in num0_columns) {
  Q1 <- quantile(class0[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(class0[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  # Mark outliers as NA
  class0[[col]][class0[[col]] < lower_bound | class0[[col]] > upper_bound] <- NA
}

#Delete rows containing NA
data0_cleaned <- na.omit(class0)

#View the cleaned data
cat("Data after removing outliers:\n")
print(data0_cleaned)
summary(data0_cleaned)

#Process class1
#Draw boxplots for each numeric variable
par(mfrow = c(2, 2))  #Set layout
boxplot(class1$age, main = "Boxplot of Age", ylab = "Years")
boxplot(class1$glu, main = "Boxplot of Glucose", ylab = "mg/dL")
boxplot(class1$sod, main = "Boxplot of Sodium", ylab = "mEq/L")
boxplot(class1$pot, main = "Boxplot of Potassium", ylab = "mEq/L")
#Define the numeric columns to analyze
num1_columns <- c("glu", "sod", "pot")
# Use IQR method to detect outliers
for (col in num1_columns) {
  # Calculate Q1 and Q3
  Q1 <- quantile(class1[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(class1[[col]], 0.75, na.rm = TRUE)
  
  # Calculate IQR and outlier bounds
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Mark outliers as NA
  class1[[col]][class1[[col]] < lower_bound | class1[[col]] > upper_bound] <- NA
}
# Delete rows containing NA
data1_cleaned <- na.omit(class1)
# View the cleaned data
cat("Data after removing outliers:\n")
print(data1_cleaned)
summary(data1_cleaned)

# Plot scatter plots for class0 data
par(mfrow = c(1, 3))  # Set layout to 1 row, 3 columns
plot(class0_cleaned$age, class0_cleaned$glu, main = "class0: Age vs Glucose", xlab = "Age", ylab = "Glucose")
plot(class0_cleaned$age, class0_cleaned$sod, main = "class0: Age vs Sodium", xlab = "Age", ylab = "Sodium")
plot(class0_cleaned$age, class0_cleaned$pot, main = "class0: Age vs Potassium", xlab = "Age", ylab = "Potassium")

# Plot scatter plots for class1 data
par(mfrow = c(1, 3))  # Set layout to 1 row, 3 columns again
plot(data1_cleaned$age, data1_cleaned$glu, main = "class1: Age vs Glucose", xlab = "Age", ylab = "Glucose")
plot(data1_cleaned$age, data1_cleaned$sod, main = "class1: Age vs Sodium", xlab = "Age", ylab = "Sodium")
plot(data1_cleaned$age, data1_cleaned$pot, main = "class1: Age vs Potassium", xlab = "Age", ylab = "Potassium")
