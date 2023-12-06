#importing dataset
df_merged <- read.csv("~/df_merged.csv")
#importing libraries needed
library(ggplot2)
library(dplyr)
#subsetting data to only include total reseipts and income
subset_data <- df_merged %>% select(Median.Household.Income, Total.Receipts)
subset_data <- subset_data[complete.cases(subset_data), ]
#making sure all variables are numeric
subset_data$Median.Household.Income <- as.numeric(subset_data$Median.Household.Income)
#creating a scatterplot with the chosen variables
ggplot(subset_data, aes(x = Median.Household.Income, y = Total.Receipts)) + geom_point() + labs(x = "Median Consumer Income", y = "Total Alcohol Receipts") + ggtitle("Median Consumer Income vs. Total Alcohol Receipts")
#finding the correlation coefficent to see how weak or strong the relationship is 
correlation <- cor(subset_data$Median.Household.Income, subset_data$Total.Receipts)
print(paste("Correlation coefficient:", correlation))