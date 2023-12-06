library(ggplot2)
head(df_merged)
df_merged$

  # Create a scatter plot using base R plot function
plot(df_merged$Responsibility.Time.Difference..Days., log(df_merged$Total.Receipts),
       main = "Scatter Plot of Responsibility Time Difference vs. Total Receipts",
       xlab = "Responsibility Time Difference (Days)",
       ylab = "Total Receipts")

# Convert the column to numeric
df_merged$Responsibility.Time.Difference..Days. <- as.numeric(df_merged$Responsibility.Time.Difference..Days.)

# Load necessary libraries
library(ggplot2)

# Load necessary libraries
library(dplyr)

# Assuming df_merged has columns Beer.Receipts, Wine.Receipts, Liquor.Receipts

# Convert columns to numeric
df_merged <- df_merged %>%
  mutate_at(vars(Beer.Receipts, Wine.Receipts, Liquor.Receipts), as.numeric)
########
library(tidyr)
df_long <- df_merged %>%
  select('Beer Receipts', 'Wine Receipts', 'Liquor Receipts') %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Sales")

# Create a bar chart for beer, wine, and liquor sales
ggplot(df_long, aes(x = Category, y = Sales, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Beer, Wine, and Liquor Sales",
       x = "Category",
       y = "Sales") +
  scale_fill_manual(values = c("Beer.Receipts" = "darkblue", "Wine.Receipts" = "brown", "Liquor.Receipts" = "pink")) +
  theme_minimal()
###Barplot for hypothesis 4
ggplot(df_long, aes(x = Category, y = log(Sales), fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Beer, Wine, and Liquor Sales",
       x = "Category",
       y = "Log Sales") +
  scale_fill_manual(
    values = c("Beer Receipts" = "darkblue", "Wine Receipts" = "pink", "Liquor Receipts" = "brown"),
    name = "Category",  # Specify legend title
    labels = c("Beer", "Liquor", "Wine")  # Specify legend labels
  ) +
  theme_minimal()
##########
#Receipt Type Visualization
# Filter columns and gather data in long format
df_long <- df_selected %>%
  select('ZIP CODES', 'Total Receipts', 'Beer Receipts', 'Wine Receipts', 'Liquor Receipts') %>%
  pivot_longer(cols = c('Beer Receipts', 'Wine Receipts', 'Liquor Receipts'), names_to = "Receipt Type", values_to = "Receipts")

# Create a bar plot
ggplot(df_long, aes(x = factor(`ZIP CODES`), y = log(Receipts), fill = `Receipt Type`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Receipt Types by Zip Code",
       x = "Zip Code",
       y = "Log Total Receipts",
       fill = "Receipt Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Receipt Type"))
###########
###Hypothesis 3 visualization
# Create a hexbin plot
ggplot(df_merged, aes(x = 'Responsibility Time Difference in Days', y = log('Total Receipts'))) +
  geom_hex(bins = 30) +
  labs(title = "Time W/ Active Alcohol License vs Total Receipts",
       x = "Responsibility Time Difference (Days)",
       y = "Total Receipts") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))
############
####Hypothesis 2 visualization

# Order the data by total receipts in descending order
df_merged_ordered <- df_merged[order(-df_merged$`Total Receipts`), ]

# Select the top 15 ZIP codes by total receipts
top15_zipcodes <- head(df_merged_ordered$`ZIP CODES`, 15)

# Filter the data for the top 15 zip codes
df_top15 <- df_merged %>%
  filter(`ZIP CODES` %in% top15_zipcodes)

# Create a bar plot for total receipts across the top 15 zip codes
ggplot(df_selected, aes(x = factor(`Location Zip`), y = log(`Total Receipts`))) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Zip Codes with Highest Total Sales",
       x = "Zip Code",
       y = "Log Total Receipts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))

########
###Preliminary Visualization 1 -- Mean and Median Household Income in top 15
###most populous zipcodes
# Order the data by total population in descending order
Zip_Demograph_ordered <- Zip_Demograph[order(-Zip_Demograph$`Total Population`), ]

# Select the top 15 ZIP codes by total population
subset_zip <- head(Zip_Demograph_ordered$`ZIP CODES`, 15)

# Reshape the data to long format using tidyr
library(tidyr)

Zip_Demograph_long <- gather(Zip_Demograph_ordered, key = "Statistic", value = "Income", 
                             `Mean Household Earnings`, `Median Household Income`)

# Create a bar plot for mean and median household incomes with rotated labels
ggplot(Zip_Demograph_long[Zip_Demograph_long$`ZIP CODES` %in% subset_zip, ], 
       aes(x = factor(`ZIP CODES`), y = Income, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean and Median Household Incomes by ZIP CODES",
       x = "ZIP CODES", y = "Income",
       fill = "Statistic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_manual(values = c("Mean Household Earnings" = "blue", "Median Household Income" = "orange")) +
  guides(fill = guide_legend(title = "Statistic"))


#############
##### Prelim Visualization 3

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Define the list of zip codes
selected_zipcodes <- c("78701", "78702", "78703", "78704", "78705", "78728", "78735", "78741", "78745", 
                       "78748", "78752", "78753", "78757", "78758", "78759")

# Filter the data for the selected zip codes
df_selected <- df_merged %>%
  filter(`ZIP CODES` %in% selected_zipcodes)

ggplot(df_selected, aes(x = factor(`ZIP CODES`), y = log(`Total Receipts`), fill = factor(`Alcohol Sales`))) +
  geom_bar(stat = "identity") +
  labs(title = "Alcohol Sales Across Highest Selling Zipcodes",
       x = "Zip Code",
       y = "Log Transformed Total Receipts",
       fill = "Alcohol Sales") +
  scale_y_continuous(labels = scales::comma) +  # Use comma separator for thousands
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values = c("Very High Sales" = "darkblue", "High Sales" = "blue", "Medium Sales" = "turquoise", "Low Sales" = "skyblue"))
  guides(fill = guide_legend(title = "Alcohol Sales"))
  
###########
  # Define custom order for alcohol sales levels
  sales_levels <- c("Very High Sales", "High Sales", "Medium Sales", "Low Sales")
  #worked
  establishment_counts <- df_selected %>%
    group_by(`ZIP CODES`) %>%
    summarize(Number_of_Establishments = n_distinct(`Taxpayer Number`))
  
  establishment_counts <- df_selected %>%
    group_by(`ZIP CODES`, `Alcohol Sales`) %>%
    summarize(Number_of_Establishments = n_distinct(`Taxpayer Number`))
  
  # Print the distinct establishment counts
  print(establishment_counts)

  df_selected <- left_join(df_selected, establishment_counts, by = "ZIP CODES")  
#### This one worked!
  ggplot(establishment_counts, aes(x = factor(`ZIP CODES`), y = log(Number_of_Establishments), fill = `Alcohol Sales`)) +
    geom_bar(stat = "identity") +
    labs(title = "Alcohol Sales by Type across Highest Selling Zipcodes",
         x = "Zip Code",
         y = "Log Transformed Number of Establishments",
         fill = "Alcohol Sales") +
    scale_y_continuous(labels = scales::comma) +  # Use comma separator for thousands
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(title = "Alcohol Sales"))
