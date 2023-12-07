# -*- coding: utf-8 -*-
"""Fiona_Final_SDS.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1N0TlvM2DVHOE0ExJmMGKKOgU__KHCLmm

Imports
"""

!pip install pandas

"""#### Loading Data
Used merged dataset from a different file so that Google Colab would not run into spacing issues, where the file would not completely load and therefore, causing incomplete analysis.
"""

from google.colab import drive
drive.mount('/content/drive')

import pandas as pd
df_merged = pd.read_csv("/content/drive/MyDrive/SDS/austin_bev_demographics.csv")

len(df_merged)

df_merged.describe()

"""Important Observations:
- 195,682 Alcohol Related Locations in Austin that submitted Tax Receipts
- Mean Liquor Receipts was 31,828
- Mean Wine Receipts was 8,386
- Mean Beer Receipts was 15,455
- Mean Cover Charge Receipts was 119
- Mean Total Receipts was 55,841
"""

# Correlation Plot
import plotly.express as px
px.imshow(df_merged.corr(), title = "df_merge corr")

numeric_df = df_merged.select_dtypes(include='number')
# Create a correlation heatmap
fig = px.imshow(numeric_df.corr(), title="df_merge corr")

# Angle x-axis labels at a 45-degree angle
fig.update_xaxes(categoryorder='array', categoryarray=numeric_df.columns, tickangle=45)

# Update layout to set smaller font size for axis titles
fig.update_layout(
    xaxis=dict(tickfont=dict(size=8)),
    yaxis=dict(tickfont=dict(size=8)),
)

# Show the plot
fig.show()

# Correlation Heatmap of Simply Target Variable of Total Receipts

total_receipts_correlation = df_merged.corr().loc[['Total Receipts']]

fig = px.imshow(total_receipts_correlation, title="Correlation Heatmap for Total Receipts")

fig.update_xaxes(categoryorder='array', categoryarray=total_receipts_correlation.columns, tickangle=45)

fig.update_layout(
    coloraxis=dict(colorbar=dict(orientation='h')),
)

fig.show()

"""#### Classification Assignments"""

# Select a target variable for classification, e.g., 'Total Receipts'
target_variable = 'Total Receipts'

# Drop rows with missing values in the target variable
df_merged = df_merged.dropna(subset=[target_variable])

# Create quartiles based on the target variable
quartiles = pd.qcut(df_merged[target_variable], q=[0, 0.25, 0.5, 0.75, 1], labels=['Low Sales', 'Medium Sales', 'High Sales', 'Very High Sales'])

# Add the quartiles as a new column to the dataframe
df_merged['Alcohol Sales'] = quartiles

# Print the dataframe with the new 'Quartile' column
print(df_merged[['Total Receipts', 'Alcohol Sales']])

"""Feature Definition and Train/Test/Split"""

from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, classification_report

# Assuming df_regression is your dataframe
# Replace 'df_regression' with the actual variable name you have assigned to your dataset

# Select the relevant columns for the classification
features = ['Responsibility Time Difference (Days)', 'Median Age in Years', 'Total Population',
            '21 years and over population', 'Male Count', 'Hispanics or Lation of Any Race', 'White Alone',
            'Black Alone', 'Asian Alone', 'Median Household Income', 'Mean Household Earnings', 'Total Housing Units']

# Target variable (based on quartiles, for example)
target_variable = 'Alcohol Sales'

# Drop rows with missing values in the features and target variable
df_merged = df_merged.dropna(subset=features + [target_variable])

# Map quartile labels to numerical values
# Assuming 'Alcohol Sales' is already in a format suitable for classification (e.g., binary labels)
# If not, you can convert it to numerical labels as needed
df_merged[target_variable] = df_merged[target_variable].astype('category').cat.codes

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(df_merged[features], df_merged[target_variable], test_size=0.2, random_state=42)

"""Decision Tree Classifier - Accuracy: 0.73"""

# Decision Tree Classifier
dt_classifier = DecisionTreeClassifier(random_state=42)
dt_classifier.fit(X_train, y_train)
dt_pred = dt_classifier.predict(X_test)
dt_accuracy = accuracy_score(y_test, dt_pred)
print(f"Decision Tree Accuracy: {dt_accuracy}")
print("Classification Report:")
print(classification_report(y_test, dt_pred))

"""Random Forest - Accuracy: 0.73"""

rf_classifier = RandomForestClassifier(random_state=42)
rf_classifier.fit(X_train, y_train)
rf_pred = rf_classifier.predict(X_test)
rf_accuracy = accuracy_score(y_test, rf_pred)
print(f"Random Forest Accuracy: {rf_accuracy}")
print("Classification Report:")
print(classification_report(y_test, rf_pred))

import matplotlib.pyplot as plt
import seaborn as sns

# Assuming rf_classifier is your Random Forest classifier
# Replace 'rf_classifier' with the actual variable name you have assigned to your Random Forest classifier

# Get feature importances from the Random Forest model
feature_importance = rf_classifier.feature_importances_

# Create a DataFrame for better visualization
importance_df = pd.DataFrame({'Feature': features, 'Importance': feature_importance})

# Sort features based on importance
importance_df = importance_df.sort_values(by='Importance', ascending=False)

# Plot feature importance
plt.figure(figsize=(10, 6))
sns.barplot(x='Importance', y='Feature', data=importance_df, palette='viridis')
plt.title('Feature Importance - Random Forest')
plt.xlabel('Importance')
plt.ylabel('Feature')
plt.show()

"""SVM Classifier - Accuracy: 0.35 (Dissapointing for the fact it took one entire hour for it to run.)"""

svm_classifier = SVC(random_state=42)
svm_classifier.fit(X_train, y_train)
svm_pred = svm_classifier.predict(X_test)
svm_accuracy = accuracy_score(y_test, svm_pred)
print(f"SVM Accuracy: {svm_accuracy}")
print("Classification Report:")
print(classification_report(y_test, svm_pred))

"""K-Nearest Neighbors Accuracy: 0.69"""

# K-Nearest Neighbors Classifier
knn_classifier = KNeighborsClassifier()
knn_classifier.fit(X_train, y_train)
knn_pred = knn_classifier.predict(X_test)
knn_accuracy = accuracy_score(y_test, knn_pred)
print(f"K-Nearest Neighbors Accuracy: {knn_accuracy}")
print("Classification Report:")
print(classification_report(y_test, knn_pred))

"""Logistic Regression Accuracy: 0.33"""

# Logistic Regression Classifier
logreg_classifier = LogisticRegression(random_state=42)
logreg_classifier.fit(X_train, y_train)
logreg_pred = logreg_classifier.predict(X_test)
logreg_accuracy = accuracy_score(y_test, logreg_pred)
print(f"Logistic Regression Accuracy: {logreg_accuracy}")
print("Classification Report:")
print(classification_report(y_test, logreg_pred))

"""Decision Tree Optimization (Had highest initial accuracy and most interpretable model)"""

# Decision Tree
feature_importances = dt_classifier.feature_importances_
num_features = len(feature_importances)

feature_names = features
for i in range(num_features):
    print(f"Feature {feature_names[i]}: {feature_importances[i]}")

# Optimizing Tree Depth
from sklearn.model_selection import cross_val_score
import numpy as np

# Define the target variable and features
#target_variable = set
#features = set
# Split the data into training and testing sets
# X_train y_train

# Define the range of max_depth values to explore
max_depth_values = range(1, 50)



# Initialize empty lists to store mean and standard deviation of cross-validation scores
mean_cv_scores = []
std_cv_scores = []

# Perform cross-validation for each max_depth value
for depth in max_depth_values:
    decision_tree = DecisionTreeClassifier(max_depth=depth)
    cv_scores = cross_val_score(decision_tree, X_train, y_train, cv=50)
    mean_cv_scores.append(np.mean(cv_scores))
    std_cv_scores.append(np.std(cv_scores))

# Plot the results with error bars
plt.figure(figsize=(10, 6))
plt.errorbar(max_depth_values, mean_cv_scores, yerr=std_cv_scores, fmt='o', capsize=5)
plt.xlabel('Max Depth of Decision Tree')
plt.ylabel('Mean Cross-Validation Accuracy')
plt.title('Decision Tree Accuracy vs. Max Depth')
plt.grid(True)
plt.xticks(np.arange(1, 21, step=1))
plt.show()

# Plot the results with error bars
plt.figure(figsize=(10, 6))
plt.errorbar(max_depth_values, mean_cv_scores, yerr=std_cv_scores, fmt='o', capsize=5)
plt.xlabel('Max Depth of Decision Tree')
plt.ylabel('Mean Cross-Validation Accuracy')
plt.title('Decision Tree Accuracy vs. Max Depth')
plt.grid(True)
plt.xticks(np.arange(1, 51, step=10))
plt.show()

"""Optimal Depth is around 31 by looking at the chart."""

# Decision Tree Classifier
dt_classifier = DecisionTreeClassifier(random_state=42, max_depth = 31)
dt_classifier.fit(X_train, y_train)
dt_pred = dt_classifier.predict(X_test)
dt_accuracy = accuracy_score(y_test, dt_pred)
print(f"Decision Tree Accuracy: {dt_accuracy}")
print("Classification Report:")
print(classification_report(y_test, dt_pred))