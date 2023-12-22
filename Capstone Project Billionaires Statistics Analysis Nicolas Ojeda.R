# Load all packages required for the project
required_packages <- c(
  "tidyverse",    # For data manipulation and visualization
  "lubridate",    # For handling date-time data
  "ggplot2",      # For creating advanced graphics
  "dplyr",        # For data manipulation
  "readr",        # For reading CSV data
  "caret",        # For modeling and machine learning
  "randomForest", # For Random Forest algorithm
  "rmarkdown",    # For dynamic report generation
  "stats",        # For statistical functions
  "broom"         # For tidying model outputs
)

# Install missing packages
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

# Load all required libraries
invisible(lapply(required_packages, library, character.only = TRUE))

# Enhanced package installation with error handling
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load the dataset (assumes CSV format)
billionaires <- read.csv("C:/Users/nico0/OneDrive/Documents/billionare/Billionaires Statistics Dataset.csv", stringsAsFactors = FALSE)

# Preliminary data cleaning
billionaires <- billionaires %>%
  mutate_if(is.character, trimws) %>%  # Trimming whitespace from character columns
  na.omit()  # Removing rows with any missing values

# Summary statistics to understand the data better
summary(billionaires)

# Split the data into training and test sets (80-20 split)
set.seed(123)  # Setting seed for reproducibility
train_index <- createDataPartition(billionaires$category, p = 0.8, list = FALSE)
train_set <- billionaires[train_index, ]
test_set <- billionaires[-train_index, ]

# Check the dimensions of the train and test sets
dim(train_set)
dim(test_set)





# Visualizing Total Billionaire Wealth by Country
ggplot(billionaires, aes(x = country, y = finalWorth)) +
  geom_col(fill = "blue", color = "black") +  # Using bar charts to represent total wealth
  labs(title = "Total Billionaire Wealth by Country", 
       x = "Country", 
       y = "Total Wealth (in billions)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotating x labels for readability




# Initialize a ggplot with billionaires data, mapping category to x-axis and finalWorth to y-axis
ggplot(billionaires, aes(x = category, y = finalWorth)) +
  # Add boxplots to show the wealth distribution across different categories
  geom_boxplot(fill = "lightblue", color = "black") +
  # Add labels for the plot, x-axis, and y-axis
  labs(title = "Wealth Distribution by Category", 
       x = "Category", 
       y = "Wealth (in billions)") +
  # Tilt the x-axis text for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





  # Histogram for Distribution of Billionaire Ages
hist(billionaires$age, 
     breaks = 30,  # Set the number of bins to 30
     main = "Distribution of Billionaire Ages",  # Title of the plot
     xlab = "Age",  # Label for the x-axis
     ylab = "Frequency",  # Label for the y-axis
     col = "lightblue",  # Color the bars light blue
     border = "black")  # Color the border of the bars black






# Initialize a ggplot with billionaires data, mapping age to x-axis and finalWorth to y-axis
ggplot(billionaires, aes(x = age, y = finalWorth)) +
  # Add an empirical cumulative distribution function (ECDF) to show the cumulative wealth distribution by age
  stat_ecdf(geom = "step", color = "darkblue") +
  # Add labels for the plot, x-axis, and y-axis
  labs(title = "Cumulative Wealth Distribution by Age", 
       x = "Age", 
       y = "Cumulative Wealth (in billions)") +
  # Apply a minimal theme for a clean look
  theme_minimal()





# Initialize a ggplot with billionaires data, mapping finalWorth to x-axis
ggplot(billionaires, aes(x = finalWorth)) +
  # Add a density plot to visualize the distribution of billionaire wealth
  geom_density(fill = "salmon", color = "darkred", alpha = 0.7) +
  # Add labels for the plot, x-axis, and y-axis
  labs(title = "Density Plot of Billionaires' Wealth", 
       x = "Wealth (in billions)", 
       y = "Density") +
  # Apply a black and white theme for a classic look
  theme_bw() +
  # Limit the x-axis to the 99th percentile to focus on the most common range and exclude extreme values
  xlim(0, quantile(billionaires$finalWorth, 0.99))





# Convert categorical variables to factors
billionaires$gender <- as.factor(billionaires$gender)
billionaires$category <- as.factor(billionaires$category)
billionaires$country <- as.factor(billionaires$country)

# Building separate linear models for Male and Female to compare
model_male <- lm(finalWorth ~ age + category + country, data = filter(billionaires, gender == 'M'))
model_female <- lm(finalWorth ~ age + category + country, data = filter(billionaires, gender == 'F'))

# Diagnostic plots to check assumptions and model fit for the male model
par(mfrow = c(2, 2))
plot(model_male)

# Diagnostic plots for the female model
par(mfrow = c(2, 2))
plot(model_female)

# Summarizing models to understand the influence of predictors
summary(model_male)
summary(model_female)




# Extracting coefficients for the male and female models
coef_male <- broom::tidy(model_male)
coef_female <- broom::tidy(model_female)

# Static ggplot for Male Model
gg_male <- ggplot(coef_male, aes(x = estimate, y = reorder(term, estimate))) +
  geom_bar(stat = 'identity', fill = 'blue') +
  labs(title = 'Significant Coefficients of the Male Model', x = "Estimates", y = "Terms") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12))

# Static ggplot for Female Model
gg_female <- ggplot(coef_female, aes(x = estimate, y = reorder(term, estimate))) +
  geom_bar(stat = 'identity', fill = 'pink') +
  labs(title = 'Significant Coefficients of the Female Model', x = "Estimates", y = "Terms") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12))





# Converting categorical variables to factors
billionaires$country <- as.factor(billionaires$country)
billionaires$source <- as.factor(billionaires$source)
billionaires$category <- as.factor(billionaires$category)

# Convert 'age' to numeric if it's not already
billionaires$age <- as.numeric(billionaires$age)

# Optional: Remove rows with NA values if they exist
billionaires <- na.omit(billionaires)

# Finding top 10 countries
top_countries <- names(sort(table(billionaires$country), decreasing = TRUE)[1:10])

# Finding top 10 sources
top_sources <- names(sort(table(billionaires$source), decreasing = TRUE)[1:10])

# Reducing the number of levels in categorical variables
# Group countries into fewer categories
billionaires$country <- as.factor(ifelse(billionaires$country %in% top_countries, billionaires$country, "Other"))

# Repeat similar steps for 'source' if it has more than 53 levels
# Replace 'top_sources' with a vector of your selected sources
billionaires$source <- as.factor(ifelse(billionaires$source %in% top_sources, billionaires$source, "Other"))

# Split the data into training and testing sets
set.seed(123) # Set a random seed for reproducibility
index <- createDataPartition(billionaires$category, p = 0.8, list = FALSE)
train_set <- billionaires[index, ]
test_set <- billionaires[-index, ]
# Building the Random Forest model
model_rf <- randomForest(category ~ age + country + source, data = billionaires, ntree = 100)





# Making predictions on the test set
predictions <- predict(model_rf, newdata = test_set)

# Evaluating the results with a confusion matrix
confusion_matrix <- table(Predicted = predictions, Actual = test_set$category)
print(confusion_matrix)

# Calculating accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

# If your 'category' variable is binary, calculate the AUC
if (length(levels(test_set$category)) == 2) {
  roc_response <- ifelse(test_set$category == levels(test_set$category)[2], 1, 0)
  predictions_numeric <- as.numeric(predictions == levels(predictions)[2])
  roc_curve <- roc(roc_response, predictions_numeric)
  auc_value <- auc(roc_curve)
  cat("AUC:", auc_value, "\n")
  
  # Plotting ROC curve
  ggplot() +
    geom_line(data = data.frame(fpr = roc_curve$specificities, tpr = roc_curve$sensitivities), aes(x = fpr, y = tpr)) +
    geom_abline(linetype = "dashed") +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    ggtitle("ROC Curve") +
    theme_minimal()
}

# Visualize the results with a variable importance plot
varImpPlot(model_rf, main = "Variable Importance Plot")

# Visualize the Out-of-Bag error rate across the number of trees
plot(model_rf$err.rate[,1], type = "l", 
     xlab = "Number of Trees", ylab = "OOB Error Rate", 
     main = "OOB Error Rate Across Trees")


