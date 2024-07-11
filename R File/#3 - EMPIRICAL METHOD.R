#National Chengchi University

#Project Description

#This study aims to investigate the relationship between suicide rates and various proxies of 'Social Capital loss' in the United States from 2000 to 2022. The breakdown of society in the West, marked by rising rates of racism, hate speech, and discrimination, has been accompanied by an increase in suicide rates.

#Given the limited research on this topic, this study seeks to elucidate the correlation and the causal effect between suicide rates and the increase in hate groups and hate crimes, among other factors.

#============================================================================#
#============================================================================#
# PART 3 - EMPIRICAL METHOD
#============================================================================#
#============================================================================#


#  --------------------------------------------------------
library(tidyverse)
library(readxl)

NEW_merged_data <- read_excel("/Users/ignatiusharry/Library/CloudStorage/Dropbox/Data/S2/NCCU/2nd Semester/Big data and social analysis/Final project/Crime risk analysis/data/Control Variables/NEW_merged_data.xlsx")
# GENERAL CLEANING ============================================================================#

# to observe NA values in our data
colSums(is.na(NEW_merged_data))

# filtering our data
NEW_merged_data <- NEW_merged_data %>%
  filter(Year >= 2000 & Year <= 2022)

# Rename 
NEW_merged_data <- NEW_merged_data %>%
  rename(poverty_rate = `% Poverty Estimate (All Ages)`,
         unemployment_rate= `Unemployment Rate (%)`,
         median_household_income = `Median Household Income`)
         
str(NEW_merged_data)

# Cleaning the data

data <- na.omit(NEW_merged_data)

colSums(is.na(data))

# Delete all of the missing data
data <- NEW_merged_data %>%
  filter(!is.na(poverty_rate) & 
           !is.na(median_household_income) &
           !is.na(unemployment_rate))




# Change into log---------------------------------------------------------
# Adding a small constant to avoid log(0) issues
data <- data %>%
  mutate(log_population = log(Population),
         log_med_household_income = log(median_household_income),
         log_unemployment = log(unemployment_rate),
         log_suicide_rate = log(Suicide_Rate + 1),  # Adding 1 to avoid log(0)
         log_numberofhategroups = log(numberofhategroups + 1), # Adding 1 to avoid log(0)
         log_poverty_rate = log(poverty_rate +1))

str(data)

# Check for NAs in the index columns
table(is.na(data$AFFGEOID))
table(is.na(data$Year))

# Remove rows with NA in the index columns
data <- data %>%
  filter(!is.na(AFFGEOID) & !is.na(Year))


# Analysing Empirical Method  ============================================================================#

str(data)
#  1. Pearson Correlation (2003-2016) Balanced Data -------------------------------------------------------------------------
library(corrplot)

# Subset the relevant variables from data_clean
vars <- c("log_suicide_rate", "log_numberofhategroups", "log_population", "log_med_household_income", 
          "log_unemployment", 
          "log_poverty_rate")
data_subset <- data[vars]

ggplot(data, aes(x = log_suicide_rate)) +
  geom_histogram()

# Calculate correlations
correlation_matrix <- cor(data_subset)
# Round the correlation matrix to 2 decimal places
rounded_correlation_matrix <- round(correlation_matrix, 2)

# Convert the correlation matrix to data frame for better display
correlation_df <- as.data.frame(rounded_correlation_matrix)

# Print the correlation matrix as a table
print(correlation_df)

# Sort correlations by absolute value from strongest to weakest for log_suicide_rate row
sorted_indices <- order(abs(correlation_matrix["log_suicide_rate", ]), decreasing = TRUE)
sorted_matrix <- correlation_matrix[sorted_indices, sorted_indices]

# Plot sorted correlation matrix using corrplot
corrplot(sorted_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, diag = FALSE,
         main = "Pearson's correlation matrix of key variables")

library(ggplot2)  

ggplot(data, aes(x = log_numberofhategroups, y = log_suicide_rate)) +
  # Theme adjustments
  theme_bw() +  # Use black and white theme for a clean look
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),  # Gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    plot.title = element_text(hjust = 0.5),  # Center title
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.title.x = element_text(vjust = -0.2),  # Lower x-axis title
    axis.title.y = element_text(angle = 90),  # Rotate y-axis title
    legend.position = "bottom"  # Move legend to bottom
  ) +
  # Geometries with adjustments
  geom_point(aes(), alpha = 0.7) +  # Color and size by factor level (optional)
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  # Enhance labels and title
  labs(title = "Suicide Rate vs. Number of Hate Groups",
       x = "Number of Hate Groups",
       y = "Suicide Rate")

correlation <- cor(data$log_numberofhategroups, data$log_suicide_rate)
correlation

# 2. OLS Method (2000-2022) unbalanced data--------------------------------------------------------------
library(sandwich)
library(broom)
library(lmtest)

#change into log 
data_clean <- NEW_merged_data %>%
  mutate(log_population = log(Population),
         log_med_household_income = log(median_household_income),
         log_unemployment = log(unemployment_rate),
         log_suicide_rate = log(Suicide_Rate + 1),  # Adding 1 to avoid log(0)
         log_numberofhategroups = log(numberofhategroups + 1), # Adding 1 to avoid log(0)
         log_poverty_rate = log(poverty_rate + 1))

# Run the OLS model using lm
model1 <- lm(log_suicide_rate ~ log_numberofhategroups + 
               #lag_log_numberofhategroups * lag_log_med_household_income + 
               log_med_household_income + 
               log_poverty_rate + 
               log_unemployment + 
               log_population, 
             data = data_clean)

# Summary of the model
summary(model1)

# Calculate robust standard errors
robust_se <- vcovHC(model1, type = "HC4")

# Use coeftest to get robust standard errors
robust_summary <- coeftest(model1, vcov = robust_se)

# Print the robust summary
print(robust_summary)


# 3. Fixed Effect Method with Lagged (2000-2022) unbalanced data-------------------------------------------------------------------------
library(plm)

# Define the panel data
# Convert County_State and Year into factors
data_clean$County_State <- as.factor(data_clean$County_State)
data_clean$Year <- as.factor(data_clean$Year)

pnel <- pdata.frame(data_clean, index = c("AFFGEOID", "Year"))
str(pnel)

# Checking year distribution
table(pnel$Year)

# Check the number of observations per entity
table(pnel$AFFGEOID)

which(is.na(pnel))

obs_per_unit <- pnel %>%
  group_by(County_State) %>%
  summarise(n = n())

# Check the distribution of the number of observations
table(obs_per_unit$n)


# Create lagged variables
pnel$lag_log_suicide_rate <- lag(pnel$log_suicide_rate, 1)
pnel$lag_log_numberofhategroups <- lag(pnel$log_numberofhategroups, 1)
pnel$lag_log_med_household_income <- lag(pnel$log_med_household_income, 1)
pnel$lag_log_poverty_rate <- lag(pnel$log_poverty_rate, 1)
pnel$lag_log_unemployment <- lag(pnel$log_unemployment, 1)
pnel$lag_log_population <- lag(pnel$log_population, 1)

summary(pnel)

# 1st Model without Interaction -------------------------------------------
model_no_interaction <- plm(log_suicide_rate ~ log_numberofhategroups + 
                              log_med_household_income + 
                              log_poverty_rate + 
                              log_unemployment + 
                              log_population, 
                            data = pnel, 
                            model = "within")

# Summary of the model
summary(model_no_interaction)

# Calculate robust standard errors
robust_se <- vcovHC(model_no_interaction, type = "HC4")

# Use coeftest to get robust standard errors
robust_summaryte <- coeftest(model_no_interaction, vcov = robust_se)

# Print the robust summary
print(robust_summaryte)



# 2nd Model without Interaction but with lag -------------------------------------------
model_lag_no_interaction <- plm(lag_log_suicide_rate ~ lag_log_numberofhategroups + 
                              lag_log_med_household_income + 
                              lag_log_poverty_rate + 
                              lag_log_unemployment + 
                              lag_log_population, 
                            data = pnel, 
                            model = "within")

# Summary of the model
summary(model_lag_no_interaction)

# Calculate robust standard errors
robust_sb <- vcovHC(model_lag_no_interaction, type = "HC4")

# Use coeftest to get robust standard errors
robust_summarytb <- coeftest(model_lag_no_interaction, vcov = robust_sb)

# Print the robust summary
print(robust_summarytb)

# Model With Interaction --------------------------------------------------

model_with_interaction <- plm(lag_log_suicide_rate ~ lag_log_numberofhategroups + 
                                lag_log_poverty_rate + 
                                #lag_log_unemployment +
                                lag_log_numberofhategroups:lag_log_med_household_income +
                                #lag_log_numberofhategroups:lag_log_unemployment +
                                #lag_log_unemployment:lag_log_population +
                                lag_log_unemployment, 
                              data = pnel, 
                              model = "within")
summary(model_with_interaction)

# Calculate robust standard errors
robust_se <- vcovHC(model_with_interaction, type = "HC4")

# Use coeftest to get robust standard errors
robust_summaryse <- coeftest(model_with_interaction, vcov = robust_se)

# Print the robust summary
print(robust_summaryse)


