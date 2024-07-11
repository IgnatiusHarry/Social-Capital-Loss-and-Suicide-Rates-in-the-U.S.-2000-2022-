#National Chengchi University

#Project Description

#This study aims to investigate the relationship between suicide rates and various proxies of 'Social Capital loss' in the United States from 2000 to 2022. The breakdown of society in the West, marked by rising rates of racism, hate speech, and discrimination, has been accompanied by an increase in suicide rates.

#Given the limited research on this topic, this study seeks to elucidate the correlation and the causal effect between suicide rates and the increase in hate groups and hate crimes, among other factors.

#============================================================================#
#============================================================================#
# PART 2 - DATA VISUALIZATION (MAP, Diagram, Chart, etc)
#============================================================================#
#============================================================================#



# -------------------------------------------------------------------------


library(tidyverse)
library(readxl)

setwd("/Users/ignatiusharry/Library/CloudStorage/Dropbox/Data/S2/NCCU/2nd Semester/Big data and social analysis/Final project/Crime risk analysis/data/Dataframe")
merged_data <- read_excel("/Users/ignatiusharry/Library/CloudStorage/Dropbox/Data/S2/NCCU/2nd Semester/Big data and social analysis/Final project/Crime risk analysis/data/Control Variables/NEW_merged_data.xlsx")


# Clean the data ----------------------------------------------------------
# Filter data for years between 2000 and 2022
filtered_data <- merged_data %>%
  filter(Year >= 2000 & Year <= 2022)


#based on the website hidden value (NA) is zero
#convert NA to 0
filtered_data$Deaths[is.na(filtered_data$Deaths)] <- 0
filtered_data$Population[is.na(filtered_data$Population)] <- 0
filtered_data$Suicide_Rate[is.na(filtered_data$Suicide_Rate)] <- 0
filtered_data$numberofhategroups[is.na(filtered_data$numberofhategroups)] <- 0

str(filtered_data)

#Save the final data frame
library(openxlsx)
write.xlsx(filtered_data, "NEW_merged_data.xlsx")

# -------------------------------------------------------------------------
# To know how many zero (0) values we have
library(data.table)

dt <- as.data.table(filtered_data)
na_positions <- dt[, lapply(.SD, function(x) which(is.na(x))), .SDcols = names(dt)]
na_positions

#
na_positions <- which(is.na(filtered_data), arr.ind = TRUE)
na_positions


total_zeros <- sum(filtered_data == 0, na.rm = TRUE)


zeros_per_variable <- colSums(filtered_data == 0, na.rm = TRUE)


total_zeros
zeros_per_variable

# getting to know more detail about the data ------------------------------------------------------

total_missing <- sum(is.na(filtered_data))
total_missing

missing_per_variable <- colSums(is.na(filtered_data))
missing_per_variable


# Analyzing data ----------------------------------------------------------

# To make the geom bar only (to know the initial findings), not related with the map or correlation --------

# we want to know the total observation and total counties
observations_per_year <- filtered_data %>%
  group_by(Year) %>%
  summarise(observations_count = n())
print(observations_per_year)


county_count_per_year <- filtered_data %>%
  group_by(Year) %>%
  summarize(total_county_observed = n_distinct(County))
print(county_count_per_year)


# Suicide Rate ------------------------------------------------------------
#to find the highest avg suicide in each year
library(dplyr)

suicide_rate_by_year <- filtered_data %>%
  group_by(Year) %>%
  summarise(mean_suicide_rate = mean(Suicide_Rate, na.rm = TRUE))

suicide_rate_by_year

year_with_highest_suicide_rate <- suicide_rate_by_year %>%
  filter(mean_suicide_rate == max(mean_suicide_rate))

# Create a bar plot 
ggplot(suicide_rate_by_year, aes(x = Year, y = mean_suicide_rate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(mean_suicide_rate, 2)), vjust = -0.5, size = 3.5) +
  labs(title = "The average suicide rates in U.S. counties between the years 2000 and 2022.", 
       subtitle = paste("Year with highest average suicide rate:", year_with_highest_suicide_rate$Year),
       x = "Year", 
       y = "Mean Suicide Rate") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10))
#2022 has the highest avg suicide rate 6.347957
# -------------------------------------------------------------------------

# Calculate the average suicide rate per county and sort
suicide_rate_by_county <- filtered_data  %>%
  group_by(County.Name) %>%
  summarise(mean_suicide_rate = mean(Suicide_Rate, na.rm = TRUE)) %>%
  arrange(desc(mean_suicide_rate))

# Filter to include only the top 10 counties
top_10_counties <- suicide_rate_by_county %>%
  slice(1:10)

# Create the bar plot for the top 10 counties
ggplot(top_10_counties, aes(x = reorder(County.Name, -mean_suicide_rate), y = mean_suicide_rate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(mean_suicide_rate, 2)), vjust = -0.5, size = 3.5) +
  labs(title = "Top 10 U.S. Counties with the Highest Average Suicide Rates (2000-2022)", 
       x = "County Name", 
       y = "Mean Suicide Rate") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotates the x-axis labels for better readability
  )

#NYE County has the highest avg suicide rate 35.38

# Hate Groups -------------------------------------------------------------
str(filtered_data)


# Total Hate Groups by Year ----------------------------------------------

# Total Hate Groups in each year
TotalHG <- filtered_data %>%
  group_by(Year) %>%
  summarise(total_groups = sum(numberofhategroups, na.rm = TRUE), .groups = 'drop')

print(TotalHG)
year_with_highest_Totalhate_groups <- TotalHG %>%
  filter(total_groups == max(total_groups))

# Create a bar plot 
ggplot(TotalHG, aes(x = Year, y = total_groups)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = total_groups), vjust = -0.5, size = 3.5) +  # Display total groups count on each bar
  labs(title = "Total Hate Groups in U.S. Counties (2000-2022)", 
       subtitle = paste("Year with highest Total hate groups:", year_with_highest_Totalhate_groups),
       x = "Year", 
       y = "Total Hate Groups") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10))


# AVG Hate Groups by year -------------------------------------------------------------------------
hate_groups_by_year <- filtered_data %>%
  group_by(Year) %>%
  summarise(mean_hate_groups = mean(numberofhategroups, na.rm = TRUE))

print(hate_groups_by_year)

#to find the highest avg Hate Groups in each year
year_with_highest_hate_groups <- hate_groups_by_year%>%
  filter(mean_hate_groups == max(mean_hate_groups))

# Create a bar plot 
ggplot(hate_groups_by_year, aes(x = Year, y = mean_hate_groups)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(mean_hate_groups, 2)), vjust = -0.5, size = 3.5) +
  labs(title = "The average hate groups in U.S. counties (2000-2022)", 
       subtitle = paste("Year with highest average hategroups:", year_with_highest_hate_groups $Year),
       x = "Year", 
       y = "Mean Hate Groups") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10))

# Combine 2 plots  into 1 graph -------------------------------------------

combined_data <- hate_groups_by_year %>%
  rename(mean_value = mean_hate_groups) %>%
  mutate(Type = "Hate Groups") %>%
  bind_rows(
    suicide_rate_by_year %>%
      rename(mean_value = mean_suicide_rate) %>%
      mutate(Type = "Suicide Rate")
  )


ggplot(combined_data, aes(x = Year, y = mean_value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_line(aes(color = Type, group = Type), size = 1, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = round(mean_value, 2)), vjust = -0.5, size = 3.5, position = position_dodge(width = 0.9)) +
  labs(title = "The average suicide rates and hate groups in U.S. counties between the years 2000 and 2022.", 
       x = "Year", 
       y = "Mean Value",
       fill = "Type",
       color = "Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


#============================================================================#
#============================================================================#
### Preliminary Graph #1 (has y and x limits)
#============================================================================#
#============================================================================#

# Replace values in the Deaths column with NA if they are less than or equal to 10 using base R
merged_data$Deaths[merged_data$Deaths <= 10] <- NA

# Create the suiciderate column using base R
merged_data$suiciderate <- (merged_data$Deaths / merged_data$Population) * 100000


# Create a scatter plot with axes switched and Y-axis limit set to 75 and X-axis limit set to 4
ggplot(merged_data, aes(x = numberofhategroups, y = suiciderate)) +
  geom_point() +  # Add points
  labs(x = "Number of Hate Groups", y = "Suicide Rate") +  # Label axes
  ggtitle("Scatter Plot of Number of Hate Groups vs Suicide Rate") +  # Add title
  ylim(0, 41) +  # Set Y-axis limit
  xlim(4, NA)   # Set X-axis limit with NA to keep the upper limit unrestricted

#============================================================================#
#============================================================================#
### Preliminary Graph #2 (does not have y and x limits)
#============================================================================#
#============================================================================#

# Replace values in the Deaths column with NA if they are less than or equal to 10 using base R
merged_data$Deaths[merged_data$Deaths <= 10] <- NA

# Create the suiciderate column using base R
merged_data$suiciderate <- (merged_data$Deaths / merged_data$Population) * 100000


# Create a scatter plot with axes switched and Y-axis limit set to 75 and X-axis limit set to 4
ggplot(merged_data, aes(x = numberofhategroups, y = suiciderate)) +
  geom_point() +  # Add points
  labs(x = "Number of Hate Groups", y = "Suicide Rate") +  # Label axes
  ggtitle("Scatter Plot of Number of Hate Groups vs Suicide Rate")

#============================================================================#
#============================================================================#
### Preliminary Graph #3 
#============================================================================#
#============================================================================#

merged_data <- read.csv("merged_data.csv")
ggplot(merged_data, aes(x = numberofhategroups, y = Suicide_Rate)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  labs(title = "Suicide Rate vs Hate Groups", x = "Number of Hate Groups", y = "Suicide Rate")


#============================================================================#
#============================================================================#
# Create a Map ------------------------------------------------------------
#============================================================================#
#============================================================================#


# Data (MAP) ---------------------------------------------------------------------
library(sf)
library(tmap)
library(tigris)

# County Level for POSTER only --------------------------------------------------------------
# -------------------------------------------------------------------------
us_counties <- counties(cb = TRUE, resolution = "20m") %>%
  shift_geometry()%>%
  filter (STATE_NAME != "Alaska")


plot(st_geometry(us_counties))
us_counties$GEOID <- as.numeric(us_counties$GEOID)
str(us_counties)

# Combine county data with suicide rate data

str(merged_data)

initial_df <- filtered_data %>%
  select(-ALAND, -AWATER, -STATEFP, -COUNTYFP,-COUNTYNS,-AFFGEOID, -STUSPS)

final_data <- left_join(us_counties, initial_df , by = c("GEOID"="County.Code"))
final_data$LSAD[is.na(final_data$LSAD)]

str(final_data)

final_data_sf <- st_as_sf(final_data)
plot(st_geometry(final_data_sf))

#convert NA to 0
final_data$numberofhategroups[is.na(final_data$numberofhategroups)] <- 0
str(final_data)

# 2012 --------------------------------------------------------------------
map2012 <- final_data_sf %>%
  filter(Year == 2012)

str(map2012)
# Convert frame data into sf objects
map2012_sf <- st_as_sf(map2012)

str(map2012_sf)

library(viridis)
# Create a tmap object
# Set tmap mode to plotting
tmap_mode("plot")

# Create the map
USmap_2012 <- tm_shape(map2012_sf) +
  tm_polygons("Suicide_Rate", palette = "Reds", title = "Suicide Rate in 2012",
              id = "NAME", 
              popup.vars = c("Suicide Rate" = "Suicide_Rate", "Hate Groups" = "numberofhategroups")) +
  tm_dots("numberofhategroups", palette = "Blues", size = "numberofhategroups", title = "Number of Hate Groups in 2012",
          id = "NAME",  
          popup.vars = c("Suicide Rate" = "Suicide_Rate", "Hate Groups" = "numberofhategroups")) +
  tm_layout(main.title = "Suicide Rates and Hate Groups in U.S. Counties in 2012",
            legend.outside = TRUE,
            frame = FALSE) +
  tm_legend(legend.position = c("left", "bottom"))

# Print Map
USmap_2012

# 2019 --------------------------------------------------------------------
map2019 <- final_data %>%
  filter(Year == 2019)

str(map2019)
# Convert frame data into sf objects
map2019_sf <- st_as_sf(map2019)

str(map2019_sf)

library(viridis)
# Create a tmap object
# Set tmap mode to plotting
tmap_mode("plot")

# Create the map
USmap_2019 <- tm_shape(map2019_sf) +
  tm_polygons("Suicide_Rate", palette = "Reds", title = "Suicide Rate in 2019",
              id = "NAME", 
              popup.vars = c("Suicide Rate" = "Suicide_Rate", "Hate Groups" = "numberofhategroups")) +
  tm_dots("numberofhategroups", palette = "Blues", size = "numberofhategroups", title = "Number of Hate Groups in 2019",
          id = "NAME",  
          popup.vars = c("Suicide Rate" = "Suicide_Rate", "Hate Groups" = "numberofhategroups")) +
  tm_layout(main.title = "Suicide Rates and Hate Groups in U.S. Counties in 2019",
            legend.outside = TRUE,
            frame = FALSE) +
  tm_legend(legend.position = c("left", "bottom"))

# Print Map
USmap_2019


# 2022--------------------------------------------------------------------
map2022 <- final_data %>%
  filter(Year == 2022)

str(map2022)
# Convert frame data into sf objects
map2022_sf <- st_as_sf(map2022)

str(map2022_sf)

library(viridis)
# Create a tmap object
# Set tmap mode to plotting
tmap_mode("plot")

# Create the map
USmap_2022 <- tm_shape(map2022_sf) +
  tm_polygons("Suicide_Rate", palette = "Reds", title = "Suicide Rate in 2022",
              id = "NAME", 
              popup.vars = c("Suicide Rate" = "Suicide_Rate", "Hate Groups" = "numberofhategroups")) +
  tm_dots("numberofhategroups", palette = "Blues", size = "numberofhategroups", title = "Number of Hate Groups in 2022",
          id = "NAME",  
          popup.vars = c("Suicide Rate" = "Suicide_Rate", "Hate Groups" = "numberofhategroups")) +
  tm_layout(main.title = "Suicide Rates and Hate Groups in U.S. Counties in 2022",
            legend.outside = TRUE,
            frame = FALSE) +
  tm_legend(legend.position = c("left", "bottom"))

# Print Map
USmap_2022

# County Level MORE DETAILED FOR OUR FINAL PRESENTATION--------------------------------------------------------------
# -------------------------------------------------------------------------
us_counties <- counties(cb = TRUE, resolution = "500k") %>%
  shift_geometry()%>%
  filter (STATE_NAME != "Alaska")

us_counties1 <- counties(cb = TRUE, resolution = "500k") %>%
  filter (STATE_NAME != "Alaska")

plot(st_geometry(us_counties1))
us_counties1$GEOID <- as.numeric(us_counties1$GEOID)
str(us_counties1)

# Combine county data with suicide rate data

str(merged_data)

initial_df <- filtered_data %>%
  select(-ALAND, -AWATER, -STATEFP, -COUNTYFP,-COUNTYNS,-AFFGEOID, -STUSPS)

final_data <- left_join(us_counties1, initial_df , by = c("GEOID"="County.Code"))
final_data$LSAD[is.na(final_data$LSAD)]

str(final_data)

final_data_sf <- st_as_sf(final_data)
plot(st_geometry(final_data_sf))

#convert NA to 0
final_data$numberofhategroups[is.na(final_data$numberofhategroups)] <- 0
str(final_data)
# 2012 --------------------------------------------------------------------
map2012 <- final_data %>%
  filter(Year == 2012)

str(map2012)
# Convert frame data into sf objects
map2012_sf <- st_as_sf(map2012)

str(map2012_sf)

library(viridis)
# Create a tmap object
# Set tmap mode to plotting
tmap_mode("view")

# Create the map
USmap_2012 <- tm_shape(map2012_sf) +
  tm_polygons("Suicide_Rate", palette = "Reds", title = "Suicide Rate in 2012",
              id = "NAME", 
              popup.vars = c("Suicide Rate" = "Suicide_Rate", "Hate Groups" = "numberofhategroups")) +
  tm_dots("numberofhategroups", palette = "Blues", size = "numberofhategroups", title = "Number of Hate Groups in 2012",
          id = "NAME",  
          popup.vars = c("Suicide Rate" = "Suicide_Rate", "Hate Groups" = "numberofhategroups")) +
  tm_layout(main.title = "Suicide Rates and Hate Groups in U.S. Counties in 2012",
            legend.outside = TRUE,
            frame = FALSE) +
  tm_legend(legend.position = c("left", "bottom"))

# Print Map
USmap_2012


# 2019 --------------------------------------------------------------------
map2019 <- final_data %>%
  filter(Year == 2019)

str(map2019)
# Convert frame data into sf objects
map2019_sf <- st_as_sf(map2019)

str(map2019_sf)

library(viridis)
# Create a tmap object
# Set tmap mode to plotting
tmap_mode("view")

# Create the map
USmap_2019 <- tm_shape(map2019_sf) +
  tm_polygons("Suicide_Rate", palette = "Reds", title = "Suicide Rate in 2019",
              id = "NAME", 
              popup.vars = c("Suicide Rate" = "Suicide_Rate", "Hate Groups" = "numberofhategroups")) +
  tm_dots("numberofhategroups", palette = "Blues", size = "numberofhategroups", title = "Number of Hate Groups in 2019",
          id = "NAME",  
          popup.vars = c("Suicide Rate" = "Suicide_Rate", "Hate Groups" = "numberofhategroups")) +
  tm_layout(main.title = "Suicide Rates and Hate Groups in U.S. Counties in 2019",
            legend.outside = TRUE,
            frame = FALSE) +
  tm_legend(legend.position = c("left", "bottom"))

# Print Map
USmap_2019



