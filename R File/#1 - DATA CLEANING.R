#National Chengchi University

#Project Description

#This study aims to investigate the relationship between suicide rates and various proxies of 'Social Capital loss' in the United States from 2000 to 2022. The breakdown of society in the West, marked by rising rates of racism, hate speech, and discrimination, has been accompanied by an increase in suicide rates.

#Given the limited research on this topic, this study seeks to elucidate the correlation and the causal effect between suicide rates and the increase in hate groups and hate crimes, among other factors.

#============================================================================#
#============================================================================#
# PART-1  DATA CLEANING
#============================================================================#
#============================================================================#



#============================================================================#
# General Cleaning - # Suicide Data --------------------------------------------------------
#============================================================================#


library(tidyverse)


setwd("/Users/ignatiusharry/Library/CloudStorage/Dropbox/Data/S2/NCCU/2nd Semester/Big data and social analysis/Final project/Crime risk analysis/data/Github")

suicide99_20 <- read.csv("with Zero Underlying Cause of Death, 1999-2020.txt", header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
suicide18_22 <- read.csv("with zero Underlying Cause of Death, 2018-2022, Single Race.txt", header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)


# delete quotation marks
data_clean0 <- lapply(suicide99_20, function(x) gsub("\"", "", x))
data_clean1 <- lapply(suicide18_22, function(x) gsub("\"", "", x))

#Convert into df and combine
data_df0 <- data.frame(data_clean0)
data_df1 <- data.frame(data_clean1)


data_df0 <- data_df0 %>%
  select(-X..of.Total.Deaths)

data_df1 <- data_df1 %>%
  select(-X.Injury.Intent.Code.,
         -X.Injury.Intent.)

data_df0 <- data_df0 %>%
  filter(X.Year. >= 1999 & X.Year. <= 2017)

str(data_df0)
str(data_df1)

#Combine into 1 df
data_clean_df <- rbind(data_df0,data_df1)

# converting the population data into numeric
data_clean_df$Population <- as.numeric(data_clean_df$Population)


str(data_clean_df)

# convert "Unreliable" to NA in Crude.Rate coloumn
data_clean_df$Crude.Rate[data_clean_df$Crude.Rate == "Unreliable"] <- NA
data_clean_df$Crude.Rate[data_clean_df$Crude.Rate == "Suppressed"] <- NA
data_clean_df$Deaths[data_clean_df$Deaths == "Suppressed"] <- 0

data_clean_df <- data_clean_df %>%
  slice(-75504:-75556)


data_clean_df$X.County.Code.<- as.numeric(data_clean_df$X.County.Code.)
data_clean_df$Deaths<- as.numeric(data_clean_df$Deaths)
str(data_clean_df)


# makesure the data ---------------------------------------------------------

caused_list <- unique(data_clean_df$X.ICD.10.113.Cause.List.)
caused_list 

total_county <- length(unique(data_clean_df$X.County.))
total_county 


year <- length(unique(data_clean_df$X.Year.))
year

# Analyzing data ----------------------------------------------------------

# Calculate suicide rate per 100,000 population
filtered_data <- data_clean_df %>%
  mutate(Suicide_Rate = (Deaths / Population) * 100000)


#arrange 

filtered_data <- filtered_data %>%
  arrange(X.County.)

# Cleaning data before converting to Excel ---------------------------------
str(filtered_data)
filtered_data <- select(filtered_data,  -X.Notes., -X.Year.Code., -Crude.Rate)

filtered_data <- filtered_data %>%
  rename("Year"= "X.Year.",
         "County Code" ="X.County.Code.")

# Include the unique code for making a map in County Level --------------------------------------------------------------

library(sf)
library(tmap)
library(tigris)

us_counties <- counties(cb = TRUE, resolution = "20m")%>%
  shift_geometry()

plot(st_geometry(us_counties))

numcounty <- length(unique(us_counties$NAMELSAD))
numcounty



#Convert character to numeric 
us_counties$GEOID <- as.numeric(us_counties$GEOID)
str(us_counties)

# Combine county data with suicide rate data
Final_Suicide_data <- left_join(filtered_data, us_counties, by = c("County Code" ="GEOID"))

#reorder
Final_Suicide_data <- Final_Suicide_data %>%
  relocate(NAMELSAD, NAME, STATE_NAME, STUSPS, STATE_NAME, ALAND, AWATER, .before = 1)

Final_Suicide_data <- Final_Suicide_data %>%
  relocate(Year, Deaths, Population, Suicide_Rate, .after=18)

#rename 
Final_Suicide_data <- Final_Suicide_data %>%
  rename("County"="NAMELSAD",
         "County Name" = "NAME",
         "State"= "STATE_NAME")

#delete selected coloumns
Final_Suicide_data <- Final_Suicide_data %>%
  select(-X.County.,-LSAD)

Final_Suicide_data <- Final_Suicide_data%>%
  filter (State != "Alaska")

# converting the data 
Final_Suicide_data$Year <- as.numeric(Final_Suicide_data$Year)
Final_Suicide_data$COUNTYFP <- as.numeric(Final_Suicide_data$COUNTYFP)
Final_Suicide_data$COUNTYFP <- as.numeric(Final_Suicide_data$COUNTYFP)

Final_Suicide_data <- Final_Suicide_data %>%
  select(-geometry)

str(Final_Suicide_data)
library(openxlsx)


write.xlsx(Final_Suicide_data, "Initial Data Frame.xlsx")
str(Final_Suicide_data)


#============================================================================#
# General Cleaning - # Hate Group --------------------------------------------------------
#============================================================================#


### 1. set your own working directory (obviously)
setwd("~/Documents/NCCU/2024-01/Big_Data_for_Social_Analysis/Final_Project/Data/hate_groups/")
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(stringr)

### 2. download all .csv files from https://www.splcenter.org/hate-map and put them into ONE folder
file_list <- list.files(pattern = "splc-hate-groups-\\d{4}\\.csv")
hategroups_raw <- file_list %>%
  map_df(~ read.csv(.x))
# this code reads all files and merges them into a single dataframe :)

#===============================================================================#
# 1. GENERAL CLEANING for hate group


### 1.1 remove NA values in "City" column  
hategroups_raw <- hategroups_raw[!is.na(hategroups_raw$City) & hategroups_raw$City != "", ]
### 1.2 Fix nomenclature of "City" values when State is Washington D.C.
hategroups_raw$City[hategroups_raw$State == "District of Columbia"] <- "Washington"
### 1.3 remove Alaska
hategroups_raw <- hategroups_raw[hategroups_raw$State != "Alaska", ]
### 1.4 remove "Headquarters" and "Statewide" column (we don't need them)
hategroups_raw <- subset(hategroups_raw, select = -c(Headquarters))
hategroups_raw <- subset(hategroups_raw, select = -c(Statewide))
### 1.5 Create "County" column, remove "county" values from City column and assign them then to County column
hategroups_raw <- hategroups_raw %>%
  mutate(County = ifelse(grepl(" County$", City), City, NA),
         City = ifelse(grepl(" County$", City), NA, City)) %>%
  select(Title, City, County, State, Group, Ideology, Year)
## and then remove "... County" from the county's name because that is redundant
hategroups_raw$County <- gsub(" County$", "", hategroups_raw$County)

## Same process but for Lousiana (Lousiana uses different names for counties)
parish_rows <- grep("Parish$", hategroups_raw$City)
hategroups_raw$County[parish_rows] <- hategroups_raw$City[parish_rows]
hategroups_raw$County <- gsub(" Parish$", "", hategroups_raw$County)
unwanted_3LA <- hategroups_raw$City %in% c("Tangipahoa Parish", "St. Tammany Parish", "East Baton Rouge Parish")
hategroups_raw$City[unwanted_3LA] <- NA_character_

### 1.6 Remove cities with word "Region" in it
hategroups_raw <- hategroups_raw[!grepl("Region", hategroups_raw$City, ignore.case = TRUE), ]

### 1.7 Fixing mispelled cities

## Altoona, PA
hategroups_raw$City <- gsub("Altona", "Altoona", hategroups_raw$City)
## De Kalb, Texas
hategroups_raw$City <- gsub("Dekalb|DeKalb", "De Kalb", hategroups_raw$City)
## Dennison, Texas*
hategroups_raw$City <- gsub("Dennison", "Denison", hategroups_raw$City)
## Albay, New York
hategroups_raw$City <- gsub("Albay", "Albany", hategroups_raw$City)
## Forth
hategroups_raw$City <- gsub("Forth Worth", "Fort Worth", hategroups_raw$City)
## Ameilia
hategroups_raw$City <- gsub("Ameilia", "Amelia", hategroups_raw$City)
## JACKSON
hategroups_raw$City <- gsub("JACKSON", "Jackson", hategroups_raw$City)
## Greater Los Angeles Area
hategroups_raw$City <- gsub("Greater Los Angeles Area", "Los Angeles", hategroups_raw$City)
## Southwest Florida (Suncoast chapter)
hategroups_raw$City <- gsub("Southwest Florida (Suncoast chapter)", "Suncoast Estates", hategroups_raw$City)
## East Saint Louis
hategroups_raw$City <- gsub("East Saint Louis", "East St. Louis", hategroups_raw$City)
## West Oakland
hategroups_raw$City <- gsub("West Oakland", "Oakland", hategroups_raw$City)
## West Chester
hategroups_raw$City <- gsub("West Chester", "Olde West Chester", hategroups_raw$City)
## Housotn
hategroups_raw$City <- gsub("Housotn", "Houston", hategroups_raw$City)
## Bay St Louis
hategroups_raw$City <- gsub("Bay St Louis", "Bay St. Louis", hategroups_raw$City)
## Beattlement Mesa
hategroups_raw$City <- gsub("Beattlement Mesa", "Battlement Mesa", hategroups_raw$City)
## benson
hategroups_raw$City <- gsub("benson", "Benson", hategroups_raw$City)
## Bethleham/Easton 
hategroups_raw$City <- gsub("Bethleham/Easton", "Bethlehem", hategroups_raw$City)
## Big Bear
hategroups_raw$City <- gsub("Big Bear", "Big Bear City", hategroups_raw$City)
## Binhamton
hategroups_raw$City <- gsub("Binhamton", "Binghamton", hategroups_raw$City)
## Bismark
hategroups_raw$City <- gsub("Bismark", "Bismarck", hategroups_raw$City)
## Black Canyon
hategroups_raw$City <- gsub("Black Canyon", "Black Canyon City", hategroups_raw$City)
## Brimingham
hategroups_raw$City <- gsub("Brimingham", "Birmingham", hategroups_raw$City)
## Brown Summitt
hategroups_raw$City <- gsub("Brown Summitt", "Browns Summit", hategroups_raw$City)
## Brunswich
hategroups_raw$City <- gsub("Brunswich", "Brunswick", hategroups_raw$City)
## Brushcreek
hategroups_raw$City <- gsub("Brushcreek", "Brush Creek", hategroups_raw$City)
## Byron and Byrum
hategroups_raw$City <- gsub("Byron", "Byram", hategroups_raw$City)
hategroups_raw$City <- gsub("Byrum", "Byram", hategroups_raw$City)
hategroups_raw$City[hategroups_raw$City == "Byram" & hategroups_raw$State == "Georgia"] <- "Byron"
## Calhoun
hategroups_raw$City[hategroups_raw$City == "Calhoun" & hategroups_raw$State == "Mississippi"] <- "Calhoun City"
## Carrolton
hategroups_raw$City <- gsub("Carrolton", "Carrollton", hategroups_raw$City)
## City of Venice
hategroups_raw$City <- gsub("City of Venice", "Venice", hategroups_raw$City)
## City of Winter Haven
hategroups_raw$City <- gsub("City of Winter Haven", "Winter Haven", hategroups_raw$City)
## Coeur d'Alene
hategroups_raw$City <- gsub("Coeur D`Alene|Coeur D'Alene", "Coeur d'Alene", hategroups_raw$City)
## columbia
hategroups_raw$City <- gsub("columbia", "Columbia", hategroups_raw$City)
## Corpus Christi | Nueces
hategroups_raw$City <- gsub("Corpus Christi | Nueces", "Corpus Christi", hategroups_raw$City)
## Corpus Christi|Corpus Christi
hategroups_raw$City <- gsub("Corpus Christi|Corpus Christi", "Corpus Christi", hategroups_raw$City)
## Daytona
hategroups_raw$City <- gsub("Daytona", "Daytona Beach", hategroups_raw$City)
## Elon College
hategroups_raw$City <- gsub("Elon College", "Elon", hategroups_raw$City)
## Fredricksburg
hategroups_raw$City <- gsub("Fredricksburg", "Fredericksburg", hategroups_raw$City)
## Robersdale
hategroups_raw$City <- gsub("Robersdale", "Robertsdale", hategroups_raw$City)
## Wichita
hategroups_raw$City <- gsub("Witchita", "Wichita", hategroups_raw$City)
## Willingboro
hategroups_raw$City <- gsub("Willingboro", "Willingboro Township", hategroups_raw$City)
## Wilkes Barre
hategroups_raw$City <- gsub("Wilkes Barre", "Wilkes-Barre", hategroups_raw$City)
## wichita
hategroups_raw$City <- gsub("wichita", "Wichita", hategroups_raw$City)
## Westminister
hategroups_raw$City <- gsub("Westminister", "Westminster", hategroups_raw$City)
## West Wego
hategroups_raw$City <- gsub("West Wego", "Westwego", hategroups_raw$City)
## Wells
hategroups_raw$City <- gsub("Wells", "Wells Township", hategroups_raw$City)
## Mineral Wells
hategroups_raw$City <- gsub("Mineral Wells Township", "Mineralwells", hategroups_raw$City)
## Waxahatchie
hategroups_raw$City <- gsub("Waxahatchie", "Waxahachie", hategroups_raw$City)
## Warwich
hategroups_raw$City <- gsub("Warwich", "Warwick", hategroups_raw$City)
# WalnutCreek
hategroups_raw$City <- gsub("WalnutCreek", "Walnut Creek", hategroups_raw$City)
# Wautauga
hategroups_raw$City <- gsub("Wautauga", "Watauga", hategroups_raw$City)
# St Louis
hategroups_raw$City <- gsub("St Louis|Saint Louis", "St. Louis", hategroups_raw$City)
# St Amant
hategroups_raw$City <- gsub("St Amant", "St. Amant", hategroups_raw$City)
# St Augustine
hategroups_raw$City <- gsub("St Augustine", "St. Augustine", hategroups_raw$City)
# St Charles
hategroups_raw$City <- gsub("St Charles", "St. Charles", hategroups_raw$City)
# St Petesburg, FL
hategroups_raw$City <- gsub("St Petesburg|Saint Petesburg|St Petersburg|Saint Petersburg", "St. Petersburg", hategroups_raw$City)
# St Paul                                 
hategroups_raw$City <- gsub("St Paul", "St. Paul", hategroups_raw$City)
# Simmms
hategroups_raw$City <- gsub("Simmms", "Simms", hategroups_raw$City)
# San Antonia
hategroups_raw$City <- gsub("San Antonia", "San Antonio", hategroups_raw$City)
# For Lauderdale
hategroups_raw$City <- gsub("Ft. Lauderdale|FT. Lauderdale|Ft Lauderdale|For Lauderdale|Lauderale|Fort Lauderdale|Fort Fort Lauderdale", "Fort Lauderdale", hategroups_raw$City)
# Fort Fort Lauderdale
hategroups_raw$City <- gsub("Fort Fort Lauderdale", "Fort Lauderdale", hategroups_raw$City)
# Tuscon
hategroups_raw$City <- gsub("Tuscon", "Tucson", hategroups_raw$City)
# Town of Richmond
hategroups_raw$City <- gsub("Town of Richmond", "Richmond", hategroups_raw$City)
# Richmondale
hategroups_raw$City <- gsub("Richmondale", "Richmond Dale", hategroups_raw$City)
# Town of Winona Lake
hategroups_raw$City <- gsub("Town of Winona Lake", "Winona Lake", hategroups_raw$City)
# San Bernadino
hategroups_raw$City <- gsub("San Bernadino", "San Bernardino", hategroups_raw$City)
# Mathison
hategroups_raw$City <- gsub("Mathison", "Mathiston", hategroups_raw$City)
# Vieanna
hategroups_raw$City <- gsub("Vieanna", "Vienna", hategroups_raw$City)
# Ventura
hategroups_raw$City <- gsub("Ventura", "San Buenaventura", hategroups_raw$City)
# Los Angeles/Ventura County
hategroups_raw$County <- gsub("Los Angeles/Ventura", "Ventura", hategroups_raw$County)
# Ty TY
hategroups_raw$City <- gsub("Ty TY", "Ty Ty", hategroups_raw$City)
# Tidewater
hategroups_raw$City <- gsub("Tidewater", "Tappahannock", hategroups_raw$City)
# Thibodeaux
hategroups_raw$City <- gsub("Thibodeaux", "Thibodaux", hategroups_raw$City)
# Shepardsville
hategroups_raw$City <- gsub("Shepardsville", "Shepherdsville", hategroups_raw$City)
# Salt Creek
hategroups_raw$City <- gsub("Salt Creek", "Sale Creek", hategroups_raw$City)
# Temecula
hategroups_raw$City <- gsub("Temecuala", "Temecula", hategroups_raw$City)
# Summersville
hategroups_raw$City <- gsub("Summersville", "Summerville", hategroups_raw$City)
# St. Mary's
hategroups_raw$City <- gsub("St. Mary's", "St. Marys", hategroups_raw$City)
# Southhaven
hategroups_raw$City <- gsub("Southhaven", "Southaven", hategroups_raw$City)
# Sherman oaks
hategroups_raw$City <- gsub("Sherman oaks", "Sherman Oaks", hategroups_raw$City)
# Arrowhead
hategroups_raw$City <- gsub("Arrowhead", "Lake Arrowhead", hategroups_raw$City)
# Summervile
hategroups_raw$City[hategroups_raw$City == "Summerville" & hategroups_raw$State == "West Virginia"] <- "Summersville"      
# South Vineland
hategroups_raw$City[hategroups_raw$City == "South Vineland" & hategroups_raw$State == "New Jersey"] <- "Vineland"      
# South bend
hategroups_raw$City[hategroups_raw$City == "South bend" & hategroups_raw$State == "Indiana"] <- "South Bend"
# Saint Thomas
hategroups_raw$City[hategroups_raw$City == "Saint Thomas" & hategroups_raw$State == "Pennsylvania"] <- "St. Thomas Township"
# Saint Paul
hategroups_raw$City[hategroups_raw$City == "Saint Paul" & hategroups_raw$State == "Minnesota"] <- "St. Paul"
# Saint Martinville
hategroups_raw$City[hategroups_raw$City == "Saint Martinville" & hategroups_raw$State == "Louisiana"] <- "St. Martinville"
# Saint James City
hategroups_raw$City[hategroups_raw$City == "Saint James City" & hategroups_raw$State == "Florida"] <- "St. James City"
# Saint James
hategroups_raw$City[hategroups_raw$City == "Saint James" & hategroups_raw$State == "New York"] <- "St. James"
# Saint Elmo
hategroups_raw$City[hategroups_raw$City == "Saint Elmo" & hategroups_raw$State == "Alabama"] <- "St. Elmo"
# Saint Cloud
hategroups_raw$City[hategroups_raw$City == "Saint Cloud" & hategroups_raw$State == "Florida"] <- "St. Cloud"
# Roxbury
hategroups_raw$City[hategroups_raw$City == "Roxbury" & hategroups_raw$State == "Massachusetts"] <- "Boston"
# Raymond, Town of
hategroups_raw$City[hategroups_raw$City == "Raymond, Town of" & hategroups_raw$State == "New Hampshire"] <- "Raymond"
# Robinsonville
hategroups_raw$City[hategroups_raw$City == "Robinsonville" & hategroups_raw$State == "Mississippi"] <- "Tunica Resorts"
# Pittsburg
hategroups_raw$City[hategroups_raw$City == "Pittsburg" & hategroups_raw$State == "Pennsylvania"] <- "Pittsburgh"
# PIttsburg
hategroups_raw$City[hategroups_raw$City == "PIttsburgh" & hategroups_raw$State == "Pennsylvania"] <- "Pittsburgh"
# Pineywoods
hategroups_raw$City[hategroups_raw$City == "Pineywoods" & hategroups_raw$State == "Mississippi"] <- "Piney Woods"
# Petosky
hategroups_raw$City[hategroups_raw$City == "Petosky" & hategroups_raw$State == "Michigan"] <- "Petoskey"
# Paso Robles
hategroups_raw$City[hategroups_raw$City == "Paso Robles" & hategroups_raw$State == "California"] <- "El Paso de Robles"
# Perkins
hategroups_raw$City[hategroups_raw$City == "Perkins" & hategroups_raw$State == "Illinois"] <- "Pekin"
# Pleasanthill
hategroups_raw$City[hategroups_raw$City == "Pleasanthill" & hategroups_raw$State == "Tennessee"] <- "Pleasant Hill"
# Pointblank
hategroups_raw$City[hategroups_raw$City == "Pointblank" & hategroups_raw$State == "Texas"] <- "Point Blank"
# Port Hadlock
hategroups_raw$City[hategroups_raw$City == "Port Hadlock" & hategroups_raw$State == "Washington"] <- "Port Hadlock-Irondale"
# New York City
hategroups_raw$City[hategroups_raw$City == "New York City" & hategroups_raw$State == "New York"] <- "New York"
# Atalla
hategroups_raw$City[hategroups_raw$City == "Atalla" & hategroups_raw$State == "Alabama"] <- "Attalla"
# Bellville
hategroups_raw$City[hategroups_raw$City == "Bellville" & hategroups_raw$State == "Michigan"] <- "Belleville"
# Big Bear City Lake
hategroups_raw$City[hategroups_raw$City == "Big Bear City Lake" & hategroups_raw$State == "California"] <- "Big Bear City"
# Big Fork
hategroups_raw$City[hategroups_raw$City == "Big Fork" & hategroups_raw$State == "Montana"] <- "Bigfork"
# Black Canyon City City
hategroups_raw$City[hategroups_raw$City == "Black Canyon City City" & hategroups_raw$State == "Arizona"] <- "Black Canyon City"
# Bourdonnais
hategroups_raw$City[hategroups_raw$City == "Bourdonnais" & hategroups_raw$State == "Illinois"] <- "Bourbonnais"
# Bridgetown
hategroups_raw$City[hategroups_raw$City == "Bridgetown" & hategroups_raw$State == "Missouri"] <- "Bridgeton"
# Brownsfield
hategroups_raw$City[hategroups_raw$City == "Brownsfield" & hategroups_raw$State == "Louisiana"] <- "Brownfields"
# Brownsville
hategroups_raw$City[hategroups_raw$City == "Brownsville" & hategroups_raw$State == "California"] <- "Challenge-Brownsville"
# Mt Vernon
hategroups_raw$City[hategroups_raw$City == "Mt Vernon" & hategroups_raw$State == "Texas"] <- "Mt. Vernon"
# Mt Airy
hategroups_raw$City[hategroups_raw$City == "Mt Airy" & hategroups_raw$State == "North Carolina"] <- "Mount Airy"
# Moxon
hategroups_raw$City[hategroups_raw$City == "Moxon" & hategroups_raw$State == "Montana"] <- "Noxon"
# Mt. Pleasant, OH
hategroups_raw$City[hategroups_raw$City == "Mt. Pleasant" & hategroups_raw$State == "Ohio"] <- "Mount Pleasant"
# Mt. Pleasant, TX
hategroups_raw$City[hategroups_raw$City == "Mt. Pleasant" & hategroups_raw$State == "Texas"] <- "Mount Pleasant"
# Mt. Vernon, TX
hategroups_raw$City[hategroups_raw$City == "Mt. Vernon" & hategroups_raw$State == "Texas"] <- "Mount Vernon"
# New Castle, ME
hategroups_raw$City[hategroups_raw$City == "New Castle" & hategroups_raw$State == "Maine"] <- "Newcastle"
#NewOrleans
hategroups_raw$City[hategroups_raw$City == "NewOrleans" & hategroups_raw$State == "Louisiana"] <- "New Orleans"
# Clarkston
hategroups_raw$City[hategroups_raw$City == "Clarkston" & hategroups_raw$State == "Missouri"] <- "Clarkton"
# Columbia Heights
hategroups_raw$City[hategroups_raw$City == "Columbia Heights" & hategroups_raw$State == "South Carolina"] <- "Columbia"
# Corpus Christi|Corpus Christi
hategroups_raw$City[hategroups_raw$City == "Corpus Christi|Corpus Christi" & hategroups_raw$State == "Texas"] <- "Corpus Christi"
# Costa Mesa (AZ -> CA)
hategroups_raw$State[hategroups_raw$City == "Costa Mesa" & hategroups_raw$State == "Arizona"] <- "California"
# Culluwhee
hategroups_raw$City[hategroups_raw$City == "Culluwhee" & hategroups_raw$State == "North Carolina"] <- "Cullowhee"
# Daytona Beach Beach
hategroups_raw$City[hategroups_raw$City == "Daytona Beach Beach" & hategroups_raw$State == "Florida"] <- "Daytona Beach"
# Defuniak Springs
hategroups_raw$City[hategroups_raw$City == "Defuniak Springs" & hategroups_raw$State == "Florida"] <- "DeFuniak Springs"
# Desoto
hategroups_raw$City[hategroups_raw$City == "Desoto" & hategroups_raw$State == "Texas"] <- "DeSoto"
# Dublin, "NJ"
hategroups_raw$State[hategroups_raw$City == "Dublin" & hategroups_raw$State == "New Jersey"] <- "New Hampshire"
# Fork McCoy
hategroups_raw$City[hategroups_raw$City == "Fork McCoy" & hategroups_raw$State == "Florida"] <- "Fort McCoy"
hategroups_raw$City[hategroups_raw$City == "Fort McCroy" & hategroups_raw$State == "Florida"] <- "Fort McCoy"
hategroups_raw$City[hategroups_raw$City == "Fort Mccoy" & hategroups_raw$State == "Florida"] <- "Fort McCoy"

# Freemont
hategroups_raw$City[hategroups_raw$City == "Freemont" & hategroups_raw$State == "Ohio"] <- "Fremont"
# Ft. Myers
hategroups_raw$City[hategroups_raw$City == "Ft Myers" & hategroups_raw$State == "Florida"] <- "Fort Myers"
# Ft. Oglethorpe
hategroups_raw$City[hategroups_raw$City == "Ft. Oglethorpe" & hategroups_raw$State == "Georgia"] <- "Fort Oglethorpe"
# Ft. Worth
hategroups_raw$City[hategroups_raw$City == "Ft. Worth" & hategroups_raw$State == "Texas"] <- "Fort Worth"
# Gainsville
hategroups_raw$City[hategroups_raw$City == "Gainsville" & hategroups_raw$State == "Florida"] <- "Gainesville"
# Goose Creek
hategroups_raw$City[hategroups_raw$City == "Goose Creek" & hategroups_raw$State == "Texas"] <- "Baytown"
# Grandbury
hategroups_raw$City[hategroups_raw$City == "Grandbury" & hategroups_raw$State == "Texas"] <- "Granbury"
# Green Brier
hategroups_raw$City[hategroups_raw$City == "Green Brier" & hategroups_raw$State == "Tennessee"] <- "Greenbrier"
# Greensburn
hategroups_raw$City[hategroups_raw$City == "Greensburn" & hategroups_raw$State == "Pennsylvania"] <- "Greensburg"
# Greensville
hategroups_raw$City[hategroups_raw$City == "Greensville" & hategroups_raw$State == "Texas"] <- "Greenville"
# Greenville
hategroups_raw$City[hategroups_raw$City == "Greenville" & hategroups_raw$State == "Tennessee"] <- "Greeneville"
# Hamptown Twp
hategroups_raw$City[hategroups_raw$City == "Hampton Twp" & hategroups_raw$State == "Pennsylvania"] <- "Hampton Township"
# Hamilton
hategroups_raw$City[hategroups_raw$City == "Hamilton" & hategroups_raw$State == "New Jersey"] <- "Hamilton Township"
# Hannibal, LA
hategroups_raw$State[hategroups_raw$City == "Hannibal" & hategroups_raw$State == "Louisiana"] <- "Missouri"
# Hathorne
hategroups_raw$City[hategroups_raw$City == "Hathorne" & hategroups_raw$State == "Massachusetts"] <- "Danvers"
# haverhill
hategroups_raw$City[hategroups_raw$City == "haverhill" & hategroups_raw$State == "Massachusetts"] <- "Haverhill"
# Hillsborough
hategroups_raw$City[hategroups_raw$City == "Hillsborough" & hategroups_raw$State == "New Hampshire"] <- "Hillsborough"
# Hilton Head
hategroups_raw$City[hategroups_raw$City == "Hilton Head" & hategroups_raw$State == "South Carolina"] <- "Hilton Head Island"
# Pulaska
hategroups_raw$City[hategroups_raw$City == "Pulaska" & hategroups_raw$State == "Tennessee"] <- "Pulaski"
# PPBEACH
hategroups_raw$City[hategroups_raw$City == "Pt. Pleasant Beach" & hategroups_raw$State == "New Jersey"] <- "Point Pleasant Beach"
# Holland
hategroups_raw$City[hategroups_raw$City == "Holland Township" & hategroups_raw$State == "New Jersey"] <- "Holland"
# Huntingdon Beach
hategroups_raw$City[hategroups_raw$City == "Huntingdon Beach" & hategroups_raw$State == "California"] <- "Huntington Beach"
# Jackson
hategroups_raw$City[hategroups_raw$City == "Jackson" & hategroups_raw$State == "Florida"] <- "Jacksonville"
# Jasckson
hategroups_raw$City[hategroups_raw$City == "Jasckson" & hategroups_raw$State == "Mississippi"] <- "Jackson"
# Kingston
hategroups_raw$City[hategroups_raw$City == "Kingston" & hategroups_raw$State == "North Carolina"] <- "Kinston"
# La Crescenta
hategroups_raw$City[hategroups_raw$City == "La Crescenta" & hategroups_raw$State == "California"] <- "La Crescenta-Montrose"
# La Fayette, GA
hategroups_raw$City[hategroups_raw$City == "La Fayette" & hategroups_raw$State == "Georgia"] <- "LaFayette"
# LaFayette, TN
hategroups_raw$City[hategroups_raw$City == "LaFayette" & hategroups_raw$State == "Tennessee"] <- "Lafayette"
# La Plume Twp
hategroups_raw$City[hategroups_raw$City == "La Plume Twp" & hategroups_raw$State == "Pennsylvania"] <- "La Plume Township"
# Lake Worth Beach
hategroups_raw$City[hategroups_raw$City == "Lake Worth Beach" & hategroups_raw$State == "Florida"] <- "Lake Worth"
# La Marque
hategroups_raw$City[hategroups_raw$City == "LaMarque" & hategroups_raw$State == "Texas"] <- "La Marque"
# lancaster
hategroups_raw$City[hategroups_raw$City == "lancaster" & hategroups_raw$State == "California"] <- "Lancaster"
# LaPorte
hategroups_raw$City[hategroups_raw$City == "LaPorte" & hategroups_raw$State == "Colorado"] <- "Laporte"
# LaPorte
hategroups_raw$City[hategroups_raw$City == "Danridge" & hategroups_raw$State == "Tennessee"] <- "Dandridge"
# Little Egg Harbor City
hategroups_raw$City[hategroups_raw$City == "Little Egg Harbor City" & hategroups_raw$State == "New Jersey"] <- "Little Egg Harbor"
hategroups_raw$City[hategroups_raw$City == "Little Egg Harbor Township" & hategroups_raw$State == "New Jersey"] <- "Little Egg Harbor"
# Lockwook
hategroups_raw$City[hategroups_raw$City == "Lockwook" & hategroups_raw$State == "Missouri"] <- "Lockwood"
# Loxahatchee
hategroups_raw$City[hategroups_raw$City == "Loxahatchee" & hategroups_raw$State == "Florida"] <- "Loxahatchee Groves"
# Lynwood
hategroups_raw$City[hategroups_raw$City == "Lynwood" & hategroups_raw$State == "Washington"] <- "Lynnwood"
# macon
hategroups_raw$City[hategroups_raw$City == "macon" & hategroups_raw$State == "Georgia"] <- "Macon"
# Mammoth Springs
hategroups_raw$City[hategroups_raw$City == "Mammoth Springs" & hategroups_raw$State == "Arkansas"] <- "Mammoth Spring"
# Manddeville
hategroups_raw$City[hategroups_raw$City == "Manddeville" & hategroups_raw$State == "Louisiana"] <- "Mandeville"
hategroups_raw$City[hategroups_raw$City == "Mandefille" & hategroups_raw$State == "Louisiana"] <- "Mandeville"
# Maple Shade Township
hategroups_raw$City[hategroups_raw$City == "Maple Shade Township" & hategroups_raw$State == "New Jersey"] <- "Maple Shade"
hategroups_raw$City[hategroups_raw$City == "Maple Shade Twp" & hategroups_raw$State == "New Jersey"] <- "Maple Shade"
# Marina Del Rey
hategroups_raw$City[hategroups_raw$City == "Marina Del Rey" & hategroups_raw$State == "California"] <- "Marina del Rey"
# Massesna
hategroups_raw$City[hategroups_raw$City == "Massesna" & hategroups_raw$State == "New York"] <- "Massena"
# Matinicus Island
hategroups_raw$City[hategroups_raw$City == "Matinicus Island" & hategroups_raw$State == "Maine"] <- "Matinicus Isle"
# Mc Lean
hategroups_raw$City[hategroups_raw$City == "Mc Lean" & hategroups_raw$State == "Virginia"] <- "McLean"
# Oley
hategroups_raw$City[hategroups_raw$City == "Oley" & hategroups_raw$State == "Pennsylvania"] <- "Friedensburg"
# Oklahoma  City
hategroups_raw$City[hategroups_raw$City == "Oklahoma  City" & hategroups_raw$State == "Oklahoma"] <- "Oklahoma City"
# La Porte
hategroups_raw$City[hategroups_raw$City == "LaPorte" & hategroups_raw$State == "Texas"] <- "La Porte"
#Menderhall
hategroups_raw$City[hategroups_raw$City == "Menderhall" & hategroups_raw$State == "Mississippi"] <- "Mendenhall"
#Milawukee
hategroups_raw$City[hategroups_raw$City == "Milawukee" & hategroups_raw$State == "Wisconsin"] <- "Milwaukee"
#Millspring, NC
hategroups_raw$City[hategroups_raw$City == "Millspring" & hategroups_raw$State == "North Carolina"] <- "Mill Spring"
#Minneaplis
hategroups_raw$City[hategroups_raw$City == "Minneaplis" & hategroups_raw$State == "Minnesota"] <- "Minneapolis"
#Monroula
hategroups_raw$City[hategroups_raw$City == "Monroula" & hategroups_raw$State == "California"] <- "Monrovia"
#North Baport
hategroups_raw$City[hategroups_raw$City == "North Baport" & hategroups_raw$State == "Minnesota"] <- "Bayport"
hategroups_raw$City[hategroups_raw$City == "North Bayport" & hategroups_raw$State == "Minnesota"] <- "Bayport"
# North Minneapolis
hategroups_raw$City[hategroups_raw$City == "North Minneapolis" & hategroups_raw$State == "Minnesota"] <- "Minneapolis"
# North Phoenix
hategroups_raw$City[hategroups_raw$City == "North Phoenix" & hategroups_raw$State == "Arizona"] <- "Phoenix"
# North Providence
hategroups_raw$City[hategroups_raw$City == "North Providence" & hategroups_raw$State == "Rhode Island"] <- "Providence"
# North Versailles
hategroups_raw$City[hategroups_raw$City == "North Versailles" & hategroups_raw$State == "Pennsylvania"] <- "Versailles"
# Ogalalla
hategroups_raw$City[hategroups_raw$City == "Ogalalla" & hategroups_raw$State == "Nebraska"] <- "Ogallala"
# Niagrara Falls
hategroups_raw$City[hategroups_raw$City == "Niagrara Falls" & hategroups_raw$State == "New York"] <- "Niagara Falls"
# South Daytona Beach
hategroups_raw$City[hategroups_raw$City == "South Daytona Beach" & hategroups_raw$State == "Florida"] <- "Daytona Beach"
# Prospects Heights
hategroups_raw$City[hategroups_raw$City == "Prospects Heights" & hategroups_raw$State == "Illinois"] <- "Prospect Heights"

### 1.7 Remove non-existent cities
stupid_strings <- c("eastern North Carolina", "northern Mississippi", "Northwest Texas", 
                    "Northern MN", "Northern Indiana", "Northern California", "Shenandoah Valley", 
                    "Northeast Texas", "Northern", "Southern California", 
                    "Southern", "Statewide" , "Southest LA", "Southwest", 
                    "souther South Carolina", "Southeast Texas", 
                    "Southern Illinois", "South Georgia", "South East Texas", "Eastern Pennsylvania", 
                    "South East", "South Central Texas","SE", "Southeast", 
                    "Northeast Texas", "East TN", "East Texas", "Delaware",
                    "East Pennsylvania", "East Georgia", "East Central TN", 
                    "West Georgia", "Southwest Virginia", "Southwest", "Red Rock Canyon",
                    "Central TN", "Central", "Central California", "North Georgia",
                    "Central Missouri", "Central New Jersey", "Central Tennessee","Central Virginia",
                    "Western Michigan", "West Texas", "unkn", "Unknown", "Upstate", "Upstate New York",
                    "upstate New York", "Northern MI", "Downstate", "Potomac Highlands", "lowcountry chapter", "upcountry chapter")
hategroups_raw <- subset(hategroups_raw, !(hategroups_raw$City %in% stupid_strings))
### 1.7.2 "Lakewood"
hategroups_raw <- hategroups_raw %>%
  filter(!(str_detect(City, "Lakewood") & State == "Oklahoma"))
### 1.7.3 "Miami, Massachusetts"
hategroups_raw <- hategroups_raw %>%
  filter(!(str_detect(City, "Miami") & State == "Massachusetts"))
### 1.7.4 "Mid-Country, Missouri"
hategroups_raw <- hategroups_raw %>%
  filter(!(str_detect(City, "Mid-Country") & State == "Missouri"))
### 1.7.5 "North Texas"
hategroups_raw <- hategroups_raw %>%
  filter(!(str_detect(City, "North") & State == "Texas"))
### 1.7.5 "Northwest
hategroups_raw <- hategroups_raw %>%
  filter(!(str_detect(City, "Northwest") & State == "Georgia"))
### 1.7.6 "Southern Missiouri"
hategroups_raw <- hategroups_raw %>%
  filter(!(str_detect(City, "Southern Missiouri") & State == "Missouri"))
### 1.7.7 "Southwest Florida (Suncoast chapter)"
hategroups_raw <- hategroups_raw %>%
  filter(!(str_detect(City, "Suncoast chapter") & State == "Florida"))

### 1.8 Separate "Dallas/Fort Worth" data into two separate cities
hategroups_raw <- hategroups_raw %>%
  mutate(City = ifelse(City %in% c("Dallas/Fort Worth", "Dallas/Ft Worth"), "Dallas", City))

### 1.9 Create City_State_HG column
hategroups_raw$City_State_HG <- paste(hategroups_raw$City, hategroups_raw$State, sep = ", ")

### 1.10 Create hategroups_raw2 (no counties)
hategroups_raw2 <- hategroups_raw[!is.na(hategroups_raw$City), ]

#===============================================================================#
# 2. MERGING GEOLOCATIONS AND HATE GROUPS
#===============================================================================#

# download the uscities.csv file from https://simplemaps.com/data/us-cities

### 2.1 create "geolocation_raw"
geolocation_raw <- read.csv("uscities.csv")
### 2.2 create "geolocation_raw2"
geolocation_raw2 <- subset(geolocation_raw, select = c("city", "county_name", "state_name", "county_fips", "lat", "lng"))
### 2.3 Rename all columns from both dataframes so they're the same
geolocation_raw2 <- geolocation_raw2 %>%
  rename(
    Cityg = city,
    Stateg = state_name,
    Countyg = county_name,
    FIPSg = county_fips,
  )
### 2.4 Clean mispelled cities
## Cherry Hill
geolocation_raw2$City <- gsub("Cherry Hill Mall", "Cherry Hill", geolocation_raw2$City)

### 2.5 Create "City,state" for geolocation_raw2
geolocation_raw2$City_stategr <- paste(geolocation_raw2$Cityg, geolocation_raw2$Stateg, sep = ", ")

### Run a loop that creates matches

# Loop through each row in hategroups_raw and find matches in geolocation_raw2
for (i in 1:nrow(hategroups_raw)) {
  # Get the City_State_HG for the current row in hategroups_raw
  city_state_hg <- hategroups_raw$City_State_HG[i]
  
  # Find the corresponding row index in geolocation_raw2 where City_stategr matches
  match_index <- which(geolocation_raw2$City_stategr == city_state_hg)
  
  # If there's a match, update County in hategroups_raw with Countyg from geolocation_raw2
  if (length(match_index) > 0) {
    hategroups_raw$County[i] <- geolocation_raw2$Countyg[match_index[1]]
  }
}

### 2.6 Fill out cells that uscities.csv cannot satisfy (small cities, unincorporated towns)
# ghost towns have been marked with 🕇

# Adolphus, KY
hategroups_raw$County[hategroups_raw$City_State_HG == "Adolphus, Kentucky"] <- "Allen"
# Alden, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Alden, New York"] <- "Erie"
# Amherst, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Amherst, New York"] <- "Erie"
# Antioch, TN
hategroups_raw$County[hategroups_raw$City_State_HG == "Antioch, Tennessee"] <- "Davidson"
# Zuni, VA
hategroups_raw$County[hategroups_raw$City == "Zuni"] <- "Isle of Wight"
# Yoder, IN
hategroups_raw$County[hategroups_raw$City == "Yoder"] <- "Allen"
# Woodridge, VA
hategroups_raw$County[hategroups_raw$City == "Woodridge"] <- "Albemarle"
# Woodlawn, TN
hategroups_raw$County[hategroups_raw$City == "Woodlawn"] <- "Montgomery"
# Woodbridge Township, NJ
hategroups_raw$County[hategroups_raw$City == "Woodbridge Township"] <- "Middlesex"
# Willingboro Township, NJ
hategroups_raw$County[hategroups_raw$City == "Willingboro Township"] <- "Burlington"
# Winchester, NH
hategroups_raw$County[hategroups_raw$City == "Winchester"] <- "Cheshire"
# Wildwood, PA
hategroups_raw$County[hategroups_raw$City == "Wildwood"] <- "Allegheny"
# White Plains, MD
hategroups_raw$County[hategroups_raw$City == "White Plains"] <- "Charles"
# White Oak, GA
hategroups_raw$County[hategroups_raw$City == "White Oak"] <- "Camden"
# Wellborn, FL
hategroups_raw$County[hategroups_raw$City == "Wellborn"] <- "Suwannee"
# Wellington, NV
hategroups_raw$County[hategroups_raw$City == "Wellington"] <- "Lyon"
# Wells Township, MI
hategroups_raw$County[hategroups_raw$City == "Wells Township"] <- "Delta"
# Warwick, NY
hategroups_raw$County[hategroups_raw$City == "Warwick"] <- "Orange"
# Wappapello, MO
hategroups_raw$County[hategroups_raw$City == "Wappapello"] <- "Wayne"
# Snow Camp, NC
hategroups_raw$County[hategroups_raw$City == "Snow Camp"] <- "Alamance"
# Simms, TX
hategroups_raw$County[hategroups_raw$City == "Simms"] <- "Bowie"
# Silver Creek, GA
hategroups_raw$County[hategroups_raw$City == "Silver Creek"] <- "Floyd"
# Shirley, NY
hategroups_raw$County[hategroups_raw$City == "Shirly"] <- "Shirley"
# Richmond, NH
hategroups_raw$County[hategroups_raw$City_State_HG == "Richmond, New Hampshire"] <- "Cheshire"
# Toney, AL
hategroups_raw$County[hategroups_raw$City_State_HG == "Toney, Alabama"] <- "Madison"
# Toms River, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Toms River, New Jersey"] <- "Ocean"
# Achilles, VA
hategroups_raw$County[hategroups_raw$City_State_HG == "Achilles, Virginia"] <- "Gloucester"
# Alta Loma, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "Alta Loma, California"] <- "San Bernardino"
# Wallingford, CT
hategroups_raw$County[hategroups_raw$City_State_HG == "Wallingford, Connecticut"] <- "New Haven"
# Waldoboro, MN (it' s actually in ME)
hategroups_raw$State[hategroups_raw$City_State_HG == "Waldoboro, Minnesota"] <- "Maine"
# Waldoboro, MN (County)
hategroups_raw$County[hategroups_raw$City_State_HG == "Waldoboro, Maine"] <- "Lincoln"
hategroups_raw$City_State_HG <- paste(hategroups_raw$City, hategroups_raw$State, sep = ", ")
hategroups_raw$County[hategroups_raw$City_State_HG == "Waldoboro, Maine"] <- "Lincoln"
# Webster, MS
hategroups_raw$County[hategroups_raw$City_State_HG == "Webster, Mississippi"] <- "Webster"
# Ulysses, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Ulysses, Pennsylvania"] <- "Potter"
# Upper Darby, PA 
hategroups_raw$County[hategroups_raw$City_State_HG == "Upper Darby, Pennsylvania"] <- "Delaware"
# Vale, NC
hategroups_raw$County[hategroups_raw$City_State_HG == "Vale, North Carolina"] <- "Lincoln"
# Spicewood, TX
hategroups_raw$County[hategroups_raw$City_State_HG == "Spicewood, Texas"] <- "Travis"
# Sparta, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Sparta, New Jersey"] <- "Sussex"
# Viera, FL
hategroups_raw$County[hategroups_raw$City_State_HG == "Viera, Florida"] <- "Brevard"
# Ty Ty, GA
hategroups_raw$County[hategroups_raw$City_State_HG == "Ty Ty, Georgia"] <- "Tift"
# Van Nuys, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "Van Nuys, California"] <- "Los Angeles"
# Valley Village, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "Valley Village, California"] <- "Los Angeles"
# Tunas, MO
hategroups_raw$County[hategroups_raw$City_State_HG == "Tunas, Missouri"] <- "Dallas"
# Tollesboro, KY
hategroups_raw$County[hategroups_raw$City_State_HG == "Tollesboro, Kentucky"] <- "Lewis"
# Star, MS
hategroups_raw$County[hategroups_raw$City_State_HG == "Star, Mississippi"] <- "Rankin"
# Sherman Oaks, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "Sherman Oaks, California"] <- "Los Angeles"
# Shawnee on Delaware, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Shawnee on Delaware, Pennsylvania"] <- "Monroe"
# Shawnee Mission, KA
hategroups_raw$County[hategroups_raw$City_State_HG == "Shawnee Mission, Kansas"] <- "Johnson"
# Shady Valley, TN
hategroups_raw$County[hategroups_raw$City_State_HG == "Shady Valley, Tennessee"] <- "Johnson"
# Shady Grove, FL
hategroups_raw$County[hategroups_raw$City_State_HG == "Shady Grove, Florida"] <- "Taylor"
# Schaghticoke, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Schaghticoke, New York"] <- "Rensselaer"
# Santa Fe, TN
hategroups_raw$County[hategroups_raw$City_State_HG == "Santa Fe, Tennessee"] <- "Maury"
# San Ysidro, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "San Ysidro, California"] <- "San Diego"
# San Pedro, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "San Pedro, California"] <- "Los Angeles"
# San Gabriel Valley, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "San Gabriel Valley, California"] <- "Los Angeles"
# San Fernando Valley, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "San Fernando Valley, California"] <- "Los Angeles"
# Tecate, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "Tecate, California"] <- "San Diego"
# Talbott, TN
hategroups_raw$County[hategroups_raw$City_State_HG == "Talbott, Tennessee"] <- "Hamblen"
# Sutton, MA
hategroups_raw$County[hategroups_raw$City_State_HG == "Sutton, Massachusetts"] <- "Worcester"
# Sunset Beach, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "Sunset Beach, California"] <- "Orange"
# Stratford, CT
hategroups_raw$County[hategroups_raw$City_State_HG == "Stratford, Connecticut"] <- "Fairfield"
# Stoughton, MA 
hategroups_raw$County[hategroups_raw$City_State_HG == "Stoughton, Massachusetts"] <- "Norfolk"
# Still River, MA
hategroups_raw$County[hategroups_raw$City_State_HG == "Still River, Massachusetts"] <- "Worcester"
# St. Amant, LA
hategroups_raw$County[hategroups_raw$City_State_HG == "St. Amant, Louisiana"] <- "Ascension"
# Ashbury Park, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Ashbury Park, New Jersey"] <- "Monmouth"
# Tennessee Colony, TX
hategroups_raw$County[hategroups_raw$City_State_HG == "Tennessee Colony, Texas"] <- "Anderson"
# Sunland, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "Sunland, California"] <- "Los Angeles"
# Sterling, MA
hategroups_raw$County[hategroups_raw$City_State_HG == "Sterling, Massachusetts"] <- "Worcester"
# Steens, MS
hategroups_raw$County[hategroups_raw$City_State_HG == "Steens, Mississippi"] <- "Lowndes"
# South Walpole, MA
hategroups_raw$County[hategroups_raw$City_State_HG == "South Walpole, Massachusetts"] <- "Norfolk"
# Thurmond, NC
hategroups_raw$County[hategroups_raw$City_State_HG == "Thurmond, North Carolina"] <- "Wilkes"
# Sagle, ID
hategroups_raw$County[hategroups_raw$City_State_HG == "Sagle, Idaho"] <- "Bonner"
# Sacramento, CA
hategroups_raw$State[hategroups_raw$City_State_HG == "Sacramento, Florida"] <- "California"
# Ruther Glen, VA
hategroups_raw$County[hategroups_raw$City_State_HG == "Ruther Glen, Virginia"] <- "Caroline"
# Raymond, NH
hategroups_raw$County[hategroups_raw$City_State_HG == "Raymond, New Hampshire"] <- "Rockingham"
# Readyville, TN
hategroups_raw$County[hategroups_raw$City_State_HG == "Readyville, Tennessee"] <- "Cannon"
# Redford, MI
hategroups_raw$County[hategroups_raw$City_State_HG == "Redford, Michigan"] <- "Wayne"
# Reseda, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "Reseda, California"] <- "Los Angeles"
# Revere, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Revere, Pennsylvania"] <- "Bucks"
# Rhodes, MI
hategroups_raw$County[hategroups_raw$City_State_HG == "Rhodes, Michigan"] <- "Gladwin"
# Richland, SC
hategroups_raw$County[hategroups_raw$City_State_HG == "Richland, South Carolina"] <- "Oconee"
# Ridgedale, MO
hategroups_raw$County[hategroups_raw$City_State_HG == "Ridgedale, Missouri"] <- "Taney"
# Ringgold, VA
hategroups_raw$County[hategroups_raw$City_State_HG == "Ringgold, Virginia"] <- "Pittsylvania"
# Rockledge, GA
hategroups_raw$County[hategroups_raw$City_State_HG == "Rockledge, Georgia"] <- "Laurens"
# St. Thomas Township, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "St. Thomas Township, Pennsylvania"] <- "Franklin"
# St. Elmo, AL
hategroups_raw$County[hategroups_raw$City_State_HG == "St. Elmo, Alabama"] <- "Mobile"
# Pewaukee, WI
hategroups_raw$County[hategroups_raw$City_State_HG == "Pewaukee, Wisconsin"] <- "Waukesha"
# Perkinston, MS
hategroups_raw$County[hategroups_raw$City_State_HG == "Perkinston, Mississippi"] <- "Stone"
# Pemberton Township, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Pemberton Township, New Jersey"] <- "Burlington"
# Pelham, NC
hategroups_raw$County[hategroups_raw$City_State_HG == "Pelham, North Carolina"] <- "Caswell"
# Parishville, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Parishville, New York"] <- "St. Lawrence"
# Parrish, FL
hategroups_raw$County[hategroups_raw$City_State_HG == "Parrish, Florida"] <- "Manatee"
# Piney Woods, MS
hategroups_raw$County[hategroups_raw$City_State_HG == "Piney Woods, Mississippi"] <- "Rankin"
# Pleasant Unity, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Pleasant Unity, Pennsylvania"] <- "Westmoreland"
# Ashland, NH
hategroups_raw$County[hategroups_raw$City_State_HG == "Ashland, New Hampshire"] <- "Grafton"
# Astoria, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Astoria, New York"] <- "Queens"
# Auburn, NH
hategroups_raw$County[hategroups_raw$City_State_HG == "Auburn, New Hampshire"] <- "Rockingham"
# Austin, KY
hategroups_raw$County[hategroups_raw$City_State_HG == "Austin, Kentucky"] <- "Barren"    
# Bankston, AL
hategroups_raw$County[hategroups_raw$City_State_HG == "Bankston, Alabama"] <- "Fayette"    
# Bath, OH
hategroups_raw$County[hategroups_raw$City_State_HG == "Bath, Ohio"] <- "Summit"  
# Baxter, KY
hategroups_raw$County[hategroups_raw$City_State_HG == "Baxter, Kentucky"] <- "Harlan"
# Bayse, VA
hategroups_raw$County[hategroups_raw$City_State_HG == "Bayse, Virginia"] <- "Shenandoah"
# Bedford, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Bedford, Pennsylvania"] <- "Bedford"
# Belleville, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Belleville, New Jersey"] <- "Essex"
# Berkeley Springs, WV
hategroups_raw$County[hategroups_raw$City_State_HG == "Berkeley Springs, West Virginia"] <- "Morgan"
# Birmingham, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Birmingham, New Jersey"] <- "Burlington"
# Blackwell, MO
hategroups_raw$County[hategroups_raw$City_State_HG == "Blackwell, Missouri"] <- "Washington"
# Bomoseen, VT
hategroups_raw$County[hategroups_raw$City_State_HG == "Bomoseen, Vermont"] <- "Rutland"
# Bonaire, GA
hategroups_raw$County[hategroups_raw$City_State_HG == "Bonaire, Georgia"] <- "Houston"
# Bordentown, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Bordentown, New Jersey"] <- "Burlington"
# Brick, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Brick, New Jersey"] <- "Ocean"
# Bridgeton, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Bridgeton, New Jersey"] <- "Cumberland"
# Broadalbin, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Broadalbin, New York"] <- "Fulton"
# Browns Summit, NC
hategroups_raw$County[hategroups_raw$City_State_HG == "Browns Summit, North Carolina"] <- "Guilford"
# Brush Creek, TN
hategroups_raw$County[hategroups_raw$City_State_HG == "Brush Creek, Tennessee"] <- "Williamson"
# Mt. Holly, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Mt. Holly, New Jersey"] <- "Burlington"
# Montville, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Montville, New Jersey"] <- "Morris"
# Nassau, DE
hategroups_raw$County[hategroups_raw$City_State_HG == "Nassau, Delaware"] <- "Sussex"
# New Fairfield, CT
hategroups_raw$County[hategroups_raw$City_State_HG == "New Fairfield, Connecticut"] <- "Fairfield"
# New Hartford, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "New Hartford, New York"] <- "Oneida"
# New Lebanon, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "New Lebanon, New York"] <- "Columbia"
# Newburgh, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Newburgh, New York"] <- "Orange"
# Newhall, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "Newhall, California"] <- "Los Angeles"
# Newcastle, ME
hategroups_raw$County[hategroups_raw$City_State_HG == "Newcastle, Maine"] <- "Lincoln"
# Bunker Hill, WV
hategroups_raw$County[hategroups_raw$City_State_HG == "Bunker Hill, West Virginia"] <- "Berkeley"
# Calcasieu, LA
hategroups_raw$County[hategroups_raw$City_State_HG == "Calcasieu, Louisiana"] <- "Rapides"
# Canterbury, CT
hategroups_raw$County[hategroups_raw$City_State_HG == "Canterbury, Connecticut"] <- "Windham"
# Canton, MI
hategroups_raw$County[hategroups_raw$City_State_HG == "Canton, Michigan"] <- "Wayne"
# Canyon Country, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "Canyon Country, California"] <- "Los Angeles"
# Cape Cod, MA
hategroups_raw$County[hategroups_raw$City_State_HG == "Cape Cod, Massachusetts"] <- "Barnstable"
# Careywood, ID
hategroups_raw$County[hategroups_raw$City_State_HG == "Careywood, Idaho"] <- "Bonner"
# Casey Creek, KY
hategroups_raw$County[hategroups_raw$City_State_HG == "Casey Creek, Kentucky"] <- "Adair"
# Chattaroy, WA
hategroups_raw$County[hategroups_raw$City_State_HG == "Chattaroy, Washington"] <- "Spokane"
# Cheektowaga, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Cheektowaga, New York"] <- "Erie"
# Cherry Hill, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Cherry Hill, New Jersey"] <- "Camden"
# Cheshire, CT
hategroups_raw$County[hategroups_raw$City_State_HG == "Cheshire, Connecticut"] <- "New Haven"
# Chesterfield, VA
hategroups_raw$County[hategroups_raw$City_State_HG == "Chesterfield, Virginia"] <- "Chesterfield"
# Churchville, MD 
hategroups_raw$County[hategroups_raw$City_State_HG == "Churchville, Maryland"] <- "Harford"
# Cinnaminson, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Cinnaminson, New Jersey"] <- "Burlington"
# Clackamas, OR
hategroups_raw$County[hategroups_raw$City_State_HG == "Clackamas, Oregon"] <- "Clackamas"
# Clements, MD
hategroups_raw$County[hategroups_raw$City_State_HG == "Clements, Maryland"] <- "St. Mary's"
# Clinton, CT
hategroups_raw$County[hategroups_raw$City_State_HG == "Clinton, Connecticut"] <- "Middlesex"
# Clinton Township, MI
hategroups_raw$County[hategroups_raw$City_State_HG == "Clinton Township, Michigan"] <- "Macomb"
# Cloverdale, MI
hategroups_raw$County[hategroups_raw$City_State_HG == "Cloverdale, Michigan"] <- "Barry"
# Colebrook, CT
hategroups_raw$County[hategroups_raw$City_State_HG == "Colebrook, Connecticut"] <- "Litchfield"
# Compton, AR
hategroups_raw$County[hategroups_raw$City_State_HG == "Compton, Arkansas"] <- "Newton"
# Conowingo, MD
hategroups_raw$County[hategroups_raw$City_State_HG == "Conowingo, Maryland"] <- "Cecil"
# Constable, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Constable, New York"] <- "Franklin"
# Cromwell, CT
hategroups_raw$County[hategroups_raw$City_State_HG == "Cromwell, Connecticut"] <- "Middlesex"
# Cromwell, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Crum Lynne, Pennsylvania"] <- "Ridley"
# Cunningham, TN
hategroups_raw$County[hategroups_raw$City_State_HG == "Cunningham, Tennessee"] <- "Montgomery"
# Danbury, NH
hategroups_raw$County[hategroups_raw$City_State_HG == "Danbury, New Hampshire"] <- "Merrimack"
# Danvers, MA
hategroups_raw$County[hategroups_raw$City_State_HG == "Danvers, Massachusetts"] <- "Essex"
# Danville, AL
hategroups_raw$County[hategroups_raw$City_State_HG == "Danville, Alabama"] <- "Morgan"
# Delaplane, VA
hategroups_raw$County[hategroups_raw$City_State_HG == "Delaplane, Virginia"] <- "Fauquier"
# Dorchester, MA
hategroups_raw$County[hategroups_raw$City_State_HG == "Dorchester, Massachusetts"] <- "Suffolk"
# Drums, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Drums, Pennsylvania"] <- "Butler"
# Dublin, NH
hategroups_raw$County[hategroups_raw$City_State_HG == "Dublin, New Hampshire"] <- "Cheshire"
# Dublin, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Dublin, New Hampshire"] <- "Cheshire"
# Dulzura, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "Dulzura, California"] <- "San Diego"
# Earlville, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Earlville, Pennsylvania"] <- "Berks"
# East Brunswick, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "East Brunswick, New Jersey"] <- "Middlesex"
# East Haven, CT
hategroups_raw$County[hategroups_raw$City_State_HG == "East Haven, Connecticut"] <- "New Haven"
# East Windsor, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "East Windsor, New Jersey"] <- "Mercer"
# Eckert, CO
hategroups_raw$County[hategroups_raw$City_State_HG == "Eckert, Colorado"] <- "Delta"
# El Prado, NM
hategroups_raw$County[hategroups_raw$City_State_HG == "El Prado, New Mexico"] <- "Taos"
# Elm, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Elm, Pennsylvania"] <- "Lancaster"
# Encino, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "Encino, California"] <- "Los Angeles"
# Exeter, NH
hategroups_raw$County[hategroups_raw$City_State_HG == "Exeter, New Hampshire"] <- "Rockingham"
# Fairdale, KY
hategroups_raw$County[hategroups_raw$City_State_HG == "Fairdale, Kentucky"] <- "Jefferson"
# Fairfield, CT
hategroups_raw$County[hategroups_raw$City_State_HG == "Fairfield, Connecticut"] <- "Fairfield"
# Finksburg, MD
hategroups_raw$County[hategroups_raw$City_State_HG == "Finksburg, Maryland"] <- "Carroll"
# Fisher, WV
hategroups_raw$County[hategroups_raw$City_State_HG == "Fisher, West Virginia"] <- "Hardy"
# Fishkill, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Fishkill, New York"] <- "Dutchess"
# Flint, TX
hategroups_raw$County[hategroups_raw$City_State_HG == "Flint, Texas"] <- "Smith"
# Fresh Meadows, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Fresh Meadows, New York"] <- "Queens"
# Fruit Hill, KY
hategroups_raw$County[hategroups_raw$City_State_HG == "Fruit Hill, Kentucky"] <- "Christian"
# Galloway, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Galloway, New Jersey"] <- "Atlantic"
# Gilford, NH
hategroups_raw$County[hategroups_raw$City_State_HG == "Gilford, New Hampshire"] <- "Belknap"
# Gladwyne, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Gladwyne, Pennsylvania"] <- "Montgomery"
# Glenelg, MD
hategroups_raw$County[hategroups_raw$City_State_HG == "Glenelg, Maryland"] <- "Howard"
# Glens Fork, KY
hategroups_raw$County[hategroups_raw$City_State_HG == "Glens Fork, Kentucky"] <- "Adair"
# Gregory, MI
hategroups_raw$County[hategroups_raw$City_State_HG == "Gregory, Michigan"] <- "Livingston"
# Haddon Township, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Haddon Township, New Jersey"] <- "Camden"
# Hampton Township, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Hampton Township, Pennsylvania"] <- "Allegheny"
# Hamden, CT
hategroups_raw$County[hategroups_raw$City_State_HG == "Hamden, Connecticut"] <- "New Haven"
# Hamilton Township, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Hamilton Township, New Jersey"] <- "Atlantic"
# Hardwick, VT
hategroups_raw$County[hategroups_raw$City_State_HG == "Hardwick, Vermont"] <- "Caledonia"
# Hardy, VA
hategroups_raw$County[hategroups_raw$City_State_HG == "Hardy, Virginia"] <- "Bedford"
# Harlem, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Harlem, New York"] <- "Manhattan"
# Harwich, MA
hategroups_raw$County[hategroups_raw$City_State_HG == "Harwich, Massachusetts"] <- "Barnstable"
# Hempstead, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Hempstead, New York"] <- "Nassau"
# Henrico, VA
hategroups_raw$County[hategroups_raw$City_State_HG == "Henrico, Virginia"] <- "Henrico"
# Highland Township, MI
hategroups_raw$County[hategroups_raw$City_State_HG == "Highland Township, Michigan"] <- "Oakland"
# Higley, AZ
hategroups_raw$County[hategroups_raw$City_State_HG == "Higley, Arizona"] <- "Maricopa"
# Hillsborough, NH
hategroups_raw$County[hategroups_raw$City_State_HG == "Hillsborough, New Hampshire"] <- "Hillsborough"
# Purgitsville, WV
hategroups_raw$County[hategroups_raw$City_State_HG == "Purgitsville, West Virginia"] <- "Hampshire"
# Laurel Bloomery
hategroups_raw$County[hategroups_raw$City_State_HG == "Laurel Bloomery, Tennessee"] <- "Johnson"
# Baltimore
hategroups_raw$County[hategroups_raw$City_State_HG == "Curtis Bay, Maryland"] <- "Baltimore"
# Hixson, TN 🕇
hategroups_raw$County[hategroups_raw$City_State_HG == "Hixson, Tennessee"] <- "Hamilton"
# Holland, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Holland, New Jersey"] <- "Hunderton"
# Hollis, NH
hategroups_raw$County[hategroups_raw$City_State_HG == "Hollis, New Hampshire"] <- "Hillsborough"
# Hopkinton, NH
hategroups_raw$County[hategroups_raw$City_State_HG == "Hopkinton, New Hampshire"] <- "Merrimack"
# Hopkinton, MA
hategroups_raw$County[hategroups_raw$City_State_HG == "Hopkinton, Massachusetts"] <- "Middlesex"
# Howell, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Howell, New Jersey"] <- "Monmouth"
# Hualapai, AZ
hategroups_raw$County[hategroups_raw$City_State_HG == "Hualapai, Arizona"] <- "Mojave"
# Hudson, NH
hategroups_raw$County[hategroups_raw$City_State_HG == "Hudson, New Hampshire"] <- "Hillsborough"
# Huffman, TX
hategroups_raw$County[hategroups_raw$City_State_HG == "Huffman, Texas"] <- "Harris"
# Huntington Mills, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Huntington Mills, Pennsylvania"] <- "Huntington"
# Hyde Park, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Hyde Park, New York"] <- "Dutchess"
# Ickesburg, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Ickesburg, Pennsylvania"] <- "Perry"
# Inland Empire, CA (???)
hategroups_raw$County[hategroups_raw$City_State_HG == "Inland Empire, California"] <- "San Bernardino"
# Irapuato, AZ (nonexistent)
hategroups_raw$County[hategroups_raw$City_State_HG == "Irapuato, Arizona"] <- "Maricopa"
# Jackman, ME
hategroups_raw$County[hategroups_raw$City_State_HG == "Jackman, Maine"] <- "Somerset"
# Jackson, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Jackson, New Jersey"] <- "Ocean"
# Jackson, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Jackson Heights, New York"] <- "Queens"
# Jamison, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Jamison, Pennsylvania"] <- "Bucks"
# Kailua-Kona, HI
hategroups_raw$County[hategroups_raw$City_State_HG == "Kailua Kona, Hawaii"] <- "Hawaii"
# Kanawha Valley, VA
hategroups_raw$County[hategroups_raw$City_State_HG == "Kanawha Valley, West Virginia"] <- "Kanawha"
# Kaweah, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "Kaweah, California"] <- "Tulare"
# Kodak, TN
hategroups_raw$County[hategroups_raw$City_State_HG == "Kodak, Tennessee"] <- "Sevierville"
# Lackawanna, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "La Plume Township, Pennsylvania"] <- "Lackawanna"
# Lafayette, TN
hategroups_raw$County[hategroups_raw$City_State_HG == "Lafayette, Tennessee"] <- "Macon"
# Lake George, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Lake George, New York"] <- "Warren"
# Lake Worth, FL
hategroups_raw$County[hategroups_raw$City_State_HG == "Lake Worth, Florida"] <- "Palm Beach County"
# Dandrige, TN
hategroups_raw$County[hategroups_raw$City_State_HG == "Dandridge, Tennessee"] <- "Jefferson"
# Fort McCoy, FL
hategroups_raw$County[hategroups_raw$City_State_HG == "Fort McCoy, Florida"] <- "Marion"
# Hope, RI
hategroups_raw$County[hategroups_raw$City_State_HG == "Hope, Rhode Island"] <- "Providence"
# Lavale, MA
hategroups_raw$County[hategroups_raw$City_State_HG == "Lavale, Maryland"] <- "Allegany"
# Leasburg, PA 🕇
hategroups_raw$County[hategroups_raw$City_State_HG == "Leasburg, North Carolina"] <- "Caswell"
# Lehigh Valley, PA 
hategroups_raw$County[hategroups_raw$City_State_HG == "Lehigh Valley, Pennsylvania"] <- "Lehigh"
# Linwood, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Linwood, New Jersey"] <- "Atlantic"
# Linwood, MI
hategroups_raw$County[hategroups_raw$City_State_HG == "Linwood, Michigan"] <- "Bay County"
# Little Egg Harbor, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Little Egg Harbor, New Jersey"] <- "Ocean"
# Livingston, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Livingston, New Jersey"] <- "Essex"
# Machias, ME
hategroups_raw$County[hategroups_raw$City_State_HG == "Machias, Maine"] <- "Washington"
# Maidsville, WV
hategroups_raw$County[hategroups_raw$City_State_HG == "Maidsville, West Virginia"] <- "Monongalia"
# Manchester Township, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Manchester Township, New Jersey"] <- "Ocean"
# Mapaville, MO
hategroups_raw$County[hategroups_raw$City_State_HG == "Mapaville, Missouri"] <- "Jefferson"
# Maple Shade, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Maple Shade, New Jersey"] <- "Burlington"
# Matinicus Isle, ME
hategroups_raw$County[hategroups_raw$City_State_HG == "Matinicus Isle, Maine"] <- "Knox"
# Mecklenburg, NC
hategroups_raw$County[hategroups_raw$City_State_HG == "Mecklenburg, North Carolina"] <- "Mecklenburg"
# North Bergen, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "North Bergen, New Jersey"] <- "Hudson"
# Ona, WV
hategroups_raw$County[hategroups_raw$City_State_HG == "Ona, West Virginia"] <- "Cabell"
# Old Bridge, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Old Bridge, New Jersey"] <- "Middlesex"
# Pride, LA
hategroups_raw$County[hategroups_raw$City_State_HG == "Pride, Louisiana"] <- "East Baton Rouge"
# Oaks, PA
hategroups_raw$County[hategroups_raw$City_State_HG == "Oaks, Pennsylvania"] <- "Montgomery"
# Nottingham, MD
hategroups_raw$County[hategroups_raw$City_State_HG == "Nottingham, Maryland"] <- "Baltimore"
# Norton, VE
hategroups_raw$County[hategroups_raw$City_State_HG == "Norton, Vermont"] <- "Essex"
# Massena, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Massena, New York"] <- "St. Lawrence"
# Melrose, FL
hategroups_raw$County[hategroups_raw$City_State_HG == "Melrose, Florida"] <- "Putnam"
# Mid-Ohio Valley, WV
hategroups_raw$County[hategroups_raw$City_State_HG == "Mid Ohio Valley, West Virginia"] <- "Wood"
# Middlebury, CT
hategroups_raw$County[hategroups_raw$City_State_HG == "Middlebury, Connecticut"] <- "New Haven"
# Middlebury, VT
hategroups_raw$County[hategroups_raw$City_State_HG == "Middlebury, Vermont"] <- "Addison"
# Milan, NH
hategroups_raw$County[hategroups_raw$City_State_HG == "Milan, New Hampshire"] <- "Coös"
# Mill Spring, NC
hategroups_raw$County[hategroups_raw$City_State_HG == "Mill Spring, North Carolina"] <- "Polk"
# Mishiwaka, IN
hategroups_raw$County[hategroups_raw$City_State_HG == "Mishiwaka, Indiana"] <- "St. Joseph"
# Monkton, MD
hategroups_raw$County[hategroups_raw$City_State_HG == "Monkton, Maryland"] <- "Baltimore"
# Monroeville, NJ
hategroups_raw$County[hategroups_raw$City_State_HG == "Monroeville, New Jersey"] <- "Gloucester"
# Niagara Falls, New York
hategroups_raw$County[hategroups_raw$City_State_HG == "Niagara Falls, New York"] <- "Niagara"
# North Hollywood, CA
hategroups_raw$County[hategroups_raw$City_State_HG == "North Hollywood, California"] <- "Los Angeles"
# Norway, MA
hategroups_raw$County[hategroups_raw$City_State_HG == "Norway, Maine"] <- "Oxford"
# Rio Grande Valley
hategroups_raw$County[hategroups_raw$City_State_HG == "Rio Grande Valley, Texas"] <- "Hidalgo"

## 2.7 Run loop again

## Updates CSHG column
hategroups_raw$City_State_HG <- paste(hategroups_raw$City, hategroups_raw$State, sep = ", ")

# Loop through each row in hategroups_raw and find matches in geolocation_raw2
for (i in 1:nrow(hategroups_raw)) {
  # Get the City_State_HG for the current row in hategroups_raw
  city_state_hg <- hategroups_raw$City_State_HG[i]
  # Find the corresponding row index in geolocation_raw2 where City_stategr matches
  match_index <- which(geolocation_raw2$City_stategr == city_state_hg)
  # If there's a match, update County in hategroups_raw with Countyg from geolocation_raw2
  if (length(match_index) > 0) {
    hategroups_raw$County[i] <- geolocation_raw2$Countyg[match_index[1]]
  }
}

############################################################################################################
# 3. Splitting city names with multiple counties                                                           #
############################################################################################################

# 3.1 Counties

# Splitting "Wilkes-Barre/Scranton" into separate rows for "Wilkes-Barre" and "Scranton"
hategroups_raw <- hategroups_raw %>%
  mutate(City = if_else(City == "Wilkes-Barre/Scranton", "Wilkes-Barre,Scranton", City)) %>%
  separate_rows(City, sep = ",")

# Splitting "Seattle/Tacoma" into separate rows for "Seattle" and "Tacoma"
hategroups_raw <- hategroups_raw %>%
  mutate(City = if_else(City == "Seattle/Tacoma", "Seattle,Tacoma", City)) %>%
  separate_rows(City, sep = ",")

# Splitting "Tampa Bay" into separate rows for "Pinellas", "Manatee" and "Hillsborough"
hategroups_raw <- hategroups_raw %>%
  mutate(City = if_else(City == "Tampa Bay", "Pinellas,Manatee,Hillsborough", City)) %>%
  separate_rows(City, sep = ",")

# Splitting "Minneapolis/St. Paul" into separate rows for "Minneapolis" and "St. Paul"
hategroups_raw <- hategroups_raw %>%
  mutate(City = if_else(City == "Minneapolis/St. Paul", "Minneapolis,St. Paul", City)) %>%
  separate_rows(City, sep = ",")

# Removing rows with old city names
hategroups_raw <- hategroups_raw %>%
  filter(!City %in% c("Wilkes-Barre/Scranton", "Seattle/Tacoma", "Tampa Bay", "Minneapolis/St. Paul"))

# Splitting "Wise/Denton counties" into separate rows for "Wise" and "Denton"
hategroups_raw <- hategroups_raw %>%
  mutate(City = if_else(City == "Wise/Denton counties", "Wise,Denton", City)) %>%
  separate_rows(City, sep = ",")

# Splitting "Willard/Sandusky" into separate rows for "Willard" and "Sandusky"
hategroups_raw <- hategroups_raw %>%
  mutate(City = if_else(City == "Willard/Sandusky", "Willard,Sandusky", City)) %>%
  separate_rows(City, sep = ",")

# Splitting "Tampa & (St. Petersburg)" into separate rows for "Tampa" and "St. Petersburg"
hategroups_raw <- hategroups_raw %>%
  mutate(City = if_else(City == "Tampa & (St. Petersburg)", "Tampa,St. Petersburg", City)) %>%
  separate_rows(City, sep = ",")

# Splitting "Spring/Magnolia" into separate rows for "Spring" and "Magnolia"
hategroups_raw <- hategroups_raw %>%
  mutate(City = if_else(City == "Spring/Magnolia", "Spring,Magnolia", City)) %>%
  separate_rows(City, sep = ",")

# Splitting "Knoxville/Chattanooga" into separate rows for "Knoxville" and "Chattanooga"
hategroups_raw <- hategroups_raw %>%
  mutate(City = if_else(City == "Knoxville/Chattanooga", "Knoxville,Chattanooga", City)) %>%
  separate_rows(City, sep = ",")

# Splitting "Long Island" into its four counties
hategroups_raw <- hategroups_raw %>%
  mutate(City = if_else(City == "Long Island", "Nassau,Suffolk,Queens,Kings", City)) %>%
  separate_rows(City, sep = ",")

# Splitting "Odessa/Midland" into separate rows for "Odessa" and "Midland"
hategroups_raw <- hategroups_raw %>%
  mutate(City = if_else(City == "Odessa/Midland", "Odessa,Midland", City)) %>%
  separate_rows(City, sep = ",")

# Removing rows with old city names
hategroups_raw <- hategroups_raw %>%
  filter(!City %in% c("Wise/Denton counties", "Willard/Sandusky", "Tampa & (St. Petersburg)", "Spring/Magnolia", "Knoxville/Chattanooga", "Long Island", "Odessa/Midland"))

# Reset row names (super necessary)
rownames(hategroups_raw) <- NULL

## loop again:

## Update CSHG column
hategroups_raw$City_State_HG <- paste(hategroups_raw$City, hategroups_raw$State, sep = ", ")

#### Loop through each row in hategroups_raw and find matches in geolocation_raw2
for (i in 1:nrow(hategroups_raw)) {
  # Get the City_State_HG for the current row in hategroups_raw
  city_state_hg <- hategroups_raw$City_State_HG[i]
  
  # Find the corresponding row index in geolocation_raw2 where City_stategr matches
  match_index <- which(geolocation_raw2$City_stategr == city_state_hg)
  
  # If there's a match, update County in hategroups_raw with Countyg from geolocation_raw2
  if (length(match_index) > 0) {
    hategroups_raw$County[i] <- geolocation_raw2$Countyg[match_index[1]]
  }
}

### 3.2 Update the split counties

# Suffolk, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Suffolk, New York"] <- "Suffolk"
# Nassau, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Nassau, New York"] <- "Nassau"
# Kings, NY
hategroups_raw$County[hategroups_raw$City_State_HG == "Kings, New York"] <- "Kings"
# Wise, TX
hategroups_raw$County[hategroups_raw$City_State_HG == "Wise, Texas"] <- "Van Zandt"
# Hillsborough, FL
hategroups_raw$County[hategroups_raw$City_State_HG == "Hillsborough, Florida"] <- "Hillsborough"
# Manatee, FL
hategroups_raw$County[hategroups_raw$City_State_HG == "Manatee, Florida"] <- "Manatee"
# Pinellas, FL
hategroups_raw$County[hategroups_raw$City_State_HG == "Pinellas, Florida"] <- "Pinellas"

############################################################################################################
# 4. Final Steps                                                                                           #
############################################################################################################

# Run Update CSHG column loop again

hategroups_raw$City_State_HG <- paste(hategroups_raw$City, hategroups_raw$State, sep = ", ")

#### Loop through each row in hategroups_raw and find matches in geolocation_raw2
for (i in 1:nrow(hategroups_raw)) {
  # Get the City_State_HG for the current row in hategroups_raw
  city_state_hg <- hategroups_raw$City_State_HG[i]
  
  # Find the corresponding row index in geolocation_raw2 where City_stategr matches
  match_index <- which(geolocation_raw2$City_stategr == city_state_hg)
  
  # If there's a match, update County in hategroups_raw with Countyg from geolocation_raw2
  if (length(match_index) > 0) {
    hategroups_raw$County[i] <- geolocation_raw2$Countyg[match_index[1]]
  }
}

# View the updated dataframe
View(hategroups_raw)

############################################################################################################
# NOTES                                                                                                    #
############################################################################################################

# Check Waldoboro
# Check Richmond
# Check NYC
# Check Mccook, NA in true raw