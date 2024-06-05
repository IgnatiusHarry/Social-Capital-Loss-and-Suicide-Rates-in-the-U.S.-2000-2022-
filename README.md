## Big Data Project 3 (on progress)


# Suicide & Social Capital in the United States Counties (2000 - 2022) 
#### **Number of observations: 71,392**
________________

#### **National Chengchi University**

Contributors

Mario Haya - 羅俊杰 (🇺🇸)

Marcella Alifia Kuswana Putri - 潘怡璇 (🇮🇩)

Ignatius Harry Cahiadharma - 柯昱安 (🇮🇩) - Main Programmer

Xóchitl Gutiérrez - 顧修齊 (🇺🇸)

### Project Description 
This study aims to investigate the relationship between suicide rates and various proxies of 'Social Capital loss' in the United States from 2000 to 2022. The breakdown of society in the West, marked by rising rates of racism, hate speech, and discrimination, has been accompanied by an increase in suicide rates. 

Given the limited research on this topic, this study seeks to elucidate the correlation and the causal effect between suicide rates and the increase in hate groups and hate crimes, among other factors.


### Research Question

What is the relationship between suicide and ‘social capital loss’ in the United States (2000-2022)?

H0: There is a strong relation between suicide and social capital loss.

H1: There is no strong relation between suicide and social capital loss.

## Definition of 'Social Capital'
Sociology defines 'Social Capital' as the pillars of a "functioning and stable society":
- Employment
- Absence of Crime
- Political Participation
- Economic Equality
- Access to Education

  
## Data

The dataset used in this study consists of various sources to measure the impact of social capital loss on suicide rates. The data includes information on hate groups, hate crimes, unemployment rates, poverty rates, and religious affiliation, alongside suicide rates per 100,000 individuals.

## Data Sources

- Hate Groups: Southern Poverty Law Center (SPLC), 2022
- Hate Crimes: Federal Bureau of Investigation (FBI), 2022
- Unemployment: Bureau of Labor Statistics (BLS), 2022
- Suicide: Centers for Disease Control and Prevention (CDC), 2022
- Other variables

Independent Variables:
- Hate Groups
- Hate Crimes
- Unemployment Rate
- Poverty Rate
- Religious Affiliation
- Etc.
  
Dependent Variable:
- Suicides per 100,000 individuals

**Number of observations: 71,392**

## File Structure

- Data File: The cleaned data for the study is available in the file Suicide_Social_Capital_Data.xlsx.
Code Files:
- Map and Spatial Analysis: Contains all code for U.S. map and including ggplot. This code is located in the folder Map_and_Spatial_Analysis.R.
- Panel Regression (Spatial and Fixed Effect): Contains code for causality or regression analysis and is located in the file Panel_Regression_Spatial_and_Fixed_Effect.R.

To ensure the code runs without error, it is recommended to clone the file structure to your local system.

## Methodology
- This study employs fixed-effects estimation of social capital loss on suicide rates across the United States from 2000 to 2022.
- This study also plots all of the suicide rates combined with the number of hate groups in each U.S. country

**Analytical Techniques**
- Panel Data Fixed Effect Regression Analysis: Useful for time-series analyses with large sample sizes and controlling characteristics that do not change drastically over time.
- Pearson Correlation: Used to measure the strength and direction of a linear relationship between two variables.

## Results (ongoing)
- The analysis found a significant correlation between rising suicide rates and the increase in hate groups in the United States from 2000 to 2022.
- we also will analyze the causal effect of hate groups on suicide rates.
## Conclusion (ongoing)
The study will provide valuable insights into the relationship between social capital loss and suicide rates in the United States. The findings will highlight the importance of addressing the social factors contributing to suicide and informing policymakers on the need to tackle hate crimes and support community cohesion.

## References
Centers for Disease Control and Prevention (n.d.). Underlying Cause of Death, 1999-2020 Results. Retrieved April 1, 2022, from CDC Wonder

Paxton, P. (1999). Is social capital declining in the United States? A multiple indicator assessment. American Journal of Sociology, 105(1), 88-127.

SPLC. (2021). Hate Map. Retrieved August 8, 2020, from SPLC Hate Map

FBI. (2021). Uniform Crime Reporting: Hate Crime Statistics. Retrieved March 10, 2022, from FBI UCR Hate Crime Statistics
