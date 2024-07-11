## Big Data Project


# Exploring the Relationship Between Proxies of Social Capital Loss and Suicide In the United States (2000-2022)
#### **Number of observations: 71,392**
________________

#### **National Chengchi University**

Contributors

Mario Haya - 羅俊杰 (🇺🇸)

Marcella Alifia Kuswana Putri - 潘怡璇 (🇮🇩)

Ignatius Harry Cahiadharma - 柯昱安 (🇮🇩) 

Xóchitl Gutiérrez - 顧修齊 (🇺🇸)

### Project Description 
This study aims to investigate the relationship between suicide rates and various proxies of 'Social Capital loss' in the United States from 2000 to 2022. The breakdown of society in the West, marked by rising rates of racism, hate speech, and discrimination, has been accompanied by an increase in suicide rates. 

Given the limited research on this topic, this study seeks to elucidate the correlation and the causal effect between suicide rates and the increase in hate groups and hate crimes, among other factors.


### Research Question

What is the relationship between suicide and ‘social capital loss’ in the United States (2000-2022)?

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
- Median Income
- Etc.
  
Dependent Variable:
- Suicides per 100,000 individuals

**Number of observations: 71,392**

## File Structure
### Data File:
- The cleaned and merged data, ready to analysis for the study, is available in the file "**NEW_merged_data.xlsx**".
- The raw data for each variable can be checked and accessed in the folder of "**raw data**"

### R Code Files:
We divided it into 3 parts of R code to make it cleaner and easier to run; you can check in the folder "**R file**".
1. DATA CLEANING      : This file contains all code for cleaning in general. This code is located in the file "**#1 - DATA CLEANING.R**."
2. DATA VISUALIZATION : This file contains all the code for Map and Spatial Analysis (U.S. map and including ggplot, scatter plot, etc). It is located in the file "**#2 - DATA VISUALIZATION.R**".
3. EMPIRICAL METHOD   : This file contains all the code for Pearson Correlation, OLS , and Panel Data Regression (Fixed Effect). It contains code for causality or regression analysis and is in the file "#**3 - EMPIRICAL METHOD.R**".

To ensure the code runs without error, it is recommended to clone the file structure to your local system.

## Methodology
- This study employs Pearson Correlation, OLS regression, and fixed-effects (with and without an intervention variable) estimation of social capital loss on suicide rates across the United States from 2000 to 2022.
- This study also plots all of the suicide rates combined with the number of hate groups in each U.S. country

**Analytical Techniques**
- OLS
- Panel Data Fixed Effect Regression Analysis: Useful for time-series analyses with large sample sizes and controlling characteristics that do not change drastically over time.
- Pearson Correlation: Used to measure the strength and direction of a linear relationship between two variables.

## Result, Conclusion and Future Research
This paper intends to explore the relationship between five social capital loss proxies and suicide across the United States at a county level. Putnam defines social capital as “features of social organization such as networks, norms, and social trust that facilitate coordination and cooperation for mutual benefit” (Putnam, 2000). Social capital is founded on individual actors and their relationships and the social structures in which they are embedded. These associated norms are generally beneficial to those inside the network, but the external effects of social capital are not always positive. Social capital can be used for malevolent purposes such as the formation of hate groups, intolerant to change and differences, becoming less connected to society. Thus, hate groups become a proxy for social capital loss. Furthermore, level of unemployment, poverty, household income, and population size can also serve as proxies for social capital loss, as they can infer potential impacts on social trust, coordination and cooperation. 

The analysis across multiple regression models provides varying insights into the factors influencing suicide rates. The Ordinary Least Squares model (OLS) model indicates a positive correlation in the increase of hate groups with higher suicide rates, suggesting counties with a higher prevalence of hate groups experience higher suicide rates. Similarly, counties with greater income levels, higher poverty rates, and larger populations also indicate a  positive correlation with elevated suicide rates. In contrast however, the OLS model indicates a negative correlation between unemployment and suicide rates, suggesting higher unemployment rates are linked to lower suicide rates. This model explains about 48 percent (R-squared = 0.4786) of the variation in suicide rates and a highly significant F-statistic (F = 7971, p < 0.01). 

The Panel Fixed Effect Regression Model (PLM) model without interaction terms indicates hate groups do not exhibit statistical significance in relation to suicide rates. On the other hand, both household income and poverty rate show significant correlation with suicide rates, which remains consisting with the previous findings in the OLS model.  Moreover, when incorporating lagged variables and interaction terms in the Panel Fixed Effect Regression Model (PLM) model, the results indicate that an increase in the lagged poverty rate is positively associated with suicide rates, suggesting economic suffering contributes to elevated or increased suicide risks. Both  household income and population also show a significant correlation with suicide rates. 

However, contrary to general belief, this model implies counties with a larger historical count of hate groups and higher unemployment rate have lower suicide rates. Despite these significant findings, the model explains a relatively small portion of the variance in suicide rates, suggesting that other unexamined factors may also play a crucial role. Overall, the findings suggest economic conditions, population size, and presence of hate groups play a critical role in influencing suicide rates. The OLS model provides a more general view with higher explanatory power, whereas the PLM models elucidates nuanced and temporal dimensions of these relationships. 

Future research could attempt to explore the relationship between social capital loss and suicide in Europe, given its social and economic similarities to the United States. However, this remains challenging due to the inconsistent and scarce data on proxies such as hate group formation, as most available data focuses on hate speech. The European Statistical Office (Eurostat) provides ample resources on statistics related to suicide, unemployment, crime, immigration and ethnic groups, etc., but none pertaining specifically to hate group formation or hate crimes. 

A similar study can also be applied to Canada, given the growing number of hate groups (e.g. Blood and Honour, and Canadian Nationalist Front) and reported hate crimes,  alongside lack of proper law enforcement to protect civilians from hate crimes. A lengthy history of racism and white supremacy in Canada is often overlooked, with very little existing contemporary academic literature on right wing organizations. Therefore, this topic would contribute to the much needed research on hate groups and hate crimes in Canada. 

Lastly, the upcoming 2024 presidential election in the United States will once again feature Donald Trump and Joe Biden as the primary candidates. Building on our existing research, we propose conducting a study to investigate the correlation between the presence of hate groups and the percentage of voters that support Donald Trump. This analysis can illustrate whether there is a significant relationship between the presence of hate groups and political leanings across counties. Furthermore, it can help us understand to what extent this relationship potentially influences campaign strategies and policy decisions. 

Systemic racism permeates all institutions in the United States, contributing to the ongoing issue of social capital loss. Suicide rates in the country are among the highest in the world, accompanied by increasing levels of inflation and limited policies enacted to counteract these trends, making it challenging for citizens to remain afloat. Furthermore, hateful rhetoric from the country’s own politicians perpetuates the continuation of hate groups and discrimination. It is imperative that more research is conducted on the potential causes of suicide and social capital loss in the United States.

Note: For more detailed information and references, kindly check our paper uploaded on this GitHub. Moreover, we also created a poster for this research.



## References
Centers for Disease Control and Prevention (n.d.). Underlying Cause of Death, 1999-2020 Results. Retrieved April 1, 2022, from CDC Wonder

Paxton, P. (1999). Is social capital declining in the United States? A multiple indicator assessment. American Journal of Sociology, 105(1), 88-127.

SPLC. (2021). Hate Map. Retrieved August 8, 2020, from SPLC Hate Map

FBI. (2021). Uniform Crime Reporting: Hate Crime Statistics. Retrieved March 10, 2022, from FBI UCR Hate Crime Statistics
