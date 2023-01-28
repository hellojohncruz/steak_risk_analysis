---
title: "How Americans Like Their Steak"
author: "John Cruz"
date: "2023-01-27"
output:
  pdf_document: default
  html_document: default
---



## Overview

Walt Hickey from FiveThirtyEight collected data from people within the United States to see if a risk-averse person would be more likely to order a steak well done. They found no evidence a person that was a higher risk taker would prefer their steaks rare. 

['FiveThirtyEight Article']('https://fivethirtyeight.com/features/how-americans-like-their-steak/')

['Data Source']('https://raw.githubusercontent.com/fivethirtyeight/data/master/steak-survey/steak-risk-survey.csv')

## Required libraries

```r
library(pander)
library(tidyverse)
library(pollster)
```

## Data Dictionary

The original data set columns included the full questions asked. They will be renamed with keywords for ease:


| Keyword  | Original Column   |
|:--------|:-----------------------------|
| id      | RespondentID |
| lottery_pick | Consider the following hypothetical situations: <br> &emsp;&emsp;In **Lottery A**, you have a 50% chance of success, with a payout of $100. <br> &emsp;&emsp;In **Lottery B**, you have a 90% chance of success, with a payout of $20. <br> Assuming you have $10 to bet, would you play Lottery A or Lottery B?      |
| smoker | Do you ever smoke cigarettes?      |
| alcohol | Do you ever drink alcohol?      |
| gamble | Do you ever gamble?     |
| skydiving | Have you ever been skydiving?      |
| speeding | Do you ever drive above the speed limit?
| cheated | Have you ever cheated on your significant other?      |
| eat_steak | Do you eat steak?      |
| doneness | How do you like your steak prepared?      |
| gender | Gender      |
| age_group | Age      |
| income_level | Household Income      |
| education_level | Education      |
| us_region | Location (Census Region)      |

## Data Preparation
Import data from GitHub and rename columns


```r
new_colnames <- c('id', 'lottery_pick', 'smoker', 'alcohol', 'gamble', 'skydiving', 'speeding', 'cheated', 'eat_steak', 'doneness', 'gender', 'age_group', 'income_level', 'education_level', 'us_region')

steak_survey <- read.csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/steak-survey/steak-risk-survey.csv', col.names = new_colnames)
```

View first few rows within the data frame.


```r
knitr::kable(head(steak_survey, 5))
```



|         id|lottery_pick |smoker   |alcohol  |gamble   |skydiving |speeding |cheated  |eat_steak |doneness    |gender   |age_group |income_level      |education_level                  |us_region          |
|----------:|:------------|:--------|:--------|:--------|:---------|:--------|:--------|:---------|:-----------|:--------|:---------|:-----------------|:--------------------------------|:------------------|
|         NA|Response     |Response |Response |Response |Response  |Response |Response |Response  |Response    |Response |Response  |Response          |Response                         |Response           |
| 3237565956|Lottery B    |         |         |         |          |         |         |          |            |         |          |                  |                                 |                   |
| 3234982343|Lottery A    |No       |Yes      |No       |No        |No       |No       |Yes       |Medium rare |Male     |> 60      |$50,000 - $99,999 |Some college or Associate degree |East North Central |
| 3234973379|Lottery A    |No       |Yes      |Yes      |No        |Yes      |Yes      |Yes       |Rare        |Male     |> 60      |$150,000+         |Graduate degree                  |South Atlantic     |
| 3234972383|Lottery B    |Yes      |Yes      |Yes      |No        |Yes      |Yes      |Yes       |Medium      |Male     |> 60      |$50,000 - $99,999 |Bachelor degree                  |New England        |

Remove data that is explicitly NA from the *id* column and people who do not eat steak. Subset data to focus on risks and their region. 


```r
steak_survey <- steak_survey %>% 
  drop_na(id)
```


```r
subset_cols <- c('smoker', 'alcohol', 'gamble', 'skydiving', 'speeding', 'cheated', 'doneness', 'us_region')
risks <- steak_survey %>% 
  select(all_of(subset_cols))
```


```r
non_null_risks <- risks %>% 
  filter(across(all_of(subset_cols), ~ .x != ''))

knitr::kable(head(non_null_risks, 5))
```



|smoker |alcohol |gamble |skydiving |speeding |cheated |doneness    |us_region          |
|:------|:-------|:------|:---------|:--------|:-------|:-----------|:------------------|
|No     |Yes     |No     |No        |No       |No      |Medium rare |East North Central |
|No     |Yes     |Yes    |No        |Yes      |Yes     |Rare        |South Atlantic     |
|Yes    |Yes     |Yes    |No        |Yes      |Yes     |Medium      |New England        |
|No     |Yes     |No     |No        |Yes      |Yes     |Medium      |Middle Atlantic    |
|No     |No      |No     |No        |Yes      |No      |Medium rare |West South Central |

## Findings
We can see that most individuals prefer Medium and Medium Rare cooking temperatures for their steaks. 


```r
ggplot(data = non_null_risks) +
  geom_bar(aes(doneness))
```

![](how_americans_like_their_steak_files/figure-latex/barplot-1.pdf)<!-- --> 

Calculate total risks taken by each individual.


```r
non_null_risks[non_null_risks=='Yes'] <- 1
non_null_risks[non_null_risks=='No'] <- 0

change_type <- c('smoker', 'alcohol', 'gamble', 'skydiving', 'speeding', 'cheated')

non_null_risks <- non_null_risks %>% 
  mutate_at(change_type, as.integer)

non_null_risks$doneness <- factor(non_null_risks$doneness, levels = c('Rare', 'Medium rare', 'Medium', 'Medium Well', 'Well'))

non_null_risks <- non_null_risks %>% 
  mutate(total_risks = smoker + alcohol + gamble + skydiving + speeding + cheated)
```

Using a crosstab view, we can determine how people like their steak within each region 


```r
knitr::kable(crosstab(non_null_risks, x = us_region, y = doneness, weight = total_risks))
```



|us_region          |      Rare| Medium rare|   Medium| Medium Well|      Well|   n|
|:------------------|---------:|-----------:|--------:|-----------:|---------:|---:|
|East North Central |  0.000000|    40.36145| 33.73494|   15.662651| 10.240964| 166|
|East South Central |  0.000000|    54.34783| 23.91304|    6.521739| 15.217391|  46|
|Middle Atlantic    | 11.510791|    20.14388| 36.69065|   22.302158|  9.352518| 139|
|Mountain           |  0.000000|    28.20513| 39.74359|   17.948718| 14.102564|  78|
|New England        |  5.333333|    44.00000| 34.66667|   12.000000|  4.000000|  75|
|Pacific            |  8.510638|    34.57447| 38.82979|   10.106383|  7.978723| 188|
|South Atlantic     |  6.666667|    38.46154| 26.66667|   23.076923|  5.128205| 195|
|West North Central | 11.578947|    46.31579| 29.47368|   12.631579|  0.000000|  95|
|West South Central |  0.000000|    51.66667| 26.66667|   13.333333|  8.333333|  60|




```r
grouped <- crosstab(non_null_risks, x = us_region, y = doneness, weight = total_risks, format = 'long')

grouped %>% 
  ggplot(aes(x = pct, y = us_region, fill = doneness)) +
  geom_bar(stat = 'identity', position = position_fill(reverse = TRUE)) +
  xlab("Total Region %") +
  ylab(element_blank()) +
  geom_text(aes(label = paste0(round(pct, 0), '%')), position = position_fill(reverse = TRUE, vjust = 0.5), size = 2.5, color = 'black', fontface = 'bold') +
  scale_x_continuous(labels = scales::percent) +
 theme(legend.position = "top", legend.title = element_blank()) +
  labs(title = 'Steak Doneness by US Region') +
  scale_y_discrete(limits=rev) +
  scale_fill_brewer(palette="RdBu")
```

![](how_americans_like_their_steak_files/figure-latex/grouped-us_region-doneness-1.pdf)<!-- --> 

Surprisingly, not every region is represented by people who prefer their steaks rare. 

## Recommendations
Given how the survey was taken, more information would be beneficial to help determine riskiness. Each question only offered the options of Yes/No, however, the degree of risk may vary based on frequency. A person who drinks alcohol, may drink a few glasses per week, but another person may drink several per day. The survey is also at risk of self-selection bias and does not account for random sampling. Lastly, are certain race/religions more prone to how they like to eat steak? 
