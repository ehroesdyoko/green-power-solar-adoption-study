# Analysis of Customer Behavior in the Dutch Retail Energy Market

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

[cite_start]This repository contains the analysis of customer behavior for Green Power, a Dutch retail energy provider, with a focus on promoting solar panel adoption[cite: 2]. The project uses duration and count models in R to uncover insights into the key drivers behind:
* [cite_start]The timing of solar panel adoption. [cite: 7]
* [cite_start]The number of solar panels installed by customers. [cite: 21]
* [cite_start]The frequency of customer service interactions (calls and chatbots). [cite: 31, 32]
* [cite_start]The timing of follow-up contacts with customer support. [cite: 40]

The findings are used to derive actionable recommendations for Green Power to improve marketing strategies and customer support efficiency.

## Key Research Questions

This analysis addresses the following questions:
1.  [cite_start]What factors affect the **timing** of customers adopting solar panels? [cite: 62]
2.  [cite_start]What influences the **number of solar panels** a customer decides to install? [cite: 87]
3.  [cite_start]What drives the total number of **customer service calls and chatbot interactions**? [cite: 88]
4.  [cite_start]What variables influence the timing of a **second service contact** after an initial one? [cite: 63]

## Methodology

### Data Preparation
The initial dataset underwent several cleaning and preparation steps:
* [cite_start]Duplicate entries were resolved by grouping records and averaging the `av_bill` to create unique user entries[cite: 55, 56].
* [cite_start]Date-related columns were converted to the correct `Date` data type[cite: 1].
* [cite_start]Missing values in the `hh_size` variable were imputed using Multiple Imputation by Chained Equations (MICE)[cite: 1].
* [cite_start]Outliers in numerical variables were identified using boxplots, and structural issues like inactive service periods were noted[cite: 48, 52, 53].

### Modeling
The analysis employed two main types of statistical models:

* [cite_start]**Cox Proportional Hazards Model**: Used for duration analysis to understand the timing of events, such as solar panel adoption and the interval between service contacts[cite: 61, 65]. [cite_start]The proportional hazards assumption was tested, and models with time-varying covariates were considered where violations occurred[cite: 82, 112].
* [cite_start]**Poisson and Negative Binomial Regression**: Used for count data to model the number of installed solar panels and the frequency of service interactions[cite: 85, 89, 90]. [cite_start]The Negative Binomial model was chosen when overdispersion was detected in the Poisson models[cite: 102, 153, 173].

[cite_start]Model fit and selection were guided by AIC, BIC, and Likelihood Ratio (LR) tests[cite: 104]. [cite_start]Multicollinearity was assessed using Variance Inflation Factors (VIFs)[cite: 103].

## Key Findings

### 1. Solar Panel Adoption & System Size
* [cite_start]**Energy Costs and Household Size**: Higher average energy bills (`av_bill`) and larger household sizes (`hh_size`) are significant predictors of both faster solar panel adoption and a greater number of panels installed[cite: 137, 139, 156, 158].
* [cite_start]**Urban vs. Rural**: Urban households adopt solar panels significantly sooner than their rural counterparts[cite: 142].
* **Marketing Effectiveness**:
    * [cite_start]Personalized (`email_pers_solar`) and sustainability-focused (`email_sustainability`) emails significantly accelerate adoption[cite: 146].
    * [cite_start]However, general solar emails and energy-saving tips were associated with slower adoption and a smaller number of installed panels[cite: 147, 162, 163].
* [cite_start]**Customer Satisfaction**: Customer satisfaction did not show a significant effect on the timing of solar adoption[cite: 145].

### 2. Customer Service Interactions
* [cite_start]**Driver of Contact Frequency**: Negative experiences with prior service calls or chatbots (`neg_service`, `neg_chatbot`) are strong drivers of repeat contacts, supporting the idea of complaint-driven escalation[cite: 177]. [cite_start]Customer satisfaction was not a significant predictor for call volume[cite: 176].
* [cite_start]**Post-Adoption Support**: Customers who have installed solar panels are 82% more likely to make a follow-up phone call sooner, suggesting a need for post-installation support[cite: 200]. [cite_start]This effect was not significant for chatbot interactions[cite: 201].
* [cite_start]**First-Contact Resolution**: Unresolved issues, indicated by negative service experiences, lead to quicker follow-up interactions across both phone and chatbot channels[cite: 195, 196, 198].

## Repository Structure
