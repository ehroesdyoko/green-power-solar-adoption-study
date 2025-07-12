# Analysis of Customer Behavior in the Dutch Retail Energy Market

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# ☀️ Solar Panel Adoption & Customer Service Modeling – Green Power

This project analyzes solar panel adoption behavior and customer service interaction frequency for customers of a Dutch energy provider, **Green Power**. It was conducted as Assignment 2 for the Data Science Methods course in the MSc Marketing Analytics & Data Science program at the University of Groningen.

---

## 📌 Project Summary

We apply **survival analysis** and **count regression models** to understand:

1. ⏱️ **Timing of solar panel adoption** among long-term customers  
2. ☎️ **Customer service and chatbot interactions**  
3. 🌞 **Number of solar panels adopted**  

The study uses behavioral, demographic, and product-level data to test **12 hypotheses**.

---

## ⚙️ Methodology

### 📉 Duration Models
- **Cox Proportional Hazards Model**
- Analyzes time-to-adoption and follow-up contact events

### 🔢 Count Models
- **Poisson Regression**
- **Negative Binomial Regression**
- Models total service interactions and number of panels purchased

---

## 📄 Files

| File                          | Description                                      |
|-------------------------------|--------------------------------------------------|
| `Report.pdf`                  | Complete report with methodology and results     |
| `duration_and_count_models.R` | R script with Cox, Poisson, and NB models        |

---

## 📈 Highlights

- Duration models evaluate timing of events while accounting for censoring.
- Count models quantify impact of customer characteristics on service usage.
- Managerial recommendations for personalized follow-ups and service load planning.

---

## 📬 Contact

For questions, please raise an issue or contact the authors.

---

## 📄 License

This project is for academic use only.
