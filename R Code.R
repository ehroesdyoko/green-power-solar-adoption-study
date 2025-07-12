library(ggplot2)
library(MASS)
library(Hmisc)
library(AER)
library(shiny)
library(survival)
library(survminer)
library(mice)
library(corrplot)

setwd("/Users/erlanggaroesdyoko/Documents/MADS/4th Block/Customer Models/Assignment 2")
df <- read.csv("energy_data.csv")

summary(df)
head(df)

# Remove the X variable
df$X <- NULL

# See if there are duplicates in the data
df[c(1610, 1944), ]
df[c(660, 869, 1050, 1173), ]
df[c(1050, 1173), ]

# The number of rows per user_id is not always the same, so we cannot conclude that it is a quarterly or monthly bill or something
sum(df$user_id == "q2b4pAo_6tP7jNFKI5Hztw")
sum(df$user_id == "ewDNa_FRoG71UFm56eTGkw")
 
# Only the av_bill is different for these rows. The solution is: keep all other columns and average the av_bill
# Specify columns that define duplicates (i.e., all except av_bill)
group_cols <- setdiff(names(df), "av_bill")

# Aggregate by those columns and average av_bill
df <- df |>
  dplyr::group_by(across(all_of(group_cols))) |>
  dplyr::summarise(av_bill = mean(av_bill, na.rm = TRUE), .groups = "drop")

# Change the date-related columns to Date data type
df$customer_since <- as.Date(df$customer_since)
df$first_service <- as.Date(df$first_service)
df$sec_service <- as.Date(df$sec_service)
df$last_service <- as.Date(df$last_service)
df$first_chatbot <- as.Date(df$first_chatbot)
df$second_chatbot <- as.Date(df$second_chatbot)
df$last_chatbot <- as.Date(df$last_chatbot)

# Explore Dataset
length(unique(df$user_id)) #1255

# Average Bill
ggplot(df, aes(x = av_bill)) +
  geom_histogram(binwidth = 250, fill = "skyblue", color = "white") +
  labs(title = "Distribution of Average Bill", x = "Average Bill (€)", y = "Count")

# Average Gas
ggplot(df, aes(x = av_gas)) +
  geom_histogram(binwidth = 250, fill = "skyblue", color = "white") +
  labs(title = "Distribution of Average Gas", x = "Average Gas", y = "Count")

# Average Elec 
ggplot(df, aes(x = av_elec)) +
  geom_histogram(binwidth = 250, fill = "skyblue", color = "white") +
  labs(title = "Distribution of Average Elec", x = "Average Elec", y = "Count")

# Average Service Length
ggplot(df, aes(x = av_service_length)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "white") +
  labs(title = "Distribution of Average Service Length", x = "Average Service Length", y = "Count")

# Household Size Distribution
ggplot(df, aes(x = as.factor(hh_size))) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribution of Household Size", x = "Household Size", y = "Count")

# Household Number of Solar Panels
ggplot(df, aes(x = as.factor(nr_solar_panels))) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribution of Number of Solar Panels", x = "Number of Solar Panels", y = "Count")

# outlier analysis of numeric variable ----
numeric_vars <- c(
  "av_gas", "av_elec", "av_bill", "hh_size", "av_service_length",
  "service_2009", "service_2010", "service_2011", "service_2012",
  "service_2013", "service_2014", "service_2015", "service_2016",
  "service_2017", "service_2018", "service_2019", "service_2020",
  "service_2021", "service_2022", "satisfaction", "pos_service", "neg_service",
  "nrchatbot", "av_chatbot_length", "chatbot_2009", "chatbot_2010",
  "chatbot_2011", "chatbot_2012", "chatbot_2013", "chatbot_2014",
  "chatbot_2015", "chatbot_2016", "chatbot_2017", "chatbot_2018", "chatbot_2019", "chatbot_2020", "chatbot_2021",
  "chatbot_2022", "pos_chatbot", "neg_chatbot", "email_pers_solar", "email_gen_solar", "email_newsletter", "email_coupon", "email_sustainability", 
  "email_information", "email_loyalty", "email_save"
)

# Function to create boxplots and print outliers
plot_outliers <- function(data, vars) {
  for (var in vars) {
    if (var %in% colnames(data) && is.numeric(data[[var]])) {
      boxplot(data[[var]], main = paste("Boxplot of", var), ylab = var)
      outliers <- boxplot.stats(data[[var]])$out
      if (length(outliers) > 0) {
        cat("Outliers in", var, ":\n")
        print(outliers)
      } else {
        cat("No outliers detected in", var, "\n")
      }
      cat("\n-------------------------\n")
    } else {
      warning(paste(var, "is not a numeric column in the dataframe."))
    }
  }
}

plot_outliers(df, numeric_vars)

check_outliers <- function(data, var) {
  Q1 <- quantile(data[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[var]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  upper_bound <- Q3 + 1.5 * IQR_val
  
  outlier_rows <- data[data[[var]] > upper_bound, ]
  return(outlier_rows)
}

# List of variables to inspect
outlier_vars <- c("av_gas", "av_bill", "av_service_length", "nrchatbot", "av_chatbot_length")

# View the outlier rows for each variable
outlier_list <- lapply(outlier_vars, function(var) {
  cat("Outliers for:", var, "\n")
  rows <- check_outliers(df, var)
  print(rows)
  cat("\n--------------------\n")
  return(rows)
})
names(outlier_list) <- outlier_vars

# av_gas and av_bill outliers are occuring for same observations
cor(df$av_gas, df$av_bill, use = "complete.obs", method = "pearson")
# very high correlation, one variable almost explains the other - need to pay attention for multicollinearity in modeling

# service 2009, and chatbot 2009-2012 are empty completely, lets remove to reduce dimensionality
df <- df[, !colnames(df) %in% c("service_2009", paste0("chatbot_", 2009:2012))]

# correlation plot as explorative tool
numeric_df <- df[, sapply(df, is.numeric)]
cor_matrix <- cor(numeric_df, use = "complete.obs", method = "pearson")
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.6, tl.col = "black", number.cex = 0.7, 
         addCoef.col = "black", diag = FALSE)

### Mice (multiple imputation method) to replace missings
# Set method vector
method <- make.method(df)
method["hh_size"] <- "polyreg" # Use proportional odds logistic regression
method[setdiff(names(df), "hh_size")] <- ""  # Don't impute other variables

# Set predictor matrix
full_mat <- make.predictorMatrix(df)
full_mat[,] <- 0  # Start with no predictors
full_mat["hh_size", c("av_gas", "av_elec", "av_bill")] <- 1  # Use av_gas, av_bill and av_elec to impute hh_size

# Run MICE
MiceImputedData <- mice(
  data = df,
  m = 1,
  maxit = 50,
  method = method,
  predictorMatrix = full_mat,
  seed = 500
)

# Extract completed data
completed_df <- complete(MiceImputedData, 1)

df <- completed_df

# Q2 - Model - Customer Adopting Solar Panels | CHECK FOR MULTICOLLINEARITY ----
# Choose your latest observation year — update as needed
latest_year <- 2022

df$hh_size <- factor(df$hh_size, levels = c("1", "2", "3", "4", "5", "> 5"), ordered = FALSE)


df$yr_to_adoption <- ifelse(
  df$solar_panels == 1,
  df$solar_panels_since - df$customer_since_yr,
  latest_year - df$customer_since_yr  # time to censoring
)

# Handle t = 0
sum(df$yr_to_adoption == 0, na.rm = TRUE) 
sum(df$yr_to_adoption == 0 & df$solar_panels == 1, na.rm = TRUE) # 69 customers are adopted within a year

# People adopting after 2 months are assigned as 0 years, but should actually be 1
df$yr_to_adoption[df$yr_to_adoption == 0 & df$solar_panels == 1] <- 1

solar_cox <- coxph(Surv(yr_to_adoption, solar_panels) ~ av_bill + hh_size + urban + satisfaction
                   + email_pers_solar + email_gen_solar + email_sustainability + email_save
, data = df)
summary(solar_cox)
vif(solar_cox)
ph_test <- cox.zph(solar_cox) # proportionality test
print(ph_test) # proportionality does not hold
plot(ph_test)

solar_cox_tv <- coxph(Surv(yr_to_adoption, solar_panels) ~ 
                         tt(av_bill) + tt(hh_size) + urban + satisfaction
                      + email_pers_solar + email_gen_solar + tt(email_sustainability) + email_save,
                      data = df)
summary(solar_cox_tv)
vif(solar_cox_tv)

null_solar <- coxph(Surv(yr_to_adoption, solar_panels) ~ 1
                    , data = df)

AIC(null_solar,solar_cox,solar_cox_tv)
BIC(null_solar,solar_cox,solar_cox_tv)

plot(ph_test, var = "email_sustainability", main = "Time-varying Effect: email_sustainability")
abline(h = 0, col = "red", lty = 2)

# Q3 - Model - Number of Solar Panels ----
# Data Preparation

# Model
# normal poisson
nr_poisson <-glm(nr_solar_panels ~ av_bill + hh_size + 
                       email_pers_solar + email_gen_solar +
                       email_sustainability +
                       email_loyalty + email_save, data = df, family = 'poisson')

summary(nr_poisson)
vif(nr_poisson)


dispersiontest(nr_poisson,trafo=2) # overdispersion is present

nr_negbin <- glm.nb(nr_solar_panels ~ av_bill + hh_size + 
                      email_pers_solar + email_gen_solar +
                      email_sustainability +
                      email_loyalty + email_save, data = df) 

summary(nr_negbin)
vif(nr_negbin)
exp(coef(nr_negbin))

# null model
nullmodel<-glm(nr_solar_panels~1, data=df ,family="poisson")

# LR test
anova(nullmodel, nr_negbin, test = 'Chisq')

# AIC and BIC
AIC(nullmodel,nr_poisson,nr_negbin)
BIC(nullmodel,nr_poisson,nr_negbin)

# Q4 - Model - Total number of Service calls ----
df$relationship_length <- 2022 - df$customer_since_yr
p_service <- glm(nrservice ~ satisfaction  + neg_service + neg_chatbot
                   + nr_solar_panels + solar_panels + email_pers_solar +
                   email_sustainability + email_loyalty + email_save + relationship_length,
                 family = poisson, data = df)
summary(p_service)
vif(p_service)

dispersiontest(p_service,trafo=2) # overdispersion

servicecalls_negbin <- glm.nb(nrservice ~ satisfaction  + neg_service + neg_chatbot
                              + nr_solar_panels + solar_panels + email_pers_solar +
                                email_sustainability + email_loyalty + email_save + relationship_length, data = df) 

summary(servicecalls_negbin)
vif(servicecalls_negbin)

# Number of Chatbot interactions
p_chatbot <- glm(nrchatbot ~  satisfaction  + neg_service + neg_chatbot
                 + nr_solar_panels + solar_panels + email_pers_solar +
                   email_sustainability + email_loyalty + email_save + relationship_length,
                    data = df, family = 'poisson')

summary(p_chatbot)
vif(p_chatbot)

dispersiontest(p_chatbot,trafo=2) # overdispersion present

nb_chatbot <- glm.nb(nrchatbot ~ satisfaction  + neg_service + neg_chatbot
                     + nr_solar_panels + solar_panels + email_pers_solar +
                       email_sustainability + email_loyalty + email_save + relationship_length,
                     data = df)
summary(nb_chatbot)
vif(nb_chatbot) 

cbnullmodel <-glm(nrchatbot ~  1, data = df, family = 'poisson')
scnullmodel <-glm(nrservice ~  1, data = df, family = 'poisson')

AIC(cbnullmodel,p_chatbot,nb_chatbot)
BIC(cbnullmodel,p_chatbot,nb_chatbot)

AIC(scnullmodel,p_service,servicecalls_negbin)
BIC(scnullmodel,p_service,servicecalls_negbin)

# LR test
anova(cbnullmodel, nb_chatbot, test = 'Chisq')
anova(scnullmodel, servicecalls_negbin, test = 'Chisq')

# Q5 - Cox PH Second service ----
# Create duration and event variables

# Create CoxPH for calls
df$days_to_sec_service <- df$sec_service - df$first_service
df$days_to_sec_service <- ifelse(
  is.na(df$days_to_sec_service),
  as.numeric(as.Date(max(df$last_chatbot)) - df$first_service), # Max last chatbot is the latest date existent in the dataset. So we see that as "Today".  
  df$days_to_sec_service
)

df$sec_service_event <- ifelse(!is.na(df$sec_service), 1, 0)
df$relationship_length <- 2022 - df$customer_since_yr

sec_service_ph<- coxph(Surv(days_to_sec_service, sec_service_event) ~ av_bill 
                       + av_service_length + neg_service + satisfaction + av_chatbot_length + nr_solar_panels + solar_panels
                       + neg_chatbot + email_information 
                       , data = df)
vif(sec_service_ph)
summary(sec_service_ph)

ph_test_sec_service <- cox.zph(sec_service_ph) # proportionality test
print(ph_test_sec_service) # proportionality does not hold
plot(ph_test_sec_service)

sec_service_ph_tt<- coxph(Surv(days_to_sec_service, sec_service_event) ~ tt(av_bill) 
                          + av_service_length + tt(neg_service) + satisfaction + tt(av_chatbot_length) + tt(nr_solar_panels) + tt(solar_panels)
                          + tt(neg_chatbot) + email_information  , data = df)
summary(sec_service_ph_tt)

plot(ph_test_sec_service, var = "solar_panels", main = "Time-varying Effect: solar_panels")
abline(h = 0, col = "red", lty = 2) # Quite constant! Maybe do this for the other variables as well?



# Create CoxPH for chatbot interactions

df$days_to_sec_chatbot <- df$second_chatbot - df$first_chatbot
df$days_to_sec_chatbot <- ifelse(
  is.na(df$days_to_sec_chatbot),
  as.numeric(as.Date(max(df$last_chatbot)) - df$first_chatbot), # Max last chatbot is the latest date existent in the dataset. So we see that as "Today".
  df$days_to_sec_chatbot
)

df$sec_chatbot_event <- ifelse(!is.na(df$second_chatbot), 1, 0)

sec_chatbot_ph<- coxph(Surv(days_to_sec_chatbot, sec_chatbot_event) ~ av_bill 
                       + av_service_length + neg_service + satisfaction + av_chatbot_length + nr_solar_panels + solar_panels
                       + neg_chatbot + email_information 
                       , data = df)
summary(sec_chatbot_ph)

ph_test_sec_chatbot <- cox.zph(sec_chatbot_ph) # proportionality test
print(ph_test_sec_chatbot) # proportionality does not hold
plot(ph_test_sec_chatbot)

sec_chatbot_ph_tt<- coxph(Surv(days_to_sec_chatbot, sec_chatbot_event) ~ tt(av_bill) 
                          + av_service_length + tt(neg_service) + satisfaction + av_chatbot_length + tt(nr_solar_panels) + tt(solar_panels)
                          + tt(neg_chatbot) + tt(email_information)
                       , data = df)
summary(sec_chatbot_ph_tt)

plot(ph_test_sec_chatbot, var = "email_information", main = "Time-varying Effect: email_information")
abline(h = 0, col = "red", lty = 2) # Quite constant!

plot(ph_test_sec_chatbot, var = "nr_solar_panels", main = "Time-varying Effect: nr_solar_panels")
abline(h = 0, col = "red", lty = 2)

null_sc_ph<- coxph(Surv(days_to_sec_service, sec_service_event) ~ 1, data = df)
null_cb_ph<- coxph(Surv(days_to_sec_chatbot, sec_chatbot_event) ~ 1, data = df)


AIC(null_sc_ph,sec_service_ph,sec_service_ph_tt)
BIC(null_sc_ph,sec_service_ph,sec_service_ph_tt)

AIC(null_cb_ph,sec_chatbot_ph,sec_chatbot_ph_tt)
BIC(null_cb_ph,sec_chatbot_ph,sec_chatbot_ph_tt)
