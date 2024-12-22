# Article: "Conditional under what conditions? Analysing the democratic preconditions for the EU’s application of ex-ante aid allocation in the European neighbourhood"
# Journal: JCMS
# Date: December 22, 2014

rm(list = ls())

library(pastecs)
library(stargazer)
library(MASS)
library(tseries)
library(lmtest)
library(nlme)
library(lme4)
library(plm)
library(Formula)
library(arm) 
library(pcse)
library(tseries)
library(nlme)
library(carData)
library(pastecs)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(ggh4x) 
library(gplots)
library(lmtest)
library(car)
library(dplyr)
library(readxl)

data <- read_excel("data.xlsx")

View(data)    

df <- as.data.frame(data) 
head(df)


df$distance
distance_new <- df$distance + 0.01
distance_new

df["log_distance"] <- log(distance_new)
df$log_distance
head(df)

df$log_eu_aid <- as.numeric(df$log_eu_aid)
df$v2x_polyarchy_lag1 <- as.numeric(df$v2x_polyarchy_lag1)
df$gdp_cap <- as.numeric(df$gdp_cap)
df$eu_stat <- as.numeric(df$eu_stat)
df$conflict <- as.numeric(df$conflict)
df$population <- as.numeric(df$population)
df$log_import_resources <- as.numeric(v$log_import_resources)
df$ln_migr <- as.numeric(df$ln_migr)
df$import_ng <- as.numeric(df$import_ng)
df$former_eu_colony <- as.numeric(df$former_eu_colony)
df$v2x_polyarchy_lag2 <- as.numeric(df$v2x_polyarchy_lag3)
df$v2x_polyarchy_lag4 <- as.numeric(df$v2x_polyarchy_lag4)
df$v2x_polyarchy_lag5 <- as.numeric(df$v2x_polyarchy_lag5)
df$eu_trade_bal <- as.numeric(df$eu_trade_bal)
df$v2x_polyarchy <- as.numeric(df$v2x_polyarchy)

df$log_import_ng <- log1p(df$import_ng)
df$log_gdp <- log1p(df$gdp_cap)
df$log_pop <- log1p(df$population)



min_value <- min(df$eu_trade_bal, na.rm = TRUE)
if (min_value <= 0) {
  shift_value <- abs(min_value) + 1
  
  df$shifted_eu_trade_bal <- df$eu_trade_bal + shift_value
  
  df$log_eu_trade_bal <- log(df$shifted_eu_trade_bal)
  
  print(paste("Shifted by:", shift_value))
  
} else {
  
  df$log_eu_trade_bal <- log(df$eu_trade_bal)
}

###### ###### ###### H1, Table 1 ###### ###### ###### 

#t-1
rev_plm_fe <- plm(log_eu_aid ~ v2x_polyarchy_lag1 + log(gdp_cap) + eu_stat  + conflict + log(population) + ln_migr + log1p(import_ng) + log_eu_trade_bal, data = df, model = "within", 
                  index = c("country_name", "year"))

coeftest(rev_plm_fe, vcov=vcovSCC(rev_plm_fe))
summary(rev_plm_fe, vcov=vcovSCC)


#t-2
rev_plm_fe_2 <- plm(log_eu_aid ~ v2x_polyarchy_lag2 + log(gdp_cap) + eu_stat + conflict + log(population) + ln_migr + log1p(import_ng) + log_eu_trade_bal, data = df, model = "within", 
                    index = c("country_name", "year"))
coeftest(rev_plm_fe_2, vcov=vcovSCC(rev_plm_fe_2))
summary(rev_plm_fe_2, vcov=vcovSCC)

#t-3
rev_plm_fe_3 <- plm(log_eu_aid ~ v2x_polyarchy_lag3 + log(gdp_cap) + eu_stat  + conflict + log(population) + ln_migr + log1p(import_ng) + log_eu_trade_bal, data = df, model = "within", 
                    index = c("country_name", "year"))
coeftest(rev_plm_fe_3, vcov=vcovSCC(rev_plm_fe_3))
summary(rev_plm_fe_3, vcov=vcovSCC)

#t-4
rev_plm_fe_4 <- plm(log_eu_aid ~ v2x_polyarchy_lag4 + log(gdp_cap) + eu_stat  + conflict + log(population) + ln_migr + log1p(import_ng) + log_eu_trade_bal, data = df, model = "within", 
                    index = c("country_name", "year"))
coeftest(rev_plm_fe_4, vcov=vcovSCC(rev_plm_fe_4))
summary(rev_plm_fe_4, vcov=vcovSCC)

#t-5
rev_plm_fe_5 <- plm(log_eu_aid ~ v2x_polyarchy_lag5 + log(gdp_cap) + eu_stat + conflict + log(population) + ln_migr + log1p(import_ng) + log_eu_trade_bal, data = df, model = "within", 
                    index = c("country_name", "year"))
coeftest(rev_plm_fe_5, vcov=vcovSCC(rev_plm_fe_5))
summary(rev_plm_fe_5, vcov=vcovSCC)

###### ###### ############ ###### ############ ###### ######
###### ###### ###### H2 Table 2  ###### ###### ###### 
###### ###### ############ ###### ############ ###### ######

#t-1
rev_plm_fe_h2_ng <- plm(log_eu_aid ~ v2x_polyarchy_lag1 + v2x_polyarchy_lag1*log1p(import_ng) + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, model = "within", 
                        index = c("country_name", "year"))
coeftest(rev_plm_fe_h2_ng, vcov=vcovSCC(rev_plm_fe_h2_ng))
summary(rev_plm_fe_h2_ng, vcov=vcovSCC)

#t-2
rev_plm_fe_h2b_ng <- plm(log_eu_aid ~ v2x_polyarchy_lag2 + v2x_polyarchy_lag2*log1p(import_ng) + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, model = "within", 
                         index = c("country_name", "year"))
coeftest(rev_plm_fe_h2b_ng, vcov=vcovSCC(rev_plm_fe_h2b_ng))
summary(rev_plm_fe_h2b_ng, vcov=vcovSCC)

#t-3
rev_plm_fe_h2c_ng <- plm(log_eu_aid ~ v2x_polyarchy_lag3 + v2x_polyarchy_lag3*log1p(import_ng) + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, model = "within", 
                         index = c("country_name", "year"))
coeftest(rev_plm_fe_h2c_ng, vcov=vcovSCC(rev_plm_fe_h2c_ng))
summary(rev_plm_fe_h2c_ng, vcov=vcovSCC)

#t-4
rev_plm_fe_h2d_ng <- plm(log_eu_aid ~ v2x_polyarchy_lag4 + v2x_polyarchy_lag4*log1p(import_ng)  +  log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) +  ln_migr + log_eu_trade_bal, data = df, model = "within", 
                         index = c("country_name", "year"))
coeftest(rev_plm_fe_h2d_ng, vcov=vcovSCC(rev_plm_fe_h2d_ng))
summary(rev_plm_fe_h2d_ng, vcov=vcovSCC)

#t-5
rev_plm_fe_h2e_ng <- plm(log_eu_aid ~ v2x_polyarchy_lag5 + v2x_polyarchy_lag5*log1p(import_ng)  + log(gdp_cap) + eu_stat  + conflict + log(population)  + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, model = "within", 
                         index = c("country_name", "year"))
coeftest(rev_plm_fe_h2e_ng, vcov=vcovSCC(rev_plm_fe_h2e_ng))
summary(rev_plm_fe_h2e_ng, vcov=vcovSCC)

###### ###### ############ ###### ############ ###### ######
###### ###### ###### H2 Table 3 ###### ###### ###### 
###### ###### ############ ###### ############ ###### ######

#t-1
rev_plm_fe_mig_ng <- plm(log_eu_aid ~ v2x_polyarchy_lag1 + v2x_polyarchy_lag1*ln_migr + log1p(import_ng) + log(gdp_cap) + eu_stat  + conflict + log(population) + ln_migr + log_eu_trade_bal, data = df, model = "within", 
                         index = c("country_name", "year"))
summary(rev_plm_fe_mig_ng, vcov=vcovSCC)

#t-2 
rev_plm_fe_mig2 <- plm(log_eu_aid ~ v2x_polyarchy_lag2 + v2x_polyarchy_lag2*ln_migr + log1p(import_ng) + log(gdp_cap) + eu_stat  + conflict + log(population) + ln_migr + log_eu_trade_bal, data = df, model = "within", 
                       index = c("country_name", "year"))
summary(rev_plm_fe_mig2, vcov=vcovSCC)

#t-3
rev_plm_fe_mig3 <- plm(log_eu_aid ~ v2x_polyarchy_lag3 + v2x_polyarchy_lag3*ln_migr + log1p(import_ng) + log(gdp_cap) + eu_stat  + conflict + log(population) + ln_migr + log_eu_trade_bal, data = df, model = "within", 
                       index = c("country_name", "year"))
summary(rev_plm_fe_mig3, vcov=vcovSCC)

#t-4 
rev_plm_fe_mig4 <- plm(log_eu_aid ~ v2x_polyarchy_lag4 + v2x_polyarchy_lag4*ln_migr + log1p(import_ng) + log(gdp_cap) + eu_stat  + conflict + log(population) + ln_migr + log_eu_trade_bal, data = df, model = "within", 
                       index = c("country_name", "year"))
summary(rev_plm_fe_mig4, vcov=vcovSCC)

#t-5
rev_plm_fe_mig5 <- plm(log_eu_aid ~ v2x_polyarchy_lag5 + v2x_polyarchy_lag5*ln_migr + log1p(import_ng) + log(gdp_cap) + eu_stat  + conflict + log(population) + ln_migr + log_eu_trade_bal, data = df, model = "within", 
                       index = c("country_name", "year"))
summary(rev_plm_fe_mig5, vcov=vcovSCC)


###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
###### ###### ###### Robustness check  with RE ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 

rev_plm_re <- plm(log_eu_aid ~ v2x_polyarchy_lag1 + log(gdp_cap) + eu_stat + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, model = "random", 
                  index = c("country_name", "year"))
coeftest(rev_plm_re, vcov=vcovSCC(rev_plm_re))
summary(rev_plm_re, vcov=vcovSCC)

rev_plm_re_2 <- plm(log_eu_aid ~ v2x_polyarchy_lag2 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, model = "random", 
                    index = c("country_name", "year"))
coeftest(rev_plm_re_2, vcov=vcovSCC(rev_plm_re_2))
summary(rev_plm_re_2, vcov=vcovSCC)

rev_plm_re_3 <- plm(log_eu_aid ~ v2x_polyarchy_lag3 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, model = "random", 
                    index = c("country_name", "year"))
coeftest(rev_plm_re_3, vcov=vcovSCC(rev_plm_re_3))
summary(rev_plm_re_3, vcov=vcovSCC)

rev_plm_re_4 <- plm(log_eu_aid ~ v2x_polyarchy_lag4 + log(gdp_cap) + eu_stat   + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, model = "random", 
                    index = c("country_name", "year"))
coeftest(rev_plm_re_4, vcov=vcovSCC(rev_plm_re_4))
summary(rev_plm_re_4, vcov=vcovSCC)

rev_plm_re_5 <- plm(log_eu_aid ~ v2x_polyarchy_lag5 + log(gdp_cap) + eu_stat + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, model = "random", 
                    index = c("country_name", "year"))
coeftest(rev_plm_re_5, vcov=vcovSCC(rev_plm_re_5))
summary(rev_plm_re_5, vcov=vcovSCC)

###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
### ### Two-way FE ### ### 
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 

rev_plm_fe_2w <- plm(log_eu_aid ~ v2x_polyarchy_lag1 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, effect = "twoways", model = "within")
summary(rev_plm_fe_2w, vcov=vcovSCC)

rev_plm_fe_2wb <- plm(log_eu_aid ~ v2x_polyarchy_lag2 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, effect="twoways", model = "within")
summary(rev_plm_fe_2wb, vcov=vcovSCC)

rev_plm_fe_2wc <- plm(log_eu_aid ~ v2x_polyarchy_lag3 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, effect="twoways", model = "within")
summary(rev_plm_fe_2wc, vcov=vcovSCC)

rev_plm_fe_2wd <- plm(log_eu_aid ~ v2x_polyarchy_lag4 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, effect = "twoways", model = "within")
summary(rev_plm_fe_2wd, vcov=vcovSCC)

rev_plm_fe_2we <- plm(log_eu_aid ~ v2x_polyarchy_lag5 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, effect = "twoways", model = "within")
summary(rev_plm_fe_2we, vcov=vcovSCC)

###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
### ### Add year ### ### 
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 

#"wb_region", "eap_region", "mena_region", "ln_migr", "log_imp_rus", "log_exp_rus",  "log_imp_chin", "log_exp_chin"

wdi.plm$wb_region <- as.numeric(wdi.plm$wb_region)
wdi.plm$eap_region <- as.numeric(wdi.plm$eap_region)
wdi.plm$mena_region <- as.numeric(wdi.plm$mena_region)
wdi.plm$distance <- as.numeric(wdi.plm$distance)


#t-1
plm_y_fe <- plm(log_eu_aid ~ v2x_polyarchy_lag1 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + log_eu_trade_bal + wb_region + eap_region + mena_region + ln_migr, data = df, model = "random", 
                index = c("country_name", "year"))
coeftest(plm_y_fe, vcov=vcovSCC(plm_y_fe))
summary(plm_y_fe, vcov=vcovSCC)

#t-2 
plm_y_fe2 <- plm(log_eu_aid ~ v2x_polyarchy_lag2 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + log_eu_trade_bal + wb_region + eap_region + mena_region + ln_migr, data = df, model = "random", 
                 index = c("country_name", "year"))
coeftest(plm_y_fe2, vcov=vcovSCC(plm_y_fe))
summary(plm_y_fe2, vcov=vcovSCC)

#t-3
plm_y_fe3 <- plm(log_eu_aid ~ v2x_polyarchy_lag3 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + log_eu_trade_bal + wb_region + eap_region + mena_region + ln_migr, data = df, model = "random", 
                 index = c("country_name", "year"))
coeftest(plm_y_fe3, vcov=vcovSCC(plm_y_fe))
summary(plm_y_fe3, vcov=vcovSCC)

#t-4
plm_y_fe4 <- plm(log_eu_aid ~ v2x_polyarchy_lag4 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + log_eu_trade_bal + wb_region + eap_region + mena_region + ln_migr, data = df, model = "random", 
                 index = c("country_name", "year"))
coeftest(plm_y_fe4, vcov=vcovSCC(plm_y_fe))
summary(plm_y_fe4, vcov=vcovSCC)

#t-5
plm_y_fe5 <- plm(log_eu_aid ~ v2x_polyarchy_lag5 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + log_eu_trade_bal + wb_region + eap_region + mena_region + ln_migr, data = df, model = "random", 
                 index = c("country_name", "year"))
coeftest(plm_y_fe5, vcov=vcovSCC(plm_y_fe))
summary(plm_y_fe5, vcov=vcovSCC)

###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
### ### Additional checks (distance, former eu colony, import ff + oil) ### ### 
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 


#t-1
plm_y2_fe <- plm(log_eu_aid ~ v2x_polyarchy_lag1 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal + log(distance) + log_import_resources + former_eu_colony, data = df, model = "random", 
                 index = c("country_name", "year"))
coeftest(plm_y2_fe, vcov=vcovSCC(plm_y_fe))
summary(plm_y2_fe, vcov=vcovSCC)

#t-2
plm_y2_fe2 <- plm(log_eu_aid ~ v2x_polyarchy_lag2 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal + log(distance) + log_import_resources + former_eu_colony, data = df, model = "random", 
                  index = c("country_name", "year"))
coeftest(plm_y2_fe2, vcov=vcovSCC(plm_y_fe))
summary(plm_y2_fe2, vcov=vcovSCC)

#t-3
plm_y2_fe3 <- plm(log_eu_aid ~ v2x_polyarchy_lag3 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal + log(distance) + log_import_resources + former_eu_colony, data = df, model = "random", 
                  index = c("country_name", "year"))
coeftest(plm_y2_fe3, vcov=vcovSCC(plm_y_fe))
summary(plm_y2_fe3, vcov=vcovSCC)

#t-4
plm_y2_fe4 <- plm(log_eu_aid ~ v2x_polyarchy_lag4 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal + log(distance) + log_import_resources + former_eu_colony, data = df, model = "random", 
                  index = c("country_name", "year"))
coeftest(plm_y2_fe4, vcov=vcovSCC(plm_y_fe))
summary(plm_y2_fe4, vcov=vcovSCC)

#t-5
plm_y2_fe5 <- plm(log_eu_aid ~ v2x_polyarchy_lag5 + log(gdp_cap) + eu_stat  + conflict + log(population) + log1p(import_ng) + ln_migr + log_eu_trade_bal + log(distance) + log_import_resources + former_eu_colony, data = df, model = "random", 
                  index = c("country_name", "year"))
coeftest(plm_y2_fe5, vcov=vcovSCC(plm_y_fe))
summary(plm_y2_fe5, vcov=vcovSCC)


###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
#### 2SLS #### -
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
library(ivreg)


df <- df %>%
  group_by(country_name) %>%                # Group by unit
  mutate(log_eu_aid_lag1 = lag(log_eu_aid, n = 1)) %>%  # Lag within group
  ungroup()                         # Remove grouping after operation

head(df$log_eu_aid_lag1)
head(df$log_eu_aid)

df <- df %>%
  group_by(country_name) %>%                # Group by unit
  mutate(log_eu_aid_lag2 = lag(log_eu_aid, n = 2)) %>%  # Lag within group
  ungroup()                         # Remove grouping after operation

df <- df %>%
  group_by(country_name) %>%                # Group by unit
  mutate(log_eu_aid_lag3 = lag(log_eu_aid, n = 3)) %>%  # Lag within group
  ungroup()                         # Remove grouping after operation

df <- df %>%
  group_by(country_name) %>%                # Group by unit
  mutate(log_eu_aid_lag4 = lag(log_eu_aid, n = 4)) %>%  # Lag within group
  ungroup()                         # Remove grouping after operation

df <- df %>%
  group_by(country_name) %>%                # Group by unit
  mutate(log_eu_aid_lag5 = lag(log_eu_aid, n = 5)) %>%  # Lag within group
  ungroup()                         # Remove grouping after operation

df <- df %>%
  group_by(country_name) %>%                # Group by unit
  mutate(log_eu_aid_lag6 = lag(log_eu_aid, n = 6)) %>%  # Lag within group
  ungroup()                         # Remove grouping after operation

df <- df %>%
  group_by(country_name) %>%                # Group by unit
  mutate(log_eu_aid_lag7 = lag(log_eu_aid, n = 7)) %>%  # Lag within group
  ungroup()                         # Remove grouping after operation



rev_m_iv <- ivreg(log_eu_aid ~ v2x_polyarchy_lag1  + log1p(import_ng)  + ln_migr  + log(population) + log(gdp_cap)  + eu_stat + conflict + log(eu_trade_bal)  | log_eu_aid_lag2 + log_eu_aid_lag3 + log(population) + log(gdp_cap) + eu_stat + conflict + log1p(import_ng)  + ln_migr + log(eu_trade_bal), data = df, method = "OLS", index = c("country_name", "year"))
summary(rev_m_iv)


rev_m_iv_2 <- ivreg(log_eu_aid ~ v2x_polyarchy_lag2  + log1p(import_ng)  + ln_migr  + log(population) + log(gdp_cap)  + eu_stat + conflict + log_eu_trade_bal  | log_eu_aid_lag3 + log_eu_aid_lag4 + log(population) + log(gdp_cap)  + eu_stat + conflict + log1p(import_ng) + ln_migr + log_eu_trade_bal, data = df, method = "OLS", index = c("country_name", "year"))
summary(rev_m_iv_2)


rev_m_iv_3 <- ivreg(log_eu_aid ~ v2x_polyarchy_lag3  + log1p(import_ng) + ln_migr  + log(population) + log(gdp_cap)  + eu_stat + conflict + log_eu_trade_bal   |
                      log_eu_aid_lag4 + log_eu_aid_lag5 + log(population) + log(gdp_cap)  + eu_stat + conflict + log1p(import_ng)  + ln_migr + log_eu_trade_bal,
                    data = df, method = "OLS", index = c("country_name", "year"))
summary(rev_m_iv_3)

rev_m_iv_4 <- ivreg(log_eu_aid ~ v2x_polyarchy_lag4  + log1p(import_ng)  + ln_migr  + log(population) + log(gdp_cap)  + eu_stat + conflict + log_eu_trade_bal  |
                      log_eu_aid_lag5 + log_eu_aid_lag6 + log(population) + log(gdp_cap)  + eu_stat + conflict + log1p(import_ng)  + ln_migr + log_eu_trade_bal,
                    data = df, method = "OLS", index = c("country_name", "year"))
summary(rev_m_iv_4)

rev_m_iv_5 <- ivreg(log_eu_aid ~ v2x_polyarchy_lag5  + log1p(import_ng)  + ln_migr  + log(population) + log(gdp_cap)  + eu_stat + conflict + log_eu_trade_bal   |
                      log_eu_aid_lag6 + log_eu_aid_lag7 + log(population) + log(gdp_cap)  + eu_stat + conflict + log1p(import_ng)  + ln_migr + log_eu_trade_bal,
                    data = df, method = "OLS", index = c("country_name", "year"))
summary(rev_m_iv_5)

#Aooendix Table 7
table7_appendix <- stargazer::stargazer(rev_m_iv,
                                        rev_m_iv_2,
                                        rev_m_iv_3,
                                        rev_m_iv_4,
                                        rev_m_iv_5,type="text",
                                        column.labels=c("t-1","t-2","t-3","t-4","t-5"), 
                                        dep.var.labels = c("Dependent variable: Aid from EU institutions"), 
                                        out="models_2sls.html", single.row=F)


