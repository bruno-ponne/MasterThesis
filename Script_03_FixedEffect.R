#-----------------------------------#
# Script 03 - Fixed-effects models  #
#-----------------------------------#

library(lmtest)
library(sandwich)
library(tidyverse)
library(stargazer)

# Primary School (Table 3 - Panel A)

load("data/DATA_COMPLETE.RData")

DATA_PM <- DATA_COMPLETE %>% 
  filter(grade == 'P' & subject == 'math') %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(treatment = if_else(abbr_state == "CE" & year > 2008, 1, 0))

DATA_PP <- DATA_COMPLETE %>% 
  filter(grade=='P' & subject=='port') %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(treatment = if_else(abbr_state=="CE" & year > 2008, 1, 0))


# Primary School - Model 1
MODEL_PM01 <- lm(score ~ treatment + as.factor(abbr_state), data=DATA_PM)
MODEL_PM01_PL <- coeftest(MODEL_PM01, vcov = vcovPL(MODEL_PM01, cluster = ~ abbr_state))
MODEL_PM01_BS <- coeftest(MODEL_PM01, vcov = vcovBS(MODEL_PM01, cluster = ~ abbr_state, type = "residual"))


MODEL_PP01 <- lm(score ~ treatment + as.factor(abbr_state), data = DATA_PP)
MODEL_PP01_PL <- coeftest(MODEL_PP01, vcov = vcovPL(MODEL_PP01, cluster = ~ abbr_state))
MODEL_PP01_BS <- coeftest(MODEL_PP01, vcov = vcovCL(MODEL_PP01, cluster = ~ abbr_state, type = "residual"))



# Primary School - Model 2
MODEL_PM02 <- lm(score ~ treatment + as.factor(abbr_state) + as.factor(year), data=DATA_PM)
MODEL_PM02_PL <- coeftest(MODEL_PM02, vcov = vcovPL(MODEL_PM02, cluster = ~ abbr_state))
MODEL_PM02_BS <- coeftest(MODEL_PM02, vcov = vcovBS(MODEL_PM02, cluster = ~ abbr_state, type = "residual"))


MODEL_PP02 <- lm(score ~ treatment + as.factor(abbr_state) + as.factor(year), data = DATA_PP)
MODEL_PP02_PL <- coeftest(MODEL_PP02, vcov = vcovPL(MODEL_PP02, cluster = ~ abbr_state))
MODEL_PP02_BS <- coeftest(MODEL_PP02, vcov = vcovBS(MODEL_PP02, cluster = ~ abbr_state, type = "residual"))


# Primary School - Model 3
MODEL_PM03 <- lm(score ~ treatment + as.factor(abbr_state) + as.factor(year) + homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_PM)
MODEL_PM03_PL <- coeftest(MODEL_PM03, vcov = vcovPL(MODEL_PM03, cluster = ~ abbr_state))
MODEL_PM03_BS <- coeftest(MODEL_PM03, vcov = vcovBS(MODEL_PM03, cluster = ~ abbr_state, type = "residual"))


MODEL_PP03 <- lm(score ~ treatment + as.factor(abbr_state) + as.factor(year) + homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_PP)
MODEL_PP03_PL <- coeftest(MODEL_PP03, vcov = vcovPL(MODEL_PP03, cluster = ~ abbr_state))
MODEL_PP03_BS <- coeftest(MODEL_PP03, vcov = vcovBS(MODEL_PP03, cluster = ~ abbr_state, type = "residual"))


# Robust SE a la Newey-West (1987) and Driscoll and Kraay (1998) for panel data.

stargazer(MODEL_PM01_PL, 
          MODEL_PP01_PL, 
          MODEL_PM02_PL, 
          MODEL_PP02_PL, 
          MODEL_PM03_PL, 
          MODEL_PP03_PL, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))

# Bootstrap Robust SE

stargazer(MODEL_PM01_BS, 
          MODEL_PP01_BS, 
          MODEL_PM02_BS, 
          MODEL_PP02_BS, 
          MODEL_PM03_BS, 
          MODEL_PP03_BS, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))
# Normal SE

stargazer(MODEL_PM01, 
          MODEL_PP01, 
          MODEL_PM02, 
          MODEL_PP02, 
          MODEL_PM03, 
          MODEL_PP03, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))


# Lower Secondary School (Table 3 - Panel B)

DATA_LSM <- DATA_COMPLETE %>% 
  filter(grade=='LS' & subject=='math') %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(treatment = if_else(abbr_state=="CE" & year > 2008, 1, 0))


DATA_LSP <- DATA_COMPLETE %>% 
  filter(grade=='LS' & subject=='port') %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(treatment = if_else(abbr_state=="CE" & year > 2008, 1, 0))


# Lower Secondary School - Model 1
MODEL_LSM01 <- lm(score ~ treatment + as.factor(abbr_state), data=DATA_LSM)
MODEL_LSM01_PL <- coeftest(MODEL_LSM01, vcov = vcovPL(MODEL_LSM01, cluster = ~ abbr_state))
MODEL_LSM01_BS <- coeftest(MODEL_LSM01, vcov = vcovBS(MODEL_LSM01, cluster = ~ abbr_state, type = "residual"))


MODEL_LSP01 <- lm(score ~ treatment + as.factor(abbr_state), data = DATA_LSP)
MODEL_LSP01_PL <- coeftest(MODEL_LSP01, vcov = vcovPL(MODEL_LSP01, cluster = ~ abbr_state))
MODEL_LSP01_BS <- coeftest(MODEL_LSP01, vcov = vcovBS(MODEL_LSP01, cluster = ~ abbr_state, type = "residual"))


# Lower Secondary School - Model 2
MODEL_LSM02 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year), data=DATA_LSM)
MODEL_LSM02_PL <- coeftest(MODEL_LSM02, vcov = vcovPL(MODEL_LSM02, cluster = ~ abbr_state))
MODEL_LSM02_BS <- coeftest(MODEL_LSM02, vcov = vcovBS(MODEL_LSM02, cluster = ~ abbr_state, type = "residual"))


MODEL_LSP02 <- lm(score ~ treatment + as.factor(abbr_state) + as.factor(year), data = DATA_LSP)
MODEL_LSP02_PL <- coeftest(MODEL_LSP02, vcov = vcovPL(MODEL_LSP02, cluster = ~ abbr_state))
MODEL_LSP02_BS <- coeftest(MODEL_LSP02, vcov = vcovBS(MODEL_LSP02, cluster = ~ abbr_state, type = "residual"))


# Model 3 - Lower Secondary School

MODEL_LSM03 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year)+ homicides + edu_invest_pc + ln_pop + unemployment + TWh, data=DATA_LSM)
MODEL_LSM03_PL <- coeftest(MODEL_LSM03, vcov = vcovPL(MODEL_LSM03, cluster = ~ abbr_state))
MODEL_LSM03_BS <- coeftest(MODEL_LSM03, vcov = vcovBS(MODEL_LSM03, cluster = ~ abbr_state, type = "residual"))


MODEL_LSP03 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year)+ homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_LSP)
MODEL_LSP03_PL <- coeftest(MODEL_LSP03, vcov = vcovPL(MODEL_LSP03, cluster = ~ abbr_state))
MODEL_LSP03_BS <- coeftest(MODEL_LSP03, vcov = vcovBS(MODEL_LSP03, cluster = ~ abbr_state, type = "residual"))



# Robust SE a la Newey-West (1987) and Driscoll and Kraay (1998) for panel data.

stargazer(MODEL_LSM01_PL, 
          MODEL_LSP01_PL, 
          MODEL_LSM02_PL, 
          MODEL_LSP02_PL, 
          MODEL_LSM03_PL, 
          MODEL_LSP03_PL, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))

# Bootstrap Robust SE error

stargazer(MODEL_LSM01_BS, 
          MODEL_LSP01_BS, 
          MODEL_LSM02_BS, 
          MODEL_LSP02_BS, 
          MODEL_LSM03_BS, 
          MODEL_LSP03_BS, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))

# Normal SE

stargazer(MODEL_LSM01, 
          MODEL_LSP01, 
          MODEL_LSM02, 
          MODEL_LSP02, 
          MODEL_LSM03, 
          MODEL_LSP03, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))


# Upper Secondary School (Table 3 - Panel C)

DATA_USM <- DATA_COMPLETE %>% 
  filter(grade=='US' & subject=='math') %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(treatment = if_else(abbr_state=="CE" & year > 2010, 1, 0))


DATA_USP <- DATA_COMPLETE %>% 
  filter(grade=='US' & subject=='port') %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(treatment = if_else(abbr_state=="CE" & year > 2010, 1, 0))


# Model 1 - Upper Secondary School 
MODEL_USM01 <- lm(score ~ treatment + as.factor(abbr_state), data=DATA_USM)
MODEL_USM01_PL <- coeftest(MODEL_USM01, vcov = vcovPL(MODEL_USM01, cluster = ~ abbr_state))
MODEL_USM01_BS <- coeftest(MODEL_USM01, vcov = vcovBS(MODEL_USM01, cluster = ~ abbr_state, type = "webb"))

MODEL_USP01 <- lm(score ~ treatment + as.factor(abbr_state), data = DATA_USP)
MODEL_USP01_PL <- coeftest(MODEL_USP01, vcov = vcovPL(MODEL_USP01, cluster = ~ abbr_state))
MODEL_USP01_BS <- coeftest(MODEL_USP01, vcov = vcovBS(MODEL_USP01, cluster = ~ abbr_state, type = "webb"))


# Model 2 - Upper Secondary School 
MODEL_USM02 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year), data=DATA_USM)
MODEL_USM02_PL <- coeftest(MODEL_USM02, vcov = vcovPL(MODEL_USM02, cluster = ~ abbr_state))
MODEL_USM02_BS <- coeftest(MODEL_USM02, vcov = vcovBS(MODEL_USM02, cluster = ~ abbr_state, type = "webb"))

MODEL_USP02 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year), data = DATA_USP)
MODEL_USP02_PL <- coeftest(MODEL_USP02, vcov = vcovPL(MODEL_USP02, cluster = ~ abbr_state))
MODEL_USP02_BS <- coeftest(MODEL_USP02, vcov = vcovBS(MODEL_USP02, cluster = ~ abbr_state, type = "webb"))


# Model 3 - Upper Secondary School 

MODEL_USM03 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year)+ homicides + edu_invest_pc + ln_pop + unemployment + TWh, data=DATA_USM)
MODEL_USM03_PL <- coeftest(MODEL_USM03, vcov = vcovPL(MODEL_USM03, cluster = ~ abbr_state))
MODEL_USM03_BS <- coeftest(MODEL_USM03, vcov = vcovBS(MODEL_USM03, cluster = ~ abbr_state, type = "wild-webb"))

MODEL_USP03 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year)+ homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_USP)
MODEL_USP03_PL <- coeftest(MODEL_USP03, vcov = vcovPL(MODEL_USP03, cluster = ~ abbr_state))
MODEL_USP03_BS <- coeftest(MODEL_USP03, vcov = vcovBS(MODEL_USP03, cluster = ~ abbr_state, type = "wild-webb"))

# Robust SE a la Newey-West (1987) and Driscoll and Kraay (1998) for panel data.

stargazer(MODEL_USM01_PL, 
          MODEL_USP01_PL, 
          MODEL_USM02_PL, 
          MODEL_USP02_PL, 
          MODEL_USM03_PL, 
          MODEL_USP03_PL, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))
# Bootstrap Robust SE

stargazer(MODEL_USM01_BS, 
          MODEL_USP01_BS, 
          MODEL_USM02_BS, 
          MODEL_USP02_BS, 
          MODEL_USM03_BS, 
          MODEL_USP03_BS, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))
# Noraml SE

stargazer(MODEL_USM01, 
          MODEL_USP01, 
          MODEL_USM02, 
          MODEL_USP02, 
          MODEL_USM03, 
          MODEL_USP03, 
          type = "text", 
          omit = c("abbr_state","year", "Constant"),
          column.labels = c("Math", 
                            "Portuguese", 
                            "Math", 
                            "Portuguese",
                            "Math", 
                            "Portuguese"))


# ROBUSTNESS CHECKS - 2 way clustering with Bootstrap Robust SE


# Model 3 (P)
MODEL_PM03 <- lm(score ~ treatment + as.factor(abbr_state) + as.factor(year) + homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_PM)
summary(MODEL_PM03)
MODEL_PM03_BS_TW <- coeftest(MODEL_PM03, vcov = vcovBS(MODEL_PM03, cluster = ~ abbr_state + year, type = "residual"))
MODEL_PM03_BS_TW

MODEL_PP03 <- lm(score ~ treatment + as.factor(abbr_state) + as.factor(year) + homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_PP)
summary(MODEL_PP03)
MODEL_PP03_BS_TW <- coeftest(MODEL_PP03, vcov = vcovBS(MODEL_PP03, cluster = ~ abbr_state + year, type = "residual"))
MODEL_PP03_BS_TW 

# Model 3 (LS)

MODEL_LSM03 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year)+ homicides + edu_invest_pc + ln_pop + unemployment + TWh, data=DATA_LSM)
summary(MODEL_LSM03)
MODEL_LSM03_BS_TW <- coeftest(MODEL_LSM03, vcov = vcovBS(MODEL_LSM03, cluster = ~ abbr_state+year, type = "residual"))
MODEL_LSM03_BS_TW

MODEL_LSP03 <- lm(score ~ treatment + as.factor(abbr_state)+ as.factor(year)+ homicides + edu_invest_pc + ln_pop + unemployment + TWh, data = DATA_LSP)
summary(MODEL_LSP03)
MODEL_LSP03_BS_TW <- coeftest(MODEL_LSP03, vcov = vcovBS(MODEL_LSP03, cluster = ~ abbr_state+year, type = "residual"))
MODEL_LSP03_BS_TW
