#################################################################
##                  Loading packages and data                  ##
#################################################################
#loading packages:
library(tidyverse)
library(mlVAR)
library(flextable)
library(lme4)
library(lmerTest)
library(multilevelTools)

#loading baseline data:
baseline_raw <- read.csv("./data/baseline_clean.csv")

#loading and cleaning EMA data:
ema_raw <- read.csv("./data/ema_clean.csv")
ema_raw <- ema_raw %>%
  mutate(core = rowMeans(select(ema_raw, starts_with("tpess")))) #computing mean score for 10-item TPESS

#removing participants who responded to less than 21 EMAs
occasion_count <- ema_raw %>%
  group_by(id) %>%
  filter(has_data == "true") %>%
  count() %>%
  filter(n > 20)

ema_clean <- ema_raw %>%
  filter(id %in% occasion_count$id) %>% 
  select(id, time, day, occasion, nse, anh:core)

ema_reliability <- ema_raw %>% 
  filter(id %in% occasion_count$id) %>% 
  select(id, time, day, occasion, starts_with("tpess"))

#################################################################
##                    Baseline descriptives                    ##
#################################################################
#subsetting baseline dataset to those retained for analysis
baseline_clean <- baseline_raw %>% 
  filter(id %in% occasion_count$id)

baseline_clean <- baseline_clean %>% 
  mutate(tpess_mean = rowMeans(select(baseline_clean, starts_with("tpess"))))

#cleaning up variable types
baseline_clean$gender <- as.character(baseline_clean$gender)  
baseline_clean$diag <- as.character(baseline_clean$diag)

#cleaning up coding
baseline_clean <- baseline_clean %>% 
  mutate(
    
    #cleaning up coding for gender:
    gender = case_match(
      gender,
      "1" ~ "male",
      "2" ~ "female",
      "3" ~ "undisclosed"),
    
    #cleaning up coding for ethnicity:
    ethnicity = case_when(
      str_detect(ethnicity, regex("chinese", ignore_case = TRUE)) ~ "chinese",
      str_detect(ethnicity, regex("india", ignore_case = TRUE)) ~ "indian",
      str_detect(ethnicity, regex("malay", ignore_case = TRUE)) ~ "malay",
      .default = ethnicity),
    
    #cleaning up coding for diagnosis history:
    diag = case_match(
      diag,
      "1" ~ "yes",
      "2" ~ "no")
    )

#cleaning up ethnicity data
baseline_clean$ethnicity <- tolower(baseline_clean$ethnicity)

#sample characteristics
mean(baseline_clean$age) %>%
  round(digits = 2)
sd(baseline_clean$age) %>% 
  round(digits = 2)
range(baseline_clean$age)

baseline_clean %>% #breakdown of gender
  count(gender)

baseline_clean %>% #breakdown of past history of diagnosis (>1 year)
  count(diag)

baseline_clean %>% #breakdown of ethnicity
  count(ethnicity)

#TPESS descriptives @ baseline
mean(baseline_clean$tpess_mean) %>%
  round(digits = 2)
sd(baseline_clean$tpess_mean) %>%
  round(digits = 2)

#calculating reliability for TPESS (raw_alpha of output)
baseline_clean %>% 
  select(starts_with("tpess"), -tpess_mean) %>% 
  psych::alpha()

#excluded participants
baseline_excluded <- baseline_raw  %>% 
  filter(!(id %in% occasion_count$id))

baseline_excluded <- baseline_excluded %>% 
  mutate(tpess_mean = rowMeans(select(baseline_excluded, starts_with("tpess"))))
  
#TPESS descriptives of excluded
mean(baseline_excluded$tpess_mean) %>%
  round(digits = 2)
sd(baseline_excluded$tpess_mean) %>%
  round(digits = 2)

t.test(baseline_clean$tpess_mean, baseline_excluded$tpess_mean,
       alternative = "two.sided")

#multi-level model (can baseline TPESS scores predict daily mean TPESS scores?)
mlm_baseline <- baseline_clean %>% 
  select(id, tpess_mean)

mlm_data <- ema_clean %>%
  left_join(mlm_baseline, by = "id")

mlm <- lmer(core ~ 1 + tpess_mean + (1 | id),
            data = mlm_data)

##################################################################
##                       EMA descriptives                       ##
##################################################################
#how many EMAs were completed in total?:
ema_raw %>% 
  count(has_data)

#how many EMAs were completed by participants with >20 measurement occasions:
ema_20 <- ema_raw %>% 
  filter(id %in% occasion_count$id) %>% #this subsets full dataset to the sample of participants who provided >20 measurement occasions.
  filter(has_data == "true")
  
n_20 <- length(unique(ema_20$id)) #number of participants with >20 measurement occasions
n_20

m_20 <- nrow(ema_20) #total number of measurement occasions provided by participants with >20 measurement occasions
m_20

m_20/n_20 #average number of measurement occasions completed

#computing person-specific means of EMA variables:
person_mean <- ema_clean %>% 
  select(id,nse, anh:core) %>% 
  group_by(id) %>% 
  summarise_all(~ mean(.x, na.rm = TRUE))

#computing person-specific SDs of EMA variables:
person_sd <- ema_clean %>% 
  select(id,nse, anh:core) %>% 
  group_by(id) %>% 
  summarise_all(~ sd(.x, na.rm = TRUE))

#computing average and SDs of person-specific means:
study_mean <- person_mean %>% 
  select(!id) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(psm_mean = mean(value, na.rm = TRUE),
            psm_sd = sd(value, na.rm = TRUE))

#computing average and SDs of person-specific SDs:
study_sd <- person_sd %>% 
  select(!id) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(pssd_mean = mean(value, na.rm = TRUE),
            pssd_sd = sd(value, na.rm = TRUE))

#constructing table for manuscript
persondesc_data <- left_join(study_mean,study_sd, by = "name")
persondesc_data[2:5] <- round(persondesc_data[2:5], digits = 2)
persondesc_data <- mutate(persondesc_data, 
                          psm = paste0(psm_mean, " ","(", psm_sd, ")"),
                          pssd = paste0(pssd_mean, " ","(", pssd_sd, ")"), .keep = "unused")

persondesc_table <- persondesc_data %>% 
  arrange(name) %>% 
  flextable() %>% 
  set_header_labels(name = "Node label",
                    psm = "Person-specific mean",
                    pssd = "Person-specific standard deviation") %>% 
  autofit()
#print(persondesc_table, preview = "docx")

#using multilevelTools::omegaSEM to estimate between- and within-person reliability of the Brief TPESS
items_brief_tpess <- ema_reliability %>% 
  select(starts_with("tpess")) %>% 
  names()
omegaSEM(items_brief_tpess, id = "id", data = ema_reliability)

#################################################################
##                       Detrending data                       ##
#################################################################
#creating new data frame to store residuals:
data_residuals <- as.data.frame(matrix(0, nrow = nrow(ema_clean), ncol = ncol(ema_clean)))

#creating variable to iterate over:
variables <- colnames(ema_clean) 

#loop to regress time on each variable of interest and extract residuals
for (v in 5:length(variables)) {
  
  formula <- as.formula(paste0(variables[v], "~ time")) #specifying formula to regress time on each variable of interest; variables[v] returns variable name i.e. "pleasure"
  regression_detrend <- lm(formula, data = ema_clean, na.action = na.exclude) #na.exclude ensures that residuals() will pad output with NAs for ommited cases (to ensure matched length)
  data_residuals[v] <- residuals(regression_detrend) #assigning residuals to corresponding column
  }

#creating new data frame for detrended data:
ema_detrended <- ema_clean 

#replacing observed scores with residuals:
ema_detrended[5:18] <- data_residuals[5:18]

##################################################################
##                      Network estimation                      ##
##################################################################
# #fitting the network:
vars <- colnames(ema_detrended[5:18])
# net <- mlVAR(ema_detrended, vars = vars,
#              idvar = "id", dayvar = "day",
#              temporal = "correlated", contemporaneous = "correlated")
# saveRDS(net, "./data/RDS/network_correlated.RDS")
net <- readRDS("./data/RDS/network_correlated.RDS") #load RDS file instead; fitting network with temporal/contempraneous = "correlated" is extremely computationally intensive
 
#grouping:
grouping <- list("NSE" = c(1),
                 "Symptoms" = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                 "TPESS" = c(14))

#temporal network:
summary_temporal <- summary(net)$temporal
plot(net, "temporal", nonsig = "hide", rule = "and", 
     layout = "spring", theme = "colorblind", alpha = .01,
     curve = 0.5, curveAll = FALSE,
     groups = grouping,
     legend = FALSE,
     filename = "temporalnet", filetype = "png", width = 20, height = 20)

#contemporaneous network:
summary_contemp <- summary(net)$contemporaneous
plot(net, "contemporaneous", nonsig = "hide", rule = "and", 
     layout = "spring", theme = "colorblind", alpha = .01,
     curve = 0.5, curveAll = FALSE, repulsion = 0.8,
     groups = grouping,
     legend = FALSE,
     filename = "contempnet", filetype = "png", width = 20, height = 20)

#between-person network:
summary_between <- summary(net)$between
plot(net, "between", nonsig = "hide", rule = "and", 
     layout = "spring", theme = "colorblind", alpha = .01,
     curve = 0.5, curveAll = FALSE, repulsion = 0.8,
     groups = grouping,
     legend = FALSE,
     filename = "betweennet", filetype = "png", width = 20, height = 20)
