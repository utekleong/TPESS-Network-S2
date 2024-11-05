#################################################################
##                  Loading packages and data                  ##
#################################################################
#loading packages:
library(tidyverse)
library(mlVAR)

#loading and cleaning data:
data_raw <- read.csv("./data/ema_clean.csv")
data <- data_raw %>%
  mutate(core = rowMeans(select(data_raw, starts_with("tpess")))) #computing mean score for 10-item TPESS

#removing participants who responded to less than 21 EMAs
occasion_count <- data %>%
  group_by(id) %>%
  filter(has_data == "true") %>%
  count() %>%
  filter(n > 20)
data <- data %>%
  filter(id %in% occasion_count$id) %>% 
  select(id, time, day, occasion, nse, anh:core)

##################################################################
##                       EMA descriptives                       ##
##################################################################
#how many EMAs were completed in total?:
data_raw %>% 
  count(has_data)

#how many EMAs were completed by participants with >20 measurement occasions:
ema_20 <- data_raw %>% 
  filter(id %in% occasion_count$id) %>% #this subsets full dataset to the sample of participants who provided >20 measurement occasions.
  filter(has_data == "true")
  
n_20 <- length(unique(ema_20$id)) #number of participants with >20 measurement occasions
n_20

m_20 <- nrow(ema_20) #total number of measurement occasions provided by participants with >20 measurement occasions
m_20

m_20/n_20 #average number of measurement occasions completed

#computing person-specific means of EMA variables:
person_mean <- data %>% 
  select(id,nse, anh:core) %>% 
  group_by(id) %>% 
  summarise_all(~ mean(.x, na.rm = TRUE))
#write.csv(person_mean, "./data/ema_person_mean.csv")

#computing person-specific SDs of EMA variables:
person_sd <- data %>% 
  select(id,nse, anh:core) %>% 
  group_by(id) %>% 
  summarise_all(~ sd(.x, na.rm = TRUE))

#computing average and SDs of person-specific means:
study_mean <- person_mean %>% 
  select(!id) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE))

#computing average and SDs of person-specific SDs:
study_sd <- person_sd %>% 
  select(!id) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE))

#################################################################
##                       Detrending data                       ##
#################################################################
#creating new data frame to store residuals:
data_residuals <- as.data.frame(matrix(0, nrow = nrow(data), ncol = ncol(data)))

#creating variable to iterate over:
variables <- colnames(data) 

#loop to regress time on each variable of interest and extract residuals
for (v in 5:length(variables)) {
  
  formula <- as.formula(paste0(variables[v], "~ time")) #specifying formula to regress time on each variable of interest; variables[v] returns variable name i.e. "pleasure"
  regression_detrend <- lm(formula, data = data, na.action = na.exclude) #na.exclude ensures that residuals() will pad output with NAs for ommited cases (to ensure matched length)
  data_residuals[v] <- residuals(regression_detrend) #assigning residuals to corresponding column
  }

#creating new data frame for detrended data:
data_detrended <- data 

#replacing observed scores with residuals:
data_detrended[5:17] <- data_residuals[5:17]

##################################################################
##                      Network estimation                      ##
##################################################################
# #fitting the network:
vars <- colnames(data_detrended[5:18])
# net <- mlVAR(data_detrended, vars = vars,
#              idvar = "id", dayvar = "day",
#              temporal = "correlated", contemporaneous = "correlated")
# saveRDS(net, "./data/RDS/network_correlated.RDS")
net <- readRDS("./data/RDS/network_correlated.RDS") #load RDS file instead; fitting network with temporal/contempraneous = "correlated" is extremely computationally intensive

#grouping:
grouping <- list("NSE" = c(1),
                 "Symptoms" = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                 "TPESS" = c(14))

#temporal network:
plot(net, "temporal", nonsig = "hide", rule = "and", 
     layout = "spring", theme = "colorblind", alpha = .01,
     curve = 0.5, curveAll = TRUE,
     groups = grouping,
     filename = "temporalnet", filetype = "png", width = 20, height = 20)

#contemporaneous network:
plot(net, "contemporaneous", nonsig = "hide", rule = "and", 
     layout = "spring", theme = "colorblind", alpha = .01,
     curve = 0.5, curveAll = FALSE, repulsion = 0.8,
     groups = grouping,
     filename = "contempnet", filetype = "png", width = 20, height = 20)

#between-subjects network:
plot(net, "between", nonsig = "hide", rule = "and", 
     layout = "spring", theme = "colorblind", alpha = .01,
     curve = 0.5, curveAll = FALSE, repulsion = 0.8,
     groups = grouping, 
     filename = "betweennet", filetype = "png", width = 20, height = 20)
