##################################################################
##                       Loading packages                       ##
##################################################################
#loading required data
library(tidyverse)


#################################################################
##                        Baseline data                        ##
#################################################################


##################################################################
##                           EMA data                           ##
##################################################################
#loading data
data <- readxl::read_excel("./data/ema_raw.xlsx", sheet = 1)

variables <- read.csv("./data/variables.csv")

key <- read.csv("./data/key.csv")
key$mbl_cod <- as.character(key$mbl_cod)

#matching ids
data_raw <- full_join(key, data, by = "mbl_cod") 

#adding "time" variables
data_raw <- data_raw %>%
  mutate(date = date(data_raw$scheduled_start_local), 
         .after = scheduled_start_local) %>% 
  mutate(hour = as.character(hour(data_raw$scheduled_start_local)),
         .after = date) %>%
  mutate(minute = as.character(minute(data_raw$scheduled_start_local)),
         .after = hour)
  
#day and occasion variables  
data_raw <- data_raw %>% 
  group_by(id) %>% 
  mutate(day = as.numeric(as.factor(date)),
         .after = minute) %>% 
  mutate(occasion = recode(hour,
                            "9" = "1",
                            "13" = "2",
                            "17" = "3"), .after = day)
  
#removing data from software errors, unknown sources, and administrative testing account
data_clean <- data_raw %>% 
  filter((hour == "9" | hour == "13" | hour == "17") & minute == "0") %>% #due to software errors or timezone issues(?), some participants were sent prompts outside of the three scheduled daily timings (9am, 1pm, 5pm), this line of code removes data from all erroneously sent EMAs
  filter(!(is.na(id))) %>% #removes testing data from unknown source(s) that does not match any mobile codes in our key; from mEMA administrators(?)
  filter(!(id == "admin_1")) %>% #removing data from administrative testing account
  filter(!(day > 21)) #due to software errors, some participants were sent EMA prompts after the 21-day study period. This line of code removes this "extra" data

#for documentation: here are the data that the above code removes
data_dirty <- anti_join(data_raw, data_clean)
#write.csv(data_dirty, "./data/ema_removed.csv")

#selecting relevant variables for network estimation
data_clean <- data_clean %>% 
  select(id, scheduled_start_local, day, occasion, has_data, SE_2, TPESS_1:TPESS_10,SYMP_1:SYMP_12)
names(data_clean)[6:28] <- variables$label #to clean up labels

#adding time variable to cleaned data
data_clean <- data_clean %>% 
  mutate(time = row_number(), .after = scheduled_start_local) #ranges from 1 to 63

#write.csv(data_clean, "./data/ema_clean.csv", row.names = FALSE)