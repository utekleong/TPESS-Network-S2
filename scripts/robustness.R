#################################################################
##                     Robustness analysis                     ##
#################################################################
# splitting ids randomly into two separate subsets:
participants <- unique(data_detrended$id) %>% 
  as.data.frame()

#specifying exact split:
{set.seed(42) #to ensure reproducibility of sample split
  split <- as.vector(c(rep(TRUE, 146), rep(FALSE, 146))) #ensuring subsets are exactly half of full sample
  split_index <- sample(split)
  }

#getting subset membership:
sample_A <- participants[split_index,]
sample_B <- participants[!split_index,]

#splitting detrended data into subsets:
data_detrended_A <- data_detrended %>% 
  filter(id %in% sample_A)
data_detrended_B <- data_detrended %>% 
  filter(id %in% sample_B)

#fitting networks to each subset:
# net_A <- mlVAR(data_detrended_A, vars = vars,
#              idvar = "id", dayvar = "day",
#              temporal = "correlated", contemporaneous = "correlated")
# saveRDS(net_A, "./data/RDS/network_robustness_A).RDS")
net_A <- readRDS("./data/RDS/network_robustness_A.RDS")

ew_temp_A <- net_A$results$Beta$mean[,,1]
ew_contemp_A <- net_A$results$Theta[["pcor"]][["mean"]]
ew_between_A <- net_A$results$Omega_mu[["pcor"]][["mean"]]

# net_B <- mlVAR(data_detrended_B, vars = vars,
#                idvar = "id", dayvar = "day",
#                temporal = "correlated", contemporaneous = "correlated")
# saveRDS(net_B, "./data/RDS/network_robustness_B).RDS")
net_B <- readRDS("./data/RDS/network_robustness_B.RDS")

ew_temp_B <- net_B$results$Beta$mean[,,1]
ew_contemp_B <- net_B$results$Theta[["pcor"]][["mean"]]
ew_between_B <- net_B$results$Omega_mu[["pcor"]][["mean"]]

#robustness
cor.test(ew_temp_A, ew_temp_B)
cor.test(ew_contemp_A, ew_contemp_B)
cor.test(ew_between_A, ew_between_B)
