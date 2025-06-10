###########################################################-
# Objective: Import click-train datasets and PAL Times (Periods when PAL was in the water)
# Author: Davide Bittelli
# Date Modified: 01.04.2025
###########################################################-

library(dplyr)

# load(file = "/Users/davide/R projects/PAL.CE.DMM/RDataEnvironments/FH22.RData")
# GW_FH22_train.details <- GW_FH22_train.details %>%
#   select(-c(Time_UTC, Time_CET))
# GE_FH22_train.details <- GE_FH22_train.details %>%
#   select(-c(Day, Timedrift, Time_adj))
# N30_FH22_train.details <- N30_FH22_train.details %>%
#   select(-c(Day, Timedrift, Time_adj))
# N60_FH22_train.details <- N60_FH22_train.details %>%
#   select(-c(Day, Timedrift, Time_adj))
# N90_FH22_train.details <- N90_FH22_train.details %>%
#   select(-c(Day, Timedrift, Time_adj))
# S30_FH22_train.details <- S30_FH22_train.details %>%
#   select(-c(Day, Timedrift, Time_adj))
# S60_FH22_train.details <- S60_FH22_train.details %>%
#   select(-c(Day, Timedrift, Time_adj))
# S90_FH22_train.details <- S90_FH22_train.details %>%
#   select(-c(Day, Timedrift, Time_adj))

# save(GW_FH22_train.details, GE_FH22_train.details,
#      N30_FH22_train.details, N60_FH22_train.details, N90_FH22_train.details,
#      S30_FH22_train.details, S60_FH22_train.details, S90_FH22_train.details,
#      PAL_times22,
#      file = "/Users/davide/R projects/PALFilterApproachDMM/REnvironment.RData")
# 
# load(file = "/Users/davide/R projects/PALFilterApproachDMM/REnvironment.RData")
# 
# 
# saveRDS(GW_FH22_train.details, file = "/Users/davide/R projects/PALFilterApproachDMM/data/GW_FH22_train.details.rds")
# saveRDS(GE_FH22_train.details, file = "/Users/davide/R projects/PALFilterApproachDMM/data/GE_FH22_train.details.rds")
# saveRDS(N30_FH22_train.details, file = "/Users/davide/R projects/PALFilterApproachDMM/data/N30_FH22_train.details.rds")
# saveRDS(N60_FH22_train.details, file = "/Users/davide/R projects/PALFilterApproachDMM/data/N60_FH22_train.details.rds")
# saveRDS(N90_FH22_train.details, file = "/Users/davide/R projects/PALFilterApproachDMM/data/N90_FH22_train.details.rds")
# saveRDS(S30_FH22_train.details, file = "/Users/davide/R projects/PALFilterApproachDMM/data/S30_FH22_train.details.rds")
# saveRDS(S60_FH22_train.details, file = "/Users/davide/R projects/PALFilterApproachDMM/data/S60_FH22_train.details.rds")
# saveRDS(S90_FH22_train.details, file = "/Users/davide/R projects/PALFilterApproachDMM/data/S90_FH22_train.details.rds")
# saveRDS(PAL_times22, file = "/Users/davide/R projects/PALFilterApproachDMM/data/PAL_times22.rds")


GW_FH22_train.details <- readRDS("data/GW_FH22_train.details.rds")
GE_FH22_train.details <- readRDS("data/GE_FH22_train.details.rds")
N30_FH22_train.details <- readRDS("data/N30_FH22_train.details.rds")
N60_FH22_train.details <- readRDS("data/N60_FH22_train.details.rds")
N90_FH22_train.details <- readRDS("data/N90_FH22_train.details.rds")
S30_FH22_train.details <- readRDS("data/S30_FH22_train.details.rds")
S60_FH22_train.details <- readRDS("data/S60_FH22_train.details.rds")
S90_FH22_train.details <- readRDS("data/S90_FH22_train.details.rds")
PAL_times22 <- readRDS("data/PAL_times22.rds")


rstudioapi::navigateToFile("2_TimeDrifts.R")




