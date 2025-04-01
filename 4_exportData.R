###########################################################-
# Objective: Export data to (manually) filter (FH22)
# Author(s): Davide Bittelli
# Date Modified: 04.03.2025
###########################################################-

library(dplyr)
library(lubridate)

#-----------------------------------------------------------------------------------------------------------------------------#
#----------------- elements to search and replace, when copyng-pasting for another campaign: FH22,  PAL_times22 --------------#
#---------------------------- We Export the filtered data to delete missed PAL signals ---------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#

# In script '3_filter_PALsignals.R' we created the following datasets:

# N30_FH22_filt_train.details
# N60_FH22_filt_train.details
# N90_FH22_filt_train.details
# S30_FH22_filt_train.details
# S60_FH22_filt_train.details
# S90_FH22_filt_train.details

# Here we add a binary variable called "HP" (Harbor Porpoise).
# In the times where PAL is OFF we set HP == 1.
# In the times where PAL is ON we set HP == 0.

# Then we export the observations JUST for the time when the PAL was ON and we check them manually by going through each .FP3 file in FPOD.EXE
# In script 6_ we merge the results with the other observations.


## N30
N30_FH22_filt_train.details$HP <- ifelse(
  rowSums(mapply(function(start, end) N30_FH22_filt_train.details$Time_adj > start & N30_FH22_filt_train.details$Time_adj < end, PAL_times22$Start, PAL_times22$End)) > 0,
  0, 1
)
ifelse(anyNA(N30_FH22_filt_train.details$HP) == TRUE, print("There are some NA in HP"), print("No NA in HP"))

## N60
N60_FH22_filt_train.details$HP <- ifelse(
  rowSums(mapply(function(start, end) N60_FH22_filt_train.details$Time_adj > start & N60_FH22_filt_train.details$Time_adj < end, PAL_times22$Start, PAL_times22$End)) > 0,
  0, 1
)
ifelse(anyNA(N60_FH22_filt_train.details$HP) == TRUE, print("There are some NA in HP"), print("No NA in HP"))

## N90
N90_FH22_filt_train.details$HP <- ifelse(
  rowSums(mapply(function(start, end) N90_FH22_filt_train.details$Time_adj > start & N90_FH22_filt_train.details$Time_adj < end, PAL_times22$Start, PAL_times22$End)) > 0,
  0, 1
)
ifelse(anyNA(N90_FH22_filt_train.details$HP) == TRUE, print("There are some NA in HP"), print("No NA in HP"))

## S30
S30_FH22_filt_train.details$HP <- ifelse(
  rowSums(mapply(function(start, end) S30_FH22_filt_train.details$Time_adj > start & S30_FH22_filt_train.details$Time_adj < end, PAL_times22$Start, PAL_times22$End)) > 0,
  0, 1
)
ifelse(anyNA(S30_FH22_filt_train.details$HP) == TRUE, print("There are some NA in HP"), print("No NA in HP"))

## S60
S60_FH22_filt_train.details$HP <- ifelse(
  rowSums(mapply(function(start, end) S60_FH22_filt_train.details$Time_adj > start & S60_FH22_filt_train.details$Time_adj < end, PAL_times22$Start, PAL_times22$End)) > 0,
  0, 1
)
ifelse(anyNA(S60_FH22_filt_train.details$HP) == TRUE, print("There are some NA in HP"), print("No NA in HP"))

## S90
S90_FH22_filt_train.details$HP <- ifelse(
  rowSums(mapply(function(start, end) S90_FH22_filt_train.details$Time_adj > start & S90_FH22_filt_train.details$Time_adj < end, PAL_times22$Start, PAL_times22$End)) > 0,
  0, 1
)
ifelse(anyNA(S90_FH22_filt_train.details$HP) == TRUE, print("There are some NA in HP"), print("No NA in HP"))



## GE
GE_FH22_filt_train.details$HP <- ifelse(
  rowSums(mapply(function(start, end) GE_FH22_filt_train.details$Time_adj > start & GE_FH22_filt_train.details$Time_adj < end, PAL_times22$Start, PAL_times22$End)) > 0,
  0, 1
)
ifelse(anyNA(GE_FH22_filt_train.details$HP) == TRUE, print("There are some NA in HP"), print("No NA in HP"))

# add 'Time_UTC' for checking them in a better way in FPOD.exe (you'll remove this variable afterwards)
N30_FH22_filt_train.details$Time_UTC <- N30_FH22_filt_train.details$Time + (N30_FH22_filt_train.details$Start*5/1000000) - hours(2)
N60_FH22_filt_train.details$Time_UTC <- N60_FH22_filt_train.details$Time + (N60_FH22_filt_train.details$Start*5/1000000) - hours(2)
N90_FH22_filt_train.details$Time_UTC <- N90_FH22_filt_train.details$Time + (N90_FH22_filt_train.details$Start*5/1000000) - hours(2)
S30_FH22_filt_train.details$Time_UTC <- S30_FH22_filt_train.details$Time + (S30_FH22_filt_train.details$Start*5/1000000) - hours(2)
S60_FH22_filt_train.details$Time_UTC <- S60_FH22_filt_train.details$Time + (S60_FH22_filt_train.details$Start*5/1000000) - hours(2)
S90_FH22_filt_train.details$Time_UTC <- S90_FH22_filt_train.details$Time + (S90_FH22_filt_train.details$Start*5/1000000) - hours(2)
GE_FH22_filt_train.details$Time_UTC <- GE_FH22_filt_train.details$Time + (GE_FH22_filt_train.details$Start*5/1000000) - hours(2)


# Filter obs where HP == 0 so we can check them on FPOD.EXE
ToCheck_N30_FH22 <- N30_FH22_filt_train.details %>% filter(HP == 0)
ToCheck_N60_FH22 <- N60_FH22_filt_train.details %>% filter(HP == 0)
ToCheck_N90_FH22 <- N90_FH22_filt_train.details %>% filter(HP == 0)
ToCheck_S30_FH22 <- S30_FH22_filt_train.details %>% filter(HP == 0)
ToCheck_S60_FH22 <- S60_FH22_filt_train.details %>% filter(HP == 0)
ToCheck_S90_FH22 <- S90_FH22_filt_train.details %>% filter(HP == 0)
ToCheck_GE_FH22 <- GE_FH22_filt_train.details %>% filter(HP == 0)






# Export them (this won't be touched)
write.table(ToCheck_N30_FH22, "exports/ToCheck/ToCheck_N30_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)
write.table(ToCheck_N60_FH22, "exports/ToCheck/ToCheck_N60_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)
write.table(ToCheck_N90_FH22, "exports/ToCheck/ToCheck_N90_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)
write.table(ToCheck_S30_FH22, "exports/ToCheck/ToCheck_S30_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)
write.table(ToCheck_S60_FH22, "exports/ToCheck/ToCheck_S60_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)
write.table(ToCheck_S90_FH22, "exports/ToCheck/ToCheck_S90_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)
write.table(ToCheck_GE_FH22, "exports/ToCheck/ToCheck_GE_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)
# Export a copy (this will be the one to visually check)
write.table(ToCheck_N30_FH22, "exports/Checked/Checked_N30_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)
write.table(ToCheck_N60_FH22, "exports/Checked/Checked_N60_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)
write.table(ToCheck_N90_FH22, "exports/Checked/Checked_N90_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)
write.table(ToCheck_S30_FH22, "exports/Checked/Checked_S30_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)
write.table(ToCheck_S60_FH22, "exports/Checked/Checked_S60_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)
write.table(ToCheck_S90_FH22, "exports/Checked/Checked_S90_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)
write.table(ToCheck_GE_FH22, "exports/Checked/Checked_GE_FH22.csv", sep = ";", row.names = FALSE, col.names = TRUE)


rstudioapi::navigateToFile("5_MergeData.R")

