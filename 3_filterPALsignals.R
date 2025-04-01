###########################################################-
# Objective: Detect PAL signals from Reference F-POD (GW), apply the +- 3 seconds range 
#            and then filter out the PAL signals from the other F-PODS.
# Author: Davide Bittelli & Tim Taugnitz
# Date Modified: 01.04.2025
###########################################################-

library(dplyr)
library(fuzzyjoin)
library(lubridate)
library(ggplot2)

# Here we remove the PAL signals
# Our reference FPOD is Gillnet West (GW)
# We filter the following F-PODs: N30, N60, N90, S30, S60, S90 and GE
# by removing the observations from their train.details files that occured in GW in the periods when PAL was ON
# (between PAL_times22$Start_UTC and PAL_times22$End_UTC)
# we will create -> "N30_FH22_filt_train.details" ecc...

# First we create $Time_UTC in GW_FH22_train.details, containing the seconds
GW_FH22_train.details$Time_UTC <- with_tz(GW_FH22_train.details$Time, tzone = "UTC") + ((GW_FH22_train.details$Start / 1e6)*5) #! GW_FH22_train.details$Time_UTC <- GW_FH22_train.details$Time + ((GW_FH22_train.details$Start / 1e6)*5)
GW_FH22_train.details$Time_CET <- GW_FH22_train.details$Time + ((GW_FH22_train.details$Start / 1e6)*5) #! GW_FH22_train.details$Time_CET <- GW_FH22_train.details$Time_UTC + 2*60*60 # before was without with_tz() and we multiplied (+ 2*3600)

# Modifica Boundaries per mantenere il formato POSIXct
Boundaries <- GW_FH22_train.details %>%
  mutate(
    Time_CET = Time_CET,
    lower = Time_CET - 3,
    upper = Time_CET + 3
  ) %>%
  select(Time_CET, lower, upper)


### CREATING DPM and TRAIN.DETAILS files FILTERED (Done)

  ### We remove every variable that has $Time_adj = GW_FH22_train.details$Time_UTC (with an error of +- 3 seconds)
  ### and that is between PAL_times22$Start_UTC and PAL_times22$End_UTC
  # (This is all done in Time Zone UTC because all the train.details file were never turned into CET timezone)
  
  ## GW 
  # Here we create GW_FH22_filt_train.details, but we don't it them for filtering the other F-PODs
  {
    GW_FH22_filt_train.details <- GW_FH22_train.details %>%
      filter(
        !sapply(Time_CET, function(t) {
          # Check if Time_adj is within PAL_times22 intervals
          any(t >= PAL_times22$Start & t <= PAL_times22$End)
        })
      )

  }
  
  ## N30 
  {
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(N30_FH22_train.details, Boundaries, By, multiple = "any")
    
    N30_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # First filter: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Second filter: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% 
      ungroup()
    
    N30_FH22_filt_train.details <- N30_FH22_filt_train.details %>%
      select(-lower, -upper)
    remove(temporary)

  }
  
  ## N60
  {
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(N60_FH22_train.details, Boundaries, By, multiple = "any")
    
    N60_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # First filter: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Second filter: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% 
      ungroup()
    
    N60_FH22_filt_train.details <- N60_FH22_filt_train.details %>%
      select(-lower, -upper)
    remove(temporary)
  }
  
  ## N90
  {
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(N90_FH22_train.details, Boundaries, By, multiple = "any")
    
    N90_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # First filter: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Second filter: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% 
      ungroup()
    
    N90_FH22_filt_train.details <- N90_FH22_filt_train.details %>%
      select(-lower, -upper)
    remove(temporary)
  }
  
  ## S30
  {
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(S30_FH22_train.details, Boundaries, By, multiple = "any")
    
    S30_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # First filter: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Second filter: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% 
      ungroup()
    
    S30_FH22_filt_train.details <- S30_FH22_filt_train.details %>%
      select(-lower, -upper)
    remove(temporary)
  }
  
  ## S60
  {
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(S60_FH22_train.details, Boundaries, By, multiple = "any")
    
    S60_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # First filter: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Second filter: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% 
      ungroup()
    
    S60_FH22_filt_train.details <- S60_FH22_filt_train.details %>%
      select(-lower, -upper)
    remove(temporary)
  }
  
  ## S90
  {
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(S90_FH22_train.details, Boundaries, By, multiple = "any")
    
    S90_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # First filter: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Second filter: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% 
      ungroup()
    
    S90_FH22_filt_train.details <- S90_FH22_filt_train.details %>%
      select(-lower, -upper)
    remove(temporary)
  }
  
  ## GE
  {
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(GE_FH22_train.details, Boundaries, By, multiple = "any")
    
    GE_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # First filter: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Second filter: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% 
      ungroup()
  }
  


rstudioapi::navigateToFile("4_exportData.R")

