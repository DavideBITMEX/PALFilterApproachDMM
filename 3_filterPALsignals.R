###########################################################-
# Objective: Build & apply filter based on baseline FPOD (FH22)
# Author(s): Davide Bittelli & Tim Taugnitz
# Date Modified: 04.03.2025
###########################################################-

library(dplyr)
library(fuzzyjoin)
library(lubridate)
library(ggplot2)

#-----------------------------------------------------------------------------------------------------------------------------#
#----------------- elements to search and replace, when copyng-pasting for another campaign: FH22,  PAL_times22 --------------#
#----------------------------------- Removing PAL signals + Creating Filtered Plots ------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#

# Here we remove the PAL signals
# Our reference FPOD is Gillnet West (GW)
# We filter the following PODs: N30, N60, N90, S30, S60, S90
# by removing the observations from their train.details files that occured in GW in the periods when PAL was on
# (between PAL_times22$Start_UTC and PAL_times22$End_UTC)
# we will create -> "N30_FH22_filt_train.details" ecc...

# Once we have them, we will create the other datasets for data visualization for click rate

# Then we will create a copy of the DPM files and call it "N30_FH22_filt_DPM"
# make sure to understand if the variable ChunkEnd is in UTC or CET
# This variable doesn't contain info regarding seconds, just till minutes
# we remove the observations that have the same Time in GW (rounded by closer minute) and that occured in the periods when PAL was on
# (between PAL_times$Start_UTC and PAL_times$End_UTC)
# Then we create the necessary files for data visualization

# train.details contains "Time_adj" (POSIXct)

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
{
  ### We remove every variable that has $Time_adj = GW_FH22_train.details$Time_UTC (with an error of +- 2 seconds)
  ### and that is between PAL_times22$Start_UTC and PAL_times22$End_UTC
  # (This is all done in Time Zone UTC because all the train.details file were never turned into CET timezone)
  
  ## GW (This might serve just for visualization purposes)
  # Here we create GW_FH22_filt_train.details and GW_FH22_filt_DPM, but we don't use them for filtering the other pods
  # For that we use the original datasets (GW_FH22_train.details, GW_FH22_DPM) since they contain also the PAL signals
  {
    ## filt train.details
    GW_FH22_filt_train.details <- GW_FH22_train.details %>%
      filter(
        !sapply(Time_CET, function(t) {
          # Controlla se Time_adj è all'interno degli intervalli di PAL_times22
          any(t >= PAL_times22$Start & t <= PAL_times22$End)
        })
      )

  }
  
  ## N30 done
  {
    # Rimuovi decimal_date e lavora direttamente con POSIXct
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(N30_FH22_train.details, Boundaries, By, multiple = "any")
    
    N30_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # Primo filtro: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Secondo filtro: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% # add '& Qn == "High" ' if needed
      ungroup()
    
    
    # remove rows lower and upper from N30_FH22_filt_train.details
    N30_FH22_filt_train.details <- N30_FH22_filt_train.details %>%
      select(-lower, -upper)
    remove(temporary)

  }
  
  
  ## N60
  {
    # Rimuovi decimal_date e lavora direttamente con POSIXct
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(N60_FH22_train.details, Boundaries, By, multiple = "any")
    
    N60_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # Primo filtro: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Secondo filtro: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% # add '& Qn == "High" ' if needed
      ungroup()
    
    
    # remove rows lower and upper from N60_FH22_filt_train.details
    N60_FH22_filt_train.details <- N60_FH22_filt_train.details %>%
      select(-lower, -upper)
    remove(temporary)

    
  }
  
  ## N90
  {
    # Rimuovi decimal_date e lavora direttamente con POSIXct
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(N90_FH22_train.details, Boundaries, By, multiple = "any")
    
    N90_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # Primo filtro: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Secondo filtro: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% # add '& Qn == "High" ' if needed
      ungroup()
    
    
    # remove rows lower and upper from N90_FH22_filt_train.details
    N90_FH22_filt_train.details <- N90_FH22_filt_train.details %>%
      select(-lower, -upper)
    remove(temporary)
   
  }
  
  ## S30
  {
    # Rimuovi decimal_date e lavora direttamente con POSIXct
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(S30_FH22_train.details, Boundaries, By, multiple = "any")
    
    S30_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # Primo filtro: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Secondo filtro: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% # add '& Qn == "High" ' if needed
      ungroup()
    
    
    # remove rows lower and upper from S30_FH22_filt_train.details
    S30_FH22_filt_train.details <- S30_FH22_filt_train.details %>%
      select(-lower, -upper)
    remove(temporary)
  }
  
  ## S60
  {
    # Rimuovi decimal_date e lavora direttamente con POSIXct
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(S60_FH22_train.details, Boundaries, By, multiple = "any")
    
    S60_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # Primo filtro: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Secondo filtro: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% # add '& Qn == "High" ' if needed
      ungroup()
    
    
    # remove rows lower and upper from S60_FH22_filt_train.details
    S60_FH22_filt_train.details <- S60_FH22_filt_train.details %>%
      select(-lower, -upper)
    remove(temporary)
    
  }
  
  ## S90
  {
    # Rimuovi decimal_date e lavora direttamente con POSIXct
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(S90_FH22_train.details, Boundaries, By, multiple = "any")
    
    S90_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # Primo filtro: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Secondo filtro: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% # add '& Qn == "High" ' if needed
      ungroup()
    
    
    # remove rows lower and upper from S90_FH22_filt_train.details
    S90_FH22_filt_train.details <- S90_FH22_filt_train.details %>%
      select(-lower, -upper)
    remove(temporary)
    
  }
  
  ## GE
  {
    # Rimuovi decimal_date e lavora direttamente con POSIXct
    By <- join_by(between(Time_adj, lower, upper, bounds = "[]"))
    temporary <- left_join(GE_FH22_train.details, Boundaries, By, multiple = "any")
    
    GE_FH22_filt_train.details <- temporary %>%
      rowwise() %>%
      # Primo filtro: keeping obs with Time_adj outside of Start and End in PAL_times22
      filter(!any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End) |
               # Secondo filtro: keep obs with NA in 'lower' within Start-End intervals
               (is.na(lower) & any(Time_adj >= PAL_times22$Start & Time_adj <= PAL_times22$End))) %>% # add '& Qn == "High" ' if needed
      ungroup()
    
  }
  
  
  }


rstudioapi::navigateToFile("4_exportData.R")

