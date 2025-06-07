###########################################################-
# Objective: Create DP10M datasets (both before & after the filter)
# Author: Davide Bittelli
# Date Modified: 25.05.2025
###########################################################-

library(dplyr)
library(lubridate)
library(tidyr)

### GE
{
  GE_FH22_DP10M_before <- GE_FH22_train.details %>%
    # 1) Bin into 10-min blocks
    mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
    # 2) Count how many obs per block
    count(Time_Start, name = "n") %>%
    # 3) Expand to a continuous seq of 10-min blocks
    complete(
      Time_Start = seq(
        floor_date(min(GE_FH22_train.details$Time), "10 minutes"),
        floor_date(max(GE_FH22_train.details$Time), "10 minutes"),
        by = "10 mins"
      ),
      fill = list(n = 0)
    ) %>%
    # 4) Build the final columns
    transmute(
      Time_Start,
      Time_End = Time_Start + minutes(10) - seconds(1),
      DP10M     = as.integer(n > 0)
    )
  sum(GE_FH22_DP10M_before$DP10M)
  
  GE_FH22_DP10M_after <- GE_FH22_filt_train.details %>%
    # 1) Bin into 10-min blocks
    mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
    # 2) Count how many obs per block
    count(Time_Start, name = "n") %>%
    # 3) Expand to a continuous seq of 10-min blocks
    complete(
      Time_Start = seq(
        floor_date(min(GE_FH22_filt_train.details$Time), "10 minutes"),
        floor_date(max(GE_FH22_filt_train.details$Time), "10 minutes"),
        by = "10 mins"
      ),
      fill = list(n = 0)
    ) %>%
    # 4) Build the final columns
    transmute(
      Time_Start,
      Time_End = Time_Start + minutes(10) - seconds(1),
      DP10M     = as.integer(n > 0)
    )
  sum(GE_FH22_DP10M_after$DP10M)
}

### N30
{
N30_FH22_DP10M_before <- N30_FH22_train.details %>%
  # 1) Bin into 10-min blocks
  mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
  # 2) Count how many obs per block
  count(Time_Start, name = "n") %>%
  # 3) Expand to a continuous seq of 10-min blocks
  complete(
    Time_Start = seq(
      floor_date(min(N30_FH22_train.details$Time), "10 minutes"),
      floor_date(max(N30_FH22_train.details$Time), "10 minutes"),
      by = "10 mins"
    ),
    fill = list(n = 0)
  ) %>%
  # 4) Build the final columns
  transmute(
    Time_Start,
    Time_End = Time_Start + minutes(10) - seconds(1),
    DP10M     = as.integer(n > 0)
  )
sum(N30_FH22_DP10M_before$DP10M)

N30_FH22_DP10M_after <- N30_FH22_filt_train.details %>%
  # 1) Bin into 10-min blocks
  mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
  # 2) Count how many obs per block
  count(Time_Start, name = "n") %>%
  # 3) Expand to a continuous seq of 10-min blocks
  complete(
    Time_Start = seq(
      floor_date(min(N30_FH22_filt_train.details$Time), "10 minutes"),
      floor_date(max(N30_FH22_filt_train.details$Time), "10 minutes"),
      by = "10 mins"
    ),
    fill = list(n = 0)
  ) %>%
  # 4) Build the final columns
  transmute(
    Time_Start,
    Time_End = Time_Start + minutes(10) - seconds(1),
    DP10M     = as.integer(n > 0)
  )
sum(N30_FH22_DP10M_after$DP10M)
}

### N60
{
  N60_FH22_DP10M_before <- N60_FH22_train.details %>%
    # 1) Bin into 10-min blocks
    mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
    # 2) Count how many obs per block
    count(Time_Start, name = "n") %>%
    # 3) Expand to a continuous seq of 10-min blocks
    complete(
      Time_Start = seq(
        floor_date(min(N60_FH22_train.details$Time), "10 minutes"),
        floor_date(max(N60_FH22_train.details$Time), "10 minutes"),
        by = "10 mins"
      ),
      fill = list(n = 0)
    ) %>%
    # 4) Build the final columns
    transmute(
      Time_Start,
      Time_End = Time_Start + minutes(10) - seconds(1),
      DP10M     = as.integer(n > 0)
    )
  sum(N60_FH22_DP10M_before$DP10M)
  
  N60_FH22_DP10M_after <- N60_FH22_filt_train.details %>%
    # 1) Bin into 10-min blocks
    mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
    # 2) Count how many obs per block
    count(Time_Start, name = "n") %>%
    # 3) Expand to a continuous seq of 10-min blocks
    complete(
      Time_Start = seq(
        floor_date(min(N60_FH22_filt_train.details$Time), "10 minutes"),
        floor_date(max(N60_FH22_filt_train.details$Time), "10 minutes"),
        by = "10 mins"
      ),
      fill = list(n = 0)
    ) %>%
    # 4) Build the final columns
    transmute(
      Time_Start,
      Time_End = Time_Start + minutes(10) - seconds(1),
      DP10M     = as.integer(n > 0)
    )
  sum(N60_FH22_DP10M_after$DP10M)
}

### N90
{
  N90_FH22_DP10M_before <- N90_FH22_train.details %>%
    # 1) Bin into 10-min blocks
    mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
    # 2) Count how many obs per block
    count(Time_Start, name = "n") %>%
    # 3) Expand to a continuous seq of 10-min blocks
    complete(
      Time_Start = seq(
        floor_date(min(N90_FH22_train.details$Time), "10 minutes"),
        floor_date(max(N90_FH22_train.details$Time), "10 minutes"),
        by = "10 mins"
      ),
      fill = list(n = 0)
    ) %>%
    # 4) Build the final columns
    transmute(
      Time_Start,
      Time_End = Time_Start + minutes(10) - seconds(1),
      DP10M     = as.integer(n > 0)
    )
  sum(N90_FH22_DP10M_before$DP10M)
  
  N90_FH22_DP10M_after <- N90_FH22_filt_train.details %>%
    # 1) Bin into 10-min blocks
    mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
    # 2) Count how many obs per block
    count(Time_Start, name = "n") %>%
    # 3) Expand to a continuous seq of 10-min blocks
    complete(
      Time_Start = seq(
        floor_date(min(N90_FH22_filt_train.details$Time), "10 minutes"),
        floor_date(max(N90_FH22_filt_train.details$Time), "10 minutes"),
        by = "10 mins"
      ),
      fill = list(n = 0)
    ) %>%
    # 4) Build the final columns
    transmute(
      Time_Start,
      Time_End = Time_Start + minutes(10) - seconds(1),
      DP10M     = as.integer(n > 0)
    )
  sum(N90_FH22_DP10M_after$DP10M)
}

### S30
{
  S30_FH22_DP10M_before <- S30_FH22_train.details %>%
    # 1) Bin into 10-min blocks
    mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
    # 2) Count how many obs per block
    count(Time_Start, name = "n") %>%
    # 3) Expand to a continuous seq of 10-min blocks
    complete(
      Time_Start = seq(
        floor_date(min(S30_FH22_train.details$Time), "10 minutes"),
        floor_date(max(S30_FH22_train.details$Time), "10 minutes"),
        by = "10 mins"
      ),
      fill = list(n = 0)
    ) %>%
    # 4) Build the final columns
    transmute(
      Time_Start,
      Time_End = Time_Start + minutes(10) - seconds(1),
      DP10M     = as.integer(n > 0)
    )
  sum(S30_FH22_DP10M_before$DP10M)
  
  S30_FH22_DP10M_after <- S30_FH22_filt_train.details %>%
    # 1) Bin into 10-min blocks
    mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
    # 2) Count how many obs per block
    count(Time_Start, name = "n") %>%
    # 3) Expand to a continuous seq of 10-min blocks
    complete(
      Time_Start = seq(
        floor_date(min(S30_FH22_filt_train.details$Time), "10 minutes"),
        floor_date(max(S30_FH22_filt_train.details$Time), "10 minutes"),
        by = "10 mins"
      ),
      fill = list(n = 0)
    ) %>%
    # 4) Build the final columns
    transmute(
      Time_Start,
      Time_End = Time_Start + minutes(10) - seconds(1),
      DP10M     = as.integer(n > 0)
    )
  sum(S30_FH22_DP10M_after$DP10M)
}

### S60
{
  S60_FH22_DP10M_before <- S60_FH22_train.details %>%
    # 1) Bin into 10-min blocks
    mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
    # 2) Count how many obs per block
    count(Time_Start, name = "n") %>%
    # 3) Expand to a continuous seq of 10-min blocks
    complete(
      Time_Start = seq(
        floor_date(min(S60_FH22_train.details$Time), "10 minutes"),
        floor_date(max(S60_FH22_train.details$Time), "10 minutes"),
        by = "10 mins"
      ),
      fill = list(n = 0)
    ) %>%
    # 4) Build the final columns
    transmute(
      Time_Start,
      Time_End = Time_Start + minutes(10) - seconds(1),
      DP10M     = as.integer(n > 0)
    )
  sum(S60_FH22_DP10M_before$DP10M)
  
  S60_FH22_DP10M_after <- S60_FH22_filt_train.details %>%
    # 1) Bin into 10-min blocks
    mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
    # 2) Count how many obs per block
    count(Time_Start, name = "n") %>%
    # 3) Expand to a continuous seq of 10-min blocks
    complete(
      Time_Start = seq(
        floor_date(min(S60_FH22_filt_train.details$Time), "10 minutes"),
        floor_date(max(S60_FH22_filt_train.details$Time), "10 minutes"),
        by = "10 mins"
      ),
      fill = list(n = 0)
    ) %>%
    # 4) Build the final columns
    transmute(
      Time_Start,
      Time_End = Time_Start + minutes(10) - seconds(1),
      DP10M     = as.integer(n > 0)
    )
  sum(S60_FH22_DP10M_after$DP10M)
}

### S90
{
  S90_FH22_DP10M_before <- S90_FH22_train.details %>%
    # 1) Bin into 10-min blocks
    mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
    # 2) Count how many obs per block
    count(Time_Start, name = "n") %>%
    # 3) Expand to a continuous seq of 10-min blocks
    complete(
      Time_Start = seq(
        floor_date(min(S90_FH22_train.details$Time), "10 minutes"),
        floor_date(max(S90_FH22_train.details$Time), "10 minutes"),
        by = "10 mins"
      ),
      fill = list(n = 0)
    ) %>%
    # 4) Build the final columns
    transmute(
      Time_Start,
      Time_End = Time_Start + minutes(10) - seconds(1),
      DP10M     = as.integer(n > 0)
    )
  sum(S90_FH22_DP10M_before$DP10M)
  
  S90_FH22_DP10M_after <- S90_FH22_filt_train.details %>%
    # 1) Bin into 10-min blocks
    mutate(Time_Start = floor_date(Time, "10 minutes")) %>%
    # 2) Count how many obs per block
    count(Time_Start, name = "n") %>%
    # 3) Expand to a continuous seq of 10-min blocks
    complete(
      Time_Start = seq(
        floor_date(min(S90_FH22_filt_train.details$Time), "10 minutes"),
        floor_date(max(S90_FH22_filt_train.details$Time), "10 minutes"),
        by = "10 mins"
      ),
      fill = list(n = 0)
    ) %>%
    # 4) Build the final columns
    transmute(
      Time_Start,
      Time_End = Time_Start + minutes(10) - seconds(1),
      DP10M     = as.integer(n > 0)
    )
  sum(S90_FH22_DP10M_after$DP10M)
}

rstudioapi::navigateToFile("7_CheckResults.R")