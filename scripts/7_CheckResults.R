###########################################################-
# Objective: Create a table that summarizes the results
# Author: Davide Bittelli
# Date Modified: 01.04.2025
###########################################################-

original_n_GE <- sum(sapply(GE_FH22_train.details$Time_adj, function(t) {
  any(t >= PAL_times22$Start & t <= PAL_times22$End)
}))
original_n_N30 <- sum(sapply(N30_FH22_train.details$Time_adj, function(t) {
  any(t >= PAL_times22$Start & t <= PAL_times22$End)
}))
original_n_N60 <- sum(sapply(N60_FH22_train.details$Time_adj, function(t) {
  any(t >= PAL_times22$Start & t <= PAL_times22$End)
}))
original_n_N90 <- sum(sapply(N90_FH22_train.details$Time_adj, function(t) {
  any(t >= PAL_times22$Start & t <= PAL_times22$End)
}))
original_n_S30 <- sum(sapply(S30_FH22_train.details$Time_adj, function(t) {
  any(t >= PAL_times22$Start & t <= PAL_times22$End)
}))
original_n_S60 <- sum(sapply(S60_FH22_train.details$Time_adj, function(t) {
  any(t >= PAL_times22$Start & t <= PAL_times22$End)
}))
original_n_S90 <- sum(sapply(S90_FH22_train.details$Time_adj, function(t) {
  any(t >= PAL_times22$Start & t <= PAL_times22$End)
}))

# We hypothesize that a trained person is able to inspect 20 signals in 1 minute
pre_n <- c(original_n_GE, original_n_N30, original_n_N60, original_n_N90, original_n_S30, original_n_S60, original_n_S90)
ext_time <- (pre_n/20)/60 # in hours

post_n <- c(nrow(ToCheck_GE_FH22), nrow(ToCheck_N30_FH22), nrow(ToCheck_N60_FH22), nrow(ToCheck_N90_FH22), nrow(ToCheck_S30_FH22), nrow(ToCheck_S60_FH22), nrow(ToCheck_S90_FH22))
ext_time_post <- (post_n/20)/60 # in hours

# PAL signals detected during the visual check from trained personnel 
GE_after <- sum(Checked_GE_FH22$HP == 0)
N30_after <- sum(Checked_N30_FH22$HP == 0)
N60_after <- sum(Checked_N60_FH22$HP == 0)
N90_after <- sum(Checked_N90_FH22$HP == 0)
S30_after <- sum(Checked_S30_FH22$HP == 0)
S60_after <- sum(Checked_S60_FH22$HP == 0)
S90_after <- sum(Checked_S90_FH22$HP == 0)

after_check <- c(GE_after, N30_after, N60_after, N90_after, S30_after, S60_after, S90_after)
time_saved <- pre_n - post_n
hours_saved <- (time_saved/20) / 60 # in hours


### DP10M (when PAL was ON)
# GE
{
  # before
  GE_DP10M_before <- sum(
    sapply(
      GE_FH22_DP10M_before$Time_Start[GE_FH22_DP10M_before$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
  # after
  GE_DP10M_after <- sum(
    sapply(
      GE_FH22_DP10M_after$Time_Start[GE_FH22_DP10M_after$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
}
# N30
{
  # before
  N30_DP10M_before <- sum(
    sapply(
      N30_FH22_DP10M_before$Time_Start[N30_FH22_DP10M_before$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
  # after
  N30_DP10M_after <- sum(
    sapply(
      N30_FH22_DP10M_after$Time_Start[N30_FH22_DP10M_after$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
}
# N60
{
  # before
  N60_DP10M_before <- sum(
    sapply(
      N60_FH22_DP10M_before$Time_Start[N60_FH22_DP10M_before$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
  # after
  N60_DP10M_after <- sum(
    sapply(
      N60_FH22_DP10M_after$Time_Start[N60_FH22_DP10M_after$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
}
# N90
{
  # before
  N90_DP10M_before <- sum(
    sapply(
      N90_FH22_DP10M_before$Time_Start[N90_FH22_DP10M_before$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
  # after
  N90_DP10M_after <- sum(
    sapply(
      N90_FH22_DP10M_after$Time_Start[N90_FH22_DP10M_after$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
}
# S30
{
  # before
  S30_DP10M_before <- sum(
    sapply(
      S30_FH22_DP10M_before$Time_Start[S30_FH22_DP10M_before$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
  # after
  S30_DP10M_after <- sum(
    sapply(
      S30_FH22_DP10M_after$Time_Start[S30_FH22_DP10M_after$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
}
# S60
{
  # before
  S60_DP10M_before <- sum(
    sapply(
      S60_FH22_DP10M_before$Time_Start[S60_FH22_DP10M_before$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
  # after
  S60_DP10M_after <- sum(
    sapply(
      S60_FH22_DP10M_after$Time_Start[S60_FH22_DP10M_after$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
}
# S90
{
  # before
  S90_DP10M_before <- sum(
    sapply(
      S90_FH22_DP10M_before$Time_Start[S90_FH22_DP10M_before$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
  # after
  S90_DP10M_after <- sum(
    sapply(
      S90_FH22_DP10M_after$Time_Start[S90_FH22_DP10M_after$DP10M == 1],
      function(t) any(
        t %within% interval(PAL_times22$Start, PAL_times22$End)
      )
    )
  )
}

DP10M_before <- c(GE_DP10M_before,
                  N30_DP10M_before,
                  N60_DP10M_before,
                  N90_DP10M_before,
                  S30_DP10M_before,
                  S60_DP10M_before,
                  S90_DP10M_before)

DP10M_after <- c(GE_DP10M_after,
                 N30_DP10M_after,
                 N60_DP10M_after,
                 N90_DP10M_after,
                 S30_DP10M_after,
                 S60_DP10M_after,
                 S90_DP10M_after)

Results <- data.frame('F-POD ID' = c("GE", "N30", "N60", "N90", "S30", "S60", "S90"),
                   'N. Click trains to filter' = pre_n,
                   'DP10M before' = DP10M_before,
                   'estimated time to filter' = ext_time,
                   'N. Click trains after filtering' = post_n,
                   'DP10M after' = DP10M_after,
                   'estimated time to filter after' = ext_time_post,
                   'PAL signals visually detected' = after_check,
                   time_saved = time_saved,
                   hours_saved = hours_saved
)
