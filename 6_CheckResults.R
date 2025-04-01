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

pre_n <- c(original_n_GE, original_n_N30, original_n_N60, original_n_N90, original_n_S30, original_n_S60, original_n_S90)
ext_time <- (pre_n/20)/60 # in hours


post_n <- c(nrow(ToCheck_GE_FH22), nrow(ToCheck_N30_FH22), nrow(ToCheck_N60_FH22), nrow(ToCheck_N90_FH22), nrow(ToCheck_S30_FH22), nrow(ToCheck_S60_FH22), nrow(ToCheck_S90_FH22))
ext_time_post <- (post_n/20)/60 # in hours

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

Results <- data.frame('F-POD ID' = c("GE", "N30", "N60", "N90", "S30", "S60", "S90"),
                   'N. Click trains to filter' = pre_n,
                   'estimated time to filter' = ext_time,
                   'N. Click trains after filtering' = post_n,
                   'estimated time to filter after' = ext_time_post,
                   'PAL signals visually detected' = after_check,
                   time_saved = time_saved,
                   hours_saved = hours_saved
)
