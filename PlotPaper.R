# Define the two stations you want to plot
selected_stations <- c("N30", "N60", "N90", 
                       "S30", "S60", "S90")
# Set up the plotting area: 1 row, 2 columns
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))
# Loop through the selected stations
for (st in selected_stations) {
  # Extract the y-data for the station
  y_data <- Timedrifts_FH_22[[st]]
  # Dynamically retrieve the corresponding drift model object
  drift_model <- get(paste0(st, "_driftmodel_FH_22"))
  # Choose point color based on station type (customize as needed)
  point_col <- ifelse(grepl("^S", st), "blue",
                      ifelse(grepl("^N", st), "darkgreen", "purple"))
  # Plot the data
  plot(Timedrifts_FH_22$Day, y_data,
       main = st, xlab = "Day", ylab = "Time Drift (seconds)",
       ylim = c(-8,8),
       pch = 16, col = point_col, cex = 1.2)
  # Add grid lines for clarity
  grid()
  # Add the regression line from the drift model
  abline(drift_model, col = "red", lwd = 2)
}
