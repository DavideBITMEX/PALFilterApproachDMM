###########################################################-
# Objective: Import time-drifts and implement them on the click-trains datasets (using linear models).
#            The time-drifts are calculated in relation to the reference pod (GW)
# Author: Davide Bittelli
# Date Modified: 01.04.2025
###########################################################-

library(lubridate)
library(readr)

# Data preparation
Timedrifts_FH_22 <- read_delim("data/Timedrifts_FH_22.csv",
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

Timedrifts_FH_22$Day <- yday(as.POSIXct(Timedrifts_FH_22$Datum, format = "%d.%m.%Y"))


# Linear Models
N30_driftmodel_FH_22 <- lm(N30 ~ Day, data = Timedrifts_FH_22)
N60_driftmodel_FH_22 <- lm(N60 ~ Day, data = Timedrifts_FH_22)
N90_driftmodel_FH_22 <- lm(N90 ~ Day, data = Timedrifts_FH_22[1:3,])

S30_driftmodel_FH_22 <- lm(S30 ~ Day, data = Timedrifts_FH_22[1:3,])
S60_driftmodel_FH_22 <- lm(S60 ~ Day, data = Timedrifts_FH_22)
S90_driftmodel_FH_22 <- lm(S90 ~ Day, data = Timedrifts_FH_22)

GE_1_driftmodel_FH_22 <- lm(GE ~ Day, data = Timedrifts_FH_22[1:2,])
GE_2_driftmodel_FH_22 <- lm(GE ~ Day, data = Timedrifts_FH_22[3:4,])


plot(Timedrifts_FH_22$Day, Timedrifts_FH_22$N30)
abline(N30_driftmodel_FH_22)



# Prediction

# w <- data.frame(Day = c(1,10,100,200),
#                 Test = c(1,10,100,204))
# w$predict.1 <- predict(N30_driftmodel_FH_22, newdata = w)
# w$predict.2 <- predict(N30_driftmodel_FH_22, newdata = data.frame(Day = w$Test))
# w

N30_FH22_train.details$Day <- yday(N30_FH22_train.details$Time)
N30_FH22_train.details$Timedrift <- predict(N30_driftmodel_FH_22, newdata = N30_FH22_train.details)
# we create Time_adj by adding/removing the timedrift and adding the seconds ('Start'*5/1000000)
N30_FH22_train.details$Time_adj <- N30_FH22_train.details$Time + as.difftime(N30_FH22_train.details$Timedrift, units = "secs") + ((N30_FH22_train.details$Start * 5)/1000000)

N60_FH22_train.details$Day <- yday(N60_FH22_train.details$Time)
N60_FH22_train.details$Timedrift <- predict(N60_driftmodel_FH_22, newdata = N60_FH22_train.details)
# we create Time_adj by adding/removing the timedrift and adding the seconds ('Start'*5/1000000)
N60_FH22_train.details$Time_adj <- N60_FH22_train.details$Time + as.difftime(N60_FH22_train.details$Timedrift, units = "secs") + ((N60_FH22_train.details$Start * 5)/1000000)

N90_FH22_train.details$Day <- yday(N90_FH22_train.details$Time)
N90_FH22_train.details$Timedrift <- predict(N90_driftmodel_FH_22, newdata = N90_FH22_train.details)
# manual adjustment because N90 was split into 2 files
N90_FH22_train.details$Timedrift[N90_FH22_train.details$File == "N90--_20220825_000B_FPOD6894_u_PAL file0 PART 3d 13h 38m.FP3"] <- Timedrifts_FH_22$N90[4]
# we create Time_adj by adding/removing the timedrift and adding the seconds ('Start'*5/1000000)
N90_FH22_train.details$Time_adj <- N90_FH22_train.details$Time + as.difftime(N90_FH22_train.details$Timedrift, units = "secs") + ((N90_FH22_train.details$Start * 5)/1000000)


S30_FH22_train.details$Day <- yday(S30_FH22_train.details$Time)
S30_FH22_train.details$Timedrift <- predict(S30_driftmodel_FH_22, newdata = S30_FH22_train.details)
# manual adjustment because S30 was split into 2 files
S30_FH22_train.details$Timedrift[S30_FH22_train.details$File == "S30 2023 08 20 FPOD_6887 file0 PART 9d 8h 22m.FP3"] <- Timedrifts_FH_22$S30[4]
# we create Time_adj by adding/removing the timedrift and adding the seconds ('Start'*5/1000000)
S30_FH22_train.details$Time_adj <- S30_FH22_train.details$Time + as.difftime(S30_FH22_train.details$Timedrift, units = "secs") + ((S30_FH22_train.details$Start * 5)/1000000)

S60_FH22_train.details$Day <- yday(S60_FH22_train.details$Time)
S60_FH22_train.details$Timedrift <- predict(S60_driftmodel_FH_22, newdata = S60_FH22_train.details)
# we create Time_adj by adding/removing the timedrift and adding the seconds ('Start'*5/1000000)
S60_FH22_train.details$Time_adj <- S60_FH22_train.details$Time + as.difftime(S60_FH22_train.details$Timedrift, units = "secs") + ((S60_FH22_train.details$Start * 5)/1000000)

S90_FH22_train.details$Day <- yday(S90_FH22_train.details$Time)
S90_FH22_train.details$Timedrift <- predict(S90_driftmodel_FH_22, newdata = S90_FH22_train.details)
# we create Time_adj by adding/removing the timedrift and adding the seconds ('Start'*5/1000000)
S90_FH22_train.details$Time_adj <- S90_FH22_train.details$Time + as.difftime(S90_FH22_train.details$Timedrift, units = "secs") + ((S90_FH22_train.details$Start * 5)/1000000)

GE_FH22_train.details$Day <- yday(GE_FH22_train.details$Time)
GE_FH22_train.details$Timedrift <- predict(GE_1_driftmodel_FH_22, newdata = GE_FH22_train.details)
GE_FH22_train.details$Timedrift[GE_FH22_train.details$File == "GEast_20220728_000C_FPOD6893_u_PAL file0 PART 31d 5h 6m.FP3"] <- predict(GE_2_driftmodel_FH_22, newdata = GE_FH22_train.details[GE_FH22_train.details$File == "GEast_20220728_000C_FPOD6893_u_PAL file0 PART 31d 5h 6m.FP3",])
# we create Time_adj by adding/removing the timedrift and adding the seconds ('Start'*5/1000000)
GE_FH22_train.details$Time_adj <- GE_FH22_train.details$Time + as.difftime(GE_FH22_train.details$Timedrift, units = "secs") + ((GE_FH22_train.details$Start * 5)/1000000)



rstudioapi::navigateToFile("3_filterPALsignals.R")

