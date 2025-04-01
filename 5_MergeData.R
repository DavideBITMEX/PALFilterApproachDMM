###########################################################-
# Objective: Import Datasets that were visually checked
# Author: Davide Bittelli
# Date Modified: 01.04.2025
###########################################################-

# We have manually checked the filtered data and labelled the signals (within the PAL times) that were PAL or Porpoises
# Now we import these temporary files and merge them with the original train.details datasets

Checked_N30_FH22 <- read.csv("exports/Checked/Checked_N30_FH22.csv", header = TRUE, sep = ";")
Checked_N60_FH22 <- read.csv("exports/Checked/Checked_N60_FH22.csv", header = TRUE, sep = ";")
Checked_N90_FH22 <- read.csv("exports/Checked/Checked_N90_FH22.csv", header = TRUE, sep = ";")
Checked_S30_FH22 <- read.csv("exports/Checked/Checked_S30_FH22.csv", header = TRUE, sep = ";")
Checked_S60_FH22 <- read.csv("exports/Checked/Checked_S60_FH22.csv", header = TRUE, sep = ";")
Checked_S90_FH22 <- read.csv("exports/Checked/Checked_S90_FH22.csv", header = TRUE, sep = ";")
Checked_GE_FH22 <- read.csv("exports/Checked/Checked_GE_FH22.csv", header = TRUE, sep = ";")


# Within the PAL times we remove the observations (from the temporary datasets we just imported) that have 'HP' == 0
# from train.details
# Then, we substitute the obs in the PAL times (from train.details) with the ones we just got

## N30
{
  trial <- Checked_N30_FH22
  # Merge dataset based on "Line"
  trial2 <- merge(
    N30_FH22_filt_train.details,
    trial[, c("Line", "HP", "File")],
    by = c("Line", "File"),
    all.x = TRUE # mantain all N30_FH22_filt_train.details observations
  )
  
  # Update HP in the merged dataset
  trial2$HP <- ifelse(!is.na(trial2$HP.y), trial2$HP.y, trial2$HP.x)
  # remove duplicated columns
  trial2 <- trial2[, !colnames(trial2) %in% c("HP.x", "HP.y")]
  # remove PAL signals
  trial2 <- subset(trial2, HP != 0)

  N30_FH22_filt_train.details <- trial2
  remove(trial, trial2)
}

## N60
{
  trial <- Checked_N60_FH22
  # Merge dataset based on "Line"
  trial2 <- merge(
    N60_FH22_filt_train.details,
    trial[, c("Line", "HP", "File")],
    by = c("Line", "File"),
    all.x = TRUE # mantain all N60_FH22_filt_train.details observations
  )
  
  # Update HP in the merged dataset
  trial2$HP <- ifelse(!is.na(trial2$HP.y), trial2$HP.y, trial2$HP.x)
  # remove duplicated columns
  trial2 <- trial2[, !colnames(trial2) %in% c("HP.x", "HP.y")]
  # remove PAL signals
  trial2 <- subset(trial2, HP != 0)

  N60_FH22_filt_train.details <- trial2
  remove(trial, trial2)
}

## N90
{
  trial <- Checked_N90_FH22
  # Merge dataset based on "Line"
  trial2 <- merge(
    N90_FH22_filt_train.details,
    trial[, c("Line", "HP", "File")],
    by = c("Line", "File"),
    all.x = TRUE # mantain all N90_FH22_filt_train.details observations
  )
  
  # Update HP in the merged dataset
  trial2$HP <- ifelse(!is.na(trial2$HP.y), trial2$HP.y, trial2$HP.x)
  # remove duplicated columns
  trial2 <- trial2[, !colnames(trial2) %in% c("HP.x", "HP.y")]
  # remove PAL signals
  trial2 <- subset(trial2, HP != 0)

  N90_FH22_filt_train.details <- trial2
  remove(trial, trial2)
}

## S30
{
  trial <- Checked_S30_FH22
  # Merge dataset based on "Line"
  trial2 <- merge(
    S30_FH22_filt_train.details,
    trial[, c("Line", "HP", "File")],
    by = c("Line", "File"),
    all.x = TRUE # mantain all S30_FH22_filt_train.details observations
  )
  
  # Update HP in the merged dataset
  trial2$HP <- ifelse(!is.na(trial2$HP.y), trial2$HP.y, trial2$HP.x)
  # remove duplicated columns
  trial2 <- trial2[, !colnames(trial2) %in% c("HP.x", "HP.y")]
  # remove PAL signals
  trial2 <- subset(trial2, HP != 0)
  sum(trial$HP)
  sum(S30_FH22_filt_train.details$HP)
  sum(trial2$HP)
  
  S30_FH22_filt_train.details <- trial2
  remove(trial, trial2)
}

## S60
{
  trial <- Checked_S60_FH22
  # Merge dataset based on "Line"
  trial2 <- merge(
    S60_FH22_filt_train.details,
    trial[, c("Line", "HP", "File")],
    by = c("Line", "File"),
    all.x = TRUE # mantain all S60_FH22_filt_train.details observations
  )
  
  # Update HP in the merged dataset
  trial2$HP <- ifelse(!is.na(trial2$HP.y), trial2$HP.y, trial2$HP.x)
  # remove duplicated columns
  trial2 <- trial2[, !colnames(trial2) %in% c("HP.x", "HP.y")]
  # remove PAL signals
  trial2 <- subset(trial2, HP != 0)
  sum(trial$HP)
  sum(S60_FH22_filt_train.details$HP)
  sum(trial2$HP)
  
  S60_FH22_filt_train.details <- trial2
  remove(trial, trial2)
}

## S90
{
  trial <- Checked_S90_FH22
  # Merge dataset based on "Line"
  trial2 <- merge(
    S90_FH22_filt_train.details,
    trial[, c("Line", "HP", "File")],
    by = c("Line", "File"),
    all.x = TRUE # mantain all S90_FH22_filt_train.details observations
  )
  
  # Update HP in the merged dataset
  trial2$HP <- ifelse(!is.na(trial2$HP.y), trial2$HP.y, trial2$HP.x)
  # remove duplicated columns
  trial2 <- trial2[, !colnames(trial2) %in% c("HP.x", "HP.y")]
  # remove PAL signals
  trial2 <- subset(trial2, HP != 0)
  sum(trial$HP)
  sum(S90_FH22_filt_train.details$HP)
  sum(trial2$HP)
  
  S90_FH22_filt_train.details <- trial2
  remove(trial, trial2)
}

## GE
{
  trial <- Checked_GE_FH22
  # Merge dataset based on "Line"
  trial2 <- merge(
    GE_FH22_filt_train.details,
    trial[, c("Line", "HP", "File")],
    by = c("Line", "File"),
    all.x = TRUE # mantain all GE_FH22_filt_train.details observations
  )
  
  # Update HP in the merged dataset
  trial2$HP <- ifelse(!is.na(trial2$HP.y), trial2$HP.y, trial2$HP.x)
  # remove duplicated columns
  trial2 <- trial2[, !colnames(trial2) %in% c("HP.x", "HP.y")]
  # remove PAL signals
  trial2 <- subset(trial2, HP != 0)
  sum(trial$HP)
  sum(GE_FH22_filt_train.details$HP)
  sum(trial2$HP)
  
  GE_FH22_filt_train.details <- trial2
  remove(trial, trial2)
}

rstudioapi::navigateToFile("6_CheckResults.R")


