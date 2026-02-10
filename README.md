Cobi fNIRS
================

[![DOI](https://zenodo.org/badge/779680219.svg)](https://zenodo.org/doi/10.5281/zenodo.10912115)

For importing and working with fNIRS data from COBI Studio in R.

## Current Status: Beta (v0.1.0)
**Date:** February 2026

## Current Features
* **Import:** Robustly handles COBI `.nir` files (including ambient channel isolation).
* **Preprocessing:**
  * **Artifact Correction:** IQR-based sliding window motion artifact rejection (SMAR).
  * **Filtering:** Zero-phase Butterworth low-pass filter.
  * **Quality Check:** Signal quality reports (weak/saturated channels).
* **Concentration:** Modified Beer-Lambert Law (MBLL) conversion to HbO/HbR ($\mu M$).
* **Analysis:**
  * **GLM:** General Linear Model with stick functions and canonical HRF.
  * **Statistics:** Beta extraction and condition comparisons.

## Installation

You can install the development version of cobifnirs from [GitHub]()

``` r

# install.packages("devtools")
devtools::install_github("christopherjwilson/cobifnirs")
```

## Quick Start: Analysis Pipeline

```r
# 1. Import & Preprocess
data <- import_nirs("test.nir") %>%
  add_markers() %>%
  remove_ambient() %>%
  apply_smar(iqr_multiplier = 1.5) %>%  # Artifact Correction
  apply_lowpass(cutoff = 0.1) %>%       # Filtering
  create_delta_od()

# 2. Calculate Concentration (Uses defaults: Gratzer, DPF=6)
oxy_data <- apply_mbll(data)

# 3. Run GLM Statistics
# Define which markers represent trial starts
conditions <- c("Start_Trial", "Condition_A")
glm_stats <- calculate_glm(oxy_data, conditions = conditions)

print(glm_stats)
```

## Importing .nir files into R

``` r
library(cobifnirs)
library(dplyr)

# Import a single .nir file

nirData <- import_nirs("path/to/your/data.nir")

# Import multiple .nir files
# path should be the folder where the file is, ending in /

myPath <- "C:.../fnirs_data/"

#list all of the nirs files in a directory

nirsFiles <- list.files(pattern = ".nir$", recursive = TRUE, path = myPath)

# import all of the nirs files

nirsData <- lapply(nirsFiles, import_nirs, folder = myPath)

# base the number of participants on the number of files in the directory
# this is useful for later analysis

nparticipants <- length(nirsFiles)


# add a participant id as the name of each dataframe in the data list

names(nirsData) <- c(paste("p",pids, sep = ""))

# add the participant id column to each of the dataframes in the data list,
# so we can identify each dataset if they are combined later

nirsData <- mapply(`[<-`, nirsData, 'npid', value = pids, SIMPLIFY = FALSE)

```

## Adding the markers to the data

Important: the markers file must be in the same folder as the .nir file
and have the same name.

See the function documentation for more details on how to use this
function (re: duplicate markers).

``` r

# add markers to a single dataset

nirsData <- add_markers(nirsData)

# add markers to multiple datasets

nirsData <- lapply(nirsData, add_markers)
```

## Creating a summary report of signal quality

``` r

# create a summary report of signal quality for a single dataset

signalSummary <- signal_summary(nirData)

# create a summary report of signal quality for multiple datasets and write to a file

signalSummary <- lapply(nirsData, signal_summary, min=1000, max = 4000)
signalSummary <- bind_rows(signalSummary, .id = "PID")
problem_channels <- signalSummary %>% filter(min_max_signal_status != "ok" || mean_signal_status != "ok")
write.csv(problem_channels, "problem_channels.csv")





```

## Removing problem channels

``` r

# remove selected channels from a single dataset

nirsData <- remove_channels(nirsData, channels = c(1, 2, 3))

# remove selected channels from multiple datasets

## List of problem channels for removal

problem_channels_r <- read_csv(paste0(myPath, "/problem_channels.csv"))

problem_channels_r$remove <- if_else(is.na(problem_channels_r$remove), 0, 1)

problem_channels_r$remove <- as.logical(problem_channels_r$remove)

problem_channels_r <- problem_channels_r %>% filter(problem_channels_r$remove == TRUE) 

## remove the channels (after visual inspection too!)

nirsData <- lapply(nirsData, removeChannels, problem_channels_r)

```
