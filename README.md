Cobi fNIRS
================

[![DOI](https://zenodo.org/badge/779680219.svg)](https://zenodo.org/doi/10.5281/zenodo.10912115)

For importing and working with fNIRS data from COBI Studio in R.

## Current status (April 2024)

This package is currently in early development. Use with caution.

## Current features

- Importing fNIRS data from COBI Studio (.nir) into a data frame
- Adding markers to the data frame from the COBI Studio markers file
  (.mrk)
- Creating a summary report of signal quality (weak and saturated
  channels)
- Removing the ambient light signal from the data file
- Removing selected channels from the data file (e.g., channels with
  poor signal quality)
- Creating delta OD (change in optical density) values from the raw
  light intensity data

## Planned features (in progress)

- Converting delta OD values to HbO and HbR values (i.e., oxy data)
  using the modified Beer-Lambert (MBLL) law
- Adding Brodmann areas to the data frame based on the channel locations
- Plotting the data (x = time, y = HbO/HbR values) for each channel
- Plotting the data (showing activation in the brain areas)
- Filtering the data using a low-pass filter
- Applying a SMAR (i.e., sliding window motion artifact rejection)
  filter to the data

## Installation

You can install the development version of cobifnirs from [GitHub]()

``` r

# install.packages("devtools")
devtools::install_github("christopherjwilson/cobifnirs")
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
