#' @title Import NIRS data
#' @description This function will import a .nir file, identify the baseline values and return a dataframe with the NIRS data in a long format.
#' @param nirFile Path to the .nir file
#' @import janitor
#' @import stringr
#' @import tibble
#' @import dplyr
#' @return A dataframe with the data in a long format with the following columns: t, startTime, fileName, dataRow, optode, freq, nirValue.
#' * t: time in seconds from the start of the recording. You will see that this matches the values of the first column in COBI Studio .nir file.
#' * startTime: the start time of the recording. This is taken from the .nir file.
#' * fileName: the name of the file (full path)
#' * dataRow: the row number of the data as it is in the original .nir file. The first row of the data is the first row after the baseline data ends in the original .nir file.
#' * optode: the optode number (e.g., 1-16)
#' * freq: the frequency (e.g., 730, 850)
#' * nirValue: the value of the nir data (light intensity) for that optode and frequency at that time point. This matches the values in the original .nir file.
#' * baselineValue: the baseline value for that optode and frequency. This is taken from the 10 second baseline period at the start of the recording, and is stored in the original .nir file. You can choose to use this value to calculate the change in optical density, if you wish. Either way, the baseline value is retained in the data frame for reference.
#' @details This function will import a .nir file, identify the baseline values and return a dataframe with the data in a long format.
#' @export
#' @examples
#' nirsData <- import_nirs("path/to/nir/file.nir")



import_nirs <- function(nirFile) {

# read the nirs file
nir1 <- readLines(nirFile,   skipNul = TRUE)

# name the column "value"

nir1 <- tibble::tibble(value = nir1)

# read the marker file
fullMrkPath <- paste(str_sub(nirFile, 1, nchar(nirFile)-4),".mrk", sep = "")  # name the marker file. If it exists, it should have exact same name as nir file

nir1 <- nir1 %>% as_tibble() #make the nirs data a tibble for easier manipulation

start_time <- nir1 %>%
  dplyr::filter(str_detect(.$value, "Start Time:")) %>% substring(17)

start_time <- as.POSIXct(start_time, format = "%b %d %H:%M:%S %Y")

nir1 <- nir1 %>%
  dplyr::mutate(startTime = start_time)


nir1 <- dplyr::mutate(nir1, id = dplyr::row_number()) # this is the row number of the data as it is in the original file. Later we will create "dataRow" to identify the beginning of data collection from the end of baseline, according to COBI studio

# add a column with the file name
nir1 <- nir1 %>% dplyr::mutate(fileName = nirFile)


# Get the baseline data from the file (the default COBI Studio baseline)

# which row does the basline data start on?
bStartRow <-  nir1 %>% dplyr::filter(str_detect(.$value, 'Baseline values')) %>% dplyr::select(id)

# which row does the baseline data end on?
bEndRow <- nir1 %>% dplyr::filter(str_detect(.$value, 'Baseline end')) %>% dplyr::select(id)

# get the baseline data between the start and end rows
baselineNirsValues <- nir1 %>% dplyr::filter(id > bStartRow$id & id < bEndRow$id)

# separate the values into columns by tab
baselineNirsValues <- tidyr::separate(baselineNirsValues, col = value, sep = "\t", into = c("t", "op_1_730","op_1_ambient","op_1_850","op_2_730","op_2_ambient","op_2_850","op_3_730","op_3_ambient","op_3_850","op_4_730","op_4_ambient","op_4_850", "op_5_730","op_5_ambient","op_5_850", "op_6_730","op_6_ambient","op_6_850", "op_7_730","op_7_ambient","op_7_850", "op_8_730","op_8_ambient","op_8_850","op_9_730","op_9_ambient","op_9_850","op_10_730","op_10_ambient","op_10_850","op_11_730","op_11_ambient","op_11_850","op_12_730","op_12_ambient","op_12_850","op_13_730","op_13_ambient","op_13_850","op_14_730","op_14_ambient","op_14_850","op_15_730","op_15_ambient","op_15_850","op_16_730","op_16_ambient","op_16_850"), extra = "merge")

## < baseline data (the default COBI Studio baseline)  have now been stored as: baselineNirsValues


  ## not removing any data at this point - just store all of the data from the end of the baseline to the end of the file
  nirsEndRow <-  nir1 %>% dplyr::filter(str_detect(.$value, '-1	Device Stopped')) %>% dplyr::select(id)
  nirsDataValues <- nir1 %>% dplyr::filter(id > bEndRow$id & id < nirsEndRow$id)

## < nirs data have now been stored as: nirsDataValues


# now orgnaise the nirs data into a data frame that we can use, with each datapoint labelled with optode, frequency etc.

# separate nirs data columns

# first column is time from start in seconds

nirsDataValues <- tidyr::separate(nirsDataValues, col = value, sep = "\t", into = c("t", "op_1_730","op_1_ambient","op_1_850","op_2_730","op_2_ambient","op_2_850","op_3_730","op_3_ambient","op_3_850","op_4_730","op_4_ambient","op_4_850", "op_5_730","op_5_ambient","op_5_850", "op_6_730","op_6_ambient","op_6_850", "op_7_730","op_7_ambient","op_7_850", "op_8_730","op_8_ambient","op_8_850","op_9_730","op_9_ambient","op_9_850","op_10_730","op_10_ambient","op_10_850","op_11_730","op_11_ambient","op_11_850","op_12_730","op_12_ambient","op_12_850","op_13_730","op_13_ambient","op_13_850","op_14_730","op_14_ambient","op_14_850","op_15_730","op_15_ambient","op_15_850","op_16_730","op_16_ambient","op_16_850"), extra = "merge")

# create a new column called dataRow to match the 3rd column in the markers file that explicitly states which row/line in the nirs file a marker is related to. This marker row information seems to start after the baseline, so the first row of nirsDataValues is equivalent to the first row as far as the marker file is concerned.

nirsDataValues <- dplyr::mutate(nirsDataValues, dataRow = dplyr::row_number())
nirsDataValues <- nirsDataValues %>% dplyr::select(-id)

# change the data from wide to long format. This will make analysis easier

nirs_long <- nirsDataValues %>%
 tidyr::pivot_longer(
    cols = op_1_730:op_16_850,
    names_to = c("optode", "freq"),
    names_pattern = "op_(.*)_(.*)",
    values_to = "nirValue"

  )

baseline_long <- baselineNirsValues %>%
  tidyr::pivot_longer(
    cols = op_1_730:op_16_850,
    names_to = c("optode", "freq"),
    names_pattern = "op_(.*)_(.*)",
    values_to = "baselineValue"

  )

baseline_long <- baseline_long %>%
  dplyr::select(optode, freq, baselineValue)

nirs_long <- nirs_long %>% left_join(baseline_long)





  nirsData <- nirs_long


## do some variable type corrections
nirsData$nirValue <- as.numeric(nirsData$nirValue)
nirsData$t <- as.numeric(nirsData$t)
nirsData$freq <- as.factor(nirsData$freq)


nirsData <- nirsData %>% ungroup()

return(nirsData %>% as.data.frame())


}
