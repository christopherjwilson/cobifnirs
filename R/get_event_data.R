#'@title Get event data using markers
#'@description This function gets the fnirs data for events in the study by identifying start and end markers in the data. It is useful if you have set up the study with automated (serial port) markers that specify when events happen (start and end) in the data.Before running this function, you should also run the \code{\link{add_markers}} function to bind the markers to the data.
#'
#'The function will look for matching start and end markers (so will assume that each event has a start and end marker). It will then extract the data between the start and end markers and return a list of data frames, one for each event. The data frames will have a trial column that specifies the event number. The function will also add a block column to the data that specifies the block name (inputted as a parameter).
#'@param data A data frame with the fnirs data. It should have been imported using the \code{\link{import_nirs}} function and had markers added using the \code{\link{add_markers}} function.
#'@param blockStart The name of the start marker for blocks of trials in the data. This should be a string and it should match a value in the marker column.
#'@param blockEnd The name of the end marker for blocks of trials in the data. This should be a string and it should match a value in the marker column.
#'@param eventStart The name of the start marker for discrete events/trials in the data. This should be a string and it should match a value in the marker column.
#'@param eventEnd The name of the end marker for discrete events/trials in the data. This should be a string and it should match a value in the marker column.
#'@param lead The number of data points to lead the start marker by. This is useful because of the lead/lag of fnirs responses to events. The default is 0.  To calculate seconds, divide by the sample rate (e.g. at 2 Hz would be a value of 10 for 5 seconds).
#'@param lag The number of data points to lag the end marker by. This is useful because of the lead/lag of fnirs responses to events. The default is 0. To calculate seconds, divide by the sample rate (e.g. at 2 Hz would be a value of 10 for 5 seconds).
#'@param blockname The name of the block. This is for the convenience of the researcher, to be able to distinguish between different blocks of trials, or different tasks. This should be a string and will be added as a column to the data.
#'@return A list of data frames, one for each event. Each data frame will have a trial column that specifies the event number and a block column that specifies the block name.
#'@import dplyr
#'@export

get_event_data <- function(data, eventStart, eventEnd, blockname, lead = 0, lag = 0) {

  data$t <- as.numeric(data$t)


  print(paste("event start marker: ", eventStart))
  print(paste("event end marker: ", eventEnd))
  print(paste("lead: ", lead, " samples"))
  print(paste("lag: ", lag, " samples"))

  # incorporate the lead parameter and select the t column that is lag points before the start marker

  rawStartTimes <- data %>%
    filter(marker == eventStart) %>%
    select(t) %>%
    distinct()

  print(paste("start times: ", startTimes))

  rawEndTimes <- data %>%
    filter(marker == eventEnd) %>%
    select(t) %>%
    distinct()

  print(paste("end times: ", endTimes))


  ## to get the start times with a lead, need to identify the row if the start time is in the t column, then subtract the lead from the row number. Then filter the data again with the new row number/t value.




  startTimes<- as.numeric(startTimes$t)
  endTimes<- as.numeric(endTimes$t)

  if (length(startTimes) != length(endTimes)) {
    print("Error: there is a mismatch in the number of start and end markers")
  } else {

    timeData <- cbind(start = as.numeric(startTimes),end = as.numeric(endTimes)) %>% as.data.frame()
    ntrials <- length(timeData$start) %>% as.numeric()
    print(paste("number of trials: ", ntrials))


    getData <- function(i, timeData, data){


      trial_data <- data %>% filter(t >= timeData$start[[i]] & t <= timeData$end[[i]]) %>% mutate(trial = i, block = blockname)





      trial_data
    }
  }

  list <- lapply(1:ntrials, getData, timeData = timeData, data = data)

  return(list)

}
