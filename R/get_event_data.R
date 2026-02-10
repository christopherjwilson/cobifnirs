#' @title Epoch Data for Multi-Level Modeling (MLM)
#' @description Extracts event-locked epochs (trials) from continuous fNIRS data.
#' This function slices the data around specific markers (e.g., "Stimulus_A")
#' to create a list of individual trials. This is the first step in preparing
#' data for Block Averaging or Multi-Level Modeling.
#'
#' @details
#' The function identifies the start and end times of events based on markers.
#' It allows for a "lead" (pre-stimulus baseline) and "lag" (post-stimulus response)
#' to capture the full hemodynamic response. The output is a list of dataframes,
#' where each dataframe represents one trial.
#'
#' @param data (dataframe) Continuous fNIRS data imported via \code{\link{import_nirs}}
#' and containing a 'marker' column from \code{\link{add_markers}}.
#' @param eventStart (character) The marker name indicating the start of the event (e.g., "Stimulus").
#' @param eventEnd (character, optional) The marker name indicating the end of the event.
#' If NULL (default), the function uses \code{lead} and \code{lag} to define a fixed-duration epoch.
#' @param eventName (character) A label for this condition (e.g., "Condition_A") to be added to the output.
#' @param lead (numeric) Number of samples *before* the start marker to include (e.g., for baseline correction).
#' Default is 0. Calculate as: \code{Seconds * SampleRate}.
#' @param lag (numeric) Number of samples *after* the end marker to include.
#' Default is 0. Calculate as: \code{Seconds * SampleRate}.
#'
#' @return A list of dataframes, where each element is a single trial containing
#' columns for time, data, trial number, and event name.
#'
#' @examples
#' \dontrun{
#' # Example: Extract 5s baseline (lead) and 25s response (lag) at 10Hz
#' # Lead = 5s * 2Hz = 10 samples
#' # Lag = 5s * 2Hz = 10 samples
#'
#' trials <- get_event_data(oxy_data,
#'                          eventStart = "Stimulus_A",
#'                          eventName = "Condition_A",
#'                          lead = 10,
#'                          lag = 10)
#'
#' # Combine into a single dataframe for MLM
#' mlm_data <- dplyr::bind_rows(trials)
#' }
#' @import dplyr
#' @export

get_event_data <- function(data, eventStart, eventEnd = NULL, eventName, lead = 0, lag = 0) {

  data$t <- as.numeric(data$t)
  data$marker <- as.character(data$marker)
  data$dataRow <- as.numeric(data$dataRow)

  ## if the end marker is not specified, then the function will assume that the end marker is the same as the start marker

  if(is.null(eventEnd)){
    eventEnd <- eventStart
    print(paste("end marker not set, defaulting to: ", eventEnd))
    print("Assuming you want to check for a single marker")
    # this means that lead and lag must be set, so we will default to a lead of 3 and a lag of 5 if the lead and lag are not set

    if(lead == 0){
      print("lead not set, defaulting to 6 samples")
      lead <- 6
    }

    if(lag == 0){
      print("lag not set, defaulting to 10 samples")
      lag <- 10
    }


  }

  print(paste("event start marker: ", eventStart))
  print(paste("event end marker: ", eventEnd))
  print(paste("lead: ", lead, " samples"))
  print(paste("lag: ", lag, " samples"))

  # incorporate the lead parameter and select the t column that is lag points before the start marker

  rawStartRows <- data %>% as.data.frame() %>%
    dplyr::filter(marker == eventStart) %>%
    dplyr::select(dataRow) %>%
    dplyr::distinct() %>%
    dplyr::pull(dataRow)

  print(paste("start rows: ", rawStartRows))
  ### print number of start rows
  print(paste("number of start rows: ", length(rawStartRows)))

  rawStartTimes <- data %>%
    dplyr::filter(marker == eventStart) %>%
    dplyr::select(t) %>%
    dplyr::distinct() %>%
    dplyr::pull(t)

  print(paste("start times: ", rawStartTimes))




  rawEndRows <- data %>% as.data.frame() %>%
    dplyr::filter(marker == eventEnd) %>%
    dplyr::select(dataRow) %>%
    dplyr::distinct() %>%
    dplyr::pull(dataRow)

  print(paste("end rows: ", rawEndRows))
  ### print number of end rows
  print(paste("number of end rows: ", length(rawEndRows)))

  rawEndTimes <- data %>%
    dplyr::filter(marker == eventEnd) %>%
    dplyr::select(t) %>%
    dplyr::distinct() %>%
    dplyr::pull(t)


  print(paste("end times: ", rawEndTimes))


if(lead > 0 | lag > 0){

  print(paste("lead: ", lead, " samples"))
  print(paste("lag: ", lag, " samples"))
  print("calculating start and end times with lead and lag")

  ## to get the start times with a lead, we need to subtract the lead from the start rows

  startRows <- rawStartRows - lead

  ## print new start rows

  print(paste("start rows with lead: ", startRows))

  ## now we need to identify the start times that correspond to the start rows

  startTimes <- data %>%
    dplyr::filter(dataRow %in% startRows) %>%
    dplyr::select(t) %>%
    dplyr::distinct() %>%
    dplyr::pull(t)

  ## to get the end times with a lag, we need to add the lag to the end rows

  endRows <- rawEndRows + lag

  ## print new end rows

  print(paste("end rows with lag: ", endRows))

  ## now we need to identify the end times that correspond to the end rows

  endTimes <- data %>%
    dplyr::filter(dataRow %in% endRows) %>%
    dplyr::select(t) %>%
    dplyr::distinct() %>%
    dplyr::pull(t)


  print(paste("start rows with lead: ", startRows))
  print(paste("start times with lead: ", startTimes))
  print(paste("end rows with lag: ", endRows))
  print(paste("end times with lag: ", endTimes))

} else {

  startTimes <- rawStartTimes
  endTimes <- rawEndTimes

}


  if (length(startTimes) != length(endTimes)) {
    print("Error: there is a mismatch in the number of start and end markers")
  } else {

    print("start and end markers match")

    # combine the start and end times into a data frame


    timeData <-cbind(start = startTimes, end = endTimes) %>%
      as.data.frame()


    ntrials <- length(timeData$start) %>% as.numeric()
    print(paste("number of trials: ", ntrials))


    getData <- function(i, timeData, data){


      trial_data <- data %>% dplyr::filter(t >= timeData$start[[i]] & t <= timeData$end[[i]]) %>% dplyr::mutate(trial = i, event = eventName)





      trial_data
    }
  }

  list <- lapply(1:ntrials, getData, timeData = timeData, data = data)

  return(list)

}
