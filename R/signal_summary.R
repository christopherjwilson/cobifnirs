#' @title Generate a summary of the signal data to view weak and saturated channels
#' @description This function will generate a summary of the signal data to view weak and saturated channels. The output contains the optode, the status of the min and max signal values, the status of the mean signal value, and whether there are any NA values in the signal data. It is meant as a quick way to check the quality of the signal data. Can be used in conjunction with visual inspection to determine low quality channels.
#' @param nirsData (DATAFRAME)  NIRS data that has been imported using the \code{\link{import_nirs}} function.
#' @param min The lower value for the signal data to be considered "weak". Default is 1000.
#' @param max The upper value for the signal data to be considered "saturated". Default is 4000.
#' @return A dataframe with the signal summary.
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' nirsData <- import_nirs("path/to/nir/file.nir")
#' signalSummary <- signal_summary(nirsData)
#' }
#' @seealso \code{\link{import_nirs}}


signal_summary <- function(nirsData, min = 1000, max = 4000){



  signal_summary <- nirsData %>%
    dplyr::group_by(optode) %>%
    dplyr::summarise(min_max_signal_status = case_when(
      min(nirValue) < min ~ "weak",
      max(nirValue) >= max ~ "saturated",
      min(nirValue > min) & max(nirValue) < max ~ "ok"
    ), mean_signal_status = case_when(
      mean(nirValue) < min ~ "weak",
      mean(nirValue) >= max ~ "saturated",
      mean(nirValue > min) & max(nirValue) < max ~ "ok"
    ),
    has_na = any(is.na(nirValue))) %>%
    dplyr::ungroup()

 # signal_summary %>% distinct()

  return(signal_summary)
}

