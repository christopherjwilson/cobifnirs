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
#' # To generate a signal summary from a single NIRS file
#' nirsData <- import_nirs("path/to/nir/file.nir")
#' signalSummary <- signal_summary(nirsData)
#'
#'  # To generate a signal summary from multiple NIRS files
#'
#'  ##  name path to files (ending in "/")
#'  mypath <- paste0(system.file("extdata", package = "cobifnirs"),"/")
#'
#'  ## List all files in the folder that end in ".nir"
#'  nirsFiles <- list.files(mypath, pattern = ".nir", full.names = FALSE)
#'
#'  ## Import all of the nirs files
#'  nirsData <- lapply(nirsFiles, import_nirs, folder = myPath)
#'
#' ## Generate a signal summary for each of the nirs files
#' signalSummary <- lapply(nirsData, signal_summary)
#'
#' ## Combine the signal summaries into a single dataframe (recommend you name each of the dataframes in the list first)
#'
#'
#' ## add a column to each dataframe in the list to identify the participant - in this example, pids is a vector of participant ids
#'
#' signalSummary <- mapply(`[<-`, signalSummary, 'npid', value = pids, SIMPLIFY = FALSE)
#'
#' ## combine the dataframes into a single dataframe
#'
#' signalSummary <- do.call(rbind, signalSummary)
#'
#' }
#' @seealso \code{\link{import_nirs}}


signal_summary <- function(nirsData, min = 1000, max = 4000){

## add tryCatch to handle errors

  tryCatch(
    expr = {
      signal_summary <- nirsData %>%
        dplyr::group_by(optode) %>%
        dplyr::summarise(min_max_signal_status = dplyr::case_when(
          min(nirValue) < min ~ "weak",
          max(nirValue) >= max ~ "saturated",
          min(nirValue) > min & max(nirValue) < max ~ "ok"
        ), mean_signal_status = dplyr::case_when(
          mean(nirValue) < min ~ "weak",
          mean(nirValue) >= max ~ "saturated",
          mean(nirValue) > min & mean(nirValue) < max ~ "ok"
        ),
        has_na = any(is.na(nirValue))) %>%
        dplyr::ungroup()


    },
    error = function(e) {
      stop("There was an error in the signal_summary function: ", e$message, ". Please check the data and try again.")
    }
  )

  return(signal_summary)
}

