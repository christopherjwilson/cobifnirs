#' @title Remove channels from NIRS data
#' @description This function will remove the specified channels from the NIRS data. This can be useful if you have channels that are not required for your analysis or if channels are saturated or weak, or noisy.
#' @param nirsData A dataframe containing NIRS data in a long format that has been imported using the import_nirs function.
#' @param channels A vector of channel names to remove from the NIRS data. The channel names must match the names in the *optode* column of the nirs data (i.e., 1-16).
#' @return A dataframe with the specified channels removed.
#' @import dplyr
#' @export
#' @examples
#' nirsData <- import_nirs("path/to/nir/file.nir")
#' problem_channels <- c("1", "2", "3")
#' nirsData <- remove_channels(nirsData, problem_channels)
#' @seealso \code{\link{import_nirs}}


remove_channels <- function(nirsData, channels){


  nirsData <- nirsData %>% dplyr::filter(!optode %in% channels)

  nirsData

}

