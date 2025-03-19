#'@title Add Brodmann's areas to the data
#'@description This function adds Brodmann's areas to the data in a new column. Brodmann's areas can be added to raw nir data or to HbO/HbR data. The assignment of Brodmann's areas is based on the coordinates of the optodes.
#'@param data The data to which Brodmann's areas will be added. This data should have been imported using the \code{\link{import_nirs}} function.
#'@return The data with Brodmann's areas added. There will be 2 new columns: \code{hemi} and \code{region}. The \code{hemi} column indicates whether the optode is on the left or right hemisphere. The \code{region} column indicates the Brodmann's area.
#'@importFrom dplyr mutate
#'@importFrom dplyr case_when
#'@export
#'@examples
#' \dontrun{
#' data <- import_nirs("data.nir")
#' data <- addBrodmanns(data)
#' }

# this function adds brodmann's areas to the data, based on the optode location
addBrodmanns <- function(nirsData) {
  data <-  nirsData %>% mutate(hemi = ifelse(optode < 9, "left", "right"))
  data <- data %>% mutate(region = case_when((optode == 1 | optode == 15) ~ "BA44",
                                             (optode == 2 | optode == 16) ~ "BA45",
                                             (optode == 3 | optode == 4 | optode == 13 | optode == 14) ~ "BA46",
                                             (optode == 5 | optode == 6 | optode == 7 | optode == 8 | optode == 9 | optode == 10 | optode == 11 | optode == 12 ) ~ "BA10",

  ))

  data
}


