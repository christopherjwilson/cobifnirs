#' @title Remove ambient light channel from NIRS data
#' @description This function will remove the ambient light channel from the NIRs data. It is not required to do this, but it can make the data easier to work with.
#' @param nirsData (DATAFRAME)  NIRS data that has been imported using the \code{\link{import_nirs}} function.
#' @return A dataframe with the ambient light channel removed.
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' nirsData <- import_nirs("path/to/nir/file.nir")
#' nirsData <- remove_ambient(nirsData)
#' }
#' @seealso \code{\link{import_nirs}}


remove_ambient <- function(nirsData) {

  nirsData <- nirsData %>% dplyr::filter(freq != "ambient")
  nirsData
}
