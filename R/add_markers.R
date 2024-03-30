#' @title add_markers
#' @description This function will add markers to the NIRS data. The data must be in a long format (imported using import_nirs) and markers must be in a separate file with the exact same name as the NIRS file (with .mrk extension).
#' @param nirsData A dataframe containing NIRS data in a long format that has been imported using the import_nirs function.
#' @import dplyr
#' @import stringr
#' @import readr
#' @export
#' @examples
#' # The location of the marker file will be read from the fileName column of the nirsData dataframe.
#' # The marker file must have the same name as the NIRS file with a .mrk extension.
#' nirsData <- import_nirs("path/to/nir/file.nir")
#' nirsData <- add_markers(nirsData)

add_markers <- function(nirsData){
  nirFileName <- nirsData$fileName[1]

  fullMrkPath <- paste(str_sub(nirFileName, 1, nchar(nirFileName)-4),".mrk", sep = "")  # name the marker file. If it exists, it should have exact same name as nir file

  # check the marker filr exists

  if (!file.exists(fullMrkPath)){
   print( paste0("Marker file does not exist. Please make sure the marker file has the same name and location (full path) as the NIRS file with a .mrk extension. In this case, it should be: ", fullMrkPath, ". Returning data without any changes.") )
  } else {

  print(paste0("Marker file path: " , fullMrkPath))



  mrk1 <- read_lines(fullMrkPath)

  mrk1 <- mrk1 %>% as_tibble()


  mrk1 <- dplyr::mutate(mrk1, id = row_number()) # overall row number is added to the file data. This helps to identify when the data actually begins in the file

  # getting the actual marker data
  mStartRow <-  mrk1 %>% dplyr::filter(str_detect(.$value, 'Freq Code:')) %>% dplyr::select(id)
  markerValues <- mrk1 %>% dplyr::filter(id > mStartRow$id & id <= max(id))

  # organising the marker data and adding the dataRow column to match with the same column in the nirs_data

  markerValues <- tidyr::separate(markerValues, col = value, sep = "\t", into = c("t","marker", "dataRow"), extra = "merge")

  markerValues$dataRow <- as.integer(markerValues$dataRow)



  ## add markers to nirsdata
  mrk2 <- markerValues %>% dplyr::select(dataRow, marker)

  nirsData <- left_join(nirsData, mrk2, by = "dataRow")

  nirsData$marker <- as.factor(nirsData$marker)

  }

  nirsData

}
