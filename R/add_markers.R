#' @title add_markers
#' @description This function will add markers to the NIRS data. The data must be in a long format (imported using import_nirs) and markers must be in a separate file with the exact same name as the NIRS file (with .mrk extension). The returned data will have the markers added as a new column. If there are duplicate markers (i.e. more than one marker sent within a 500ms window), the function will either allow duplicates or remove them, depending on the parameters set.
#' @param nirsData A dataframe containing NIRS data in a long format that has been imported using the import_nirs function.
#' @param allowDuplicates A logical value. If TRUE, the function will allow duplicate dataRow values in the marker file. Default is FALSE.
#' @param removeDuplicates A logical value. If TRUE, the function will remove duplicate dataRow values in the marker file, retaining only the first marker value for that particular datapoint. Default is FALSE.
#' @import dplyr
#' @import stringr
#' @import readr
#' @export
#' @examples
#' # The location of the marker file will be read from the fileName column of the nirsData dataframe.
#' # The marker file must have the same name as the NIRS file with a .mrk extension.
#' nirsData <- import_nirs("path/to/nir/file.nir")
#' nirsData <- add_markers(nirsData)
#'
#' # Let's assume there are duplicate values in the marker file.
#' # This means 2 (or more) markers that have been sent within a 500ms window.
#' # Because the default recording frequency is 2hz,
#' # this means the both markers would be linked to the exact same datapoint (this is confusing!).
#'
#'
#' # We have 2 options: allow the duplicates or remove them.
#' # If we allow them, the output will have extra rows.
#' # These will be exact duplicates of the rows that have the same dataRow value (i.e. the same timepoint) but with the additional markers.
#' # You might want this, and it might cause no issues with your analysis.
#'
#' # To allow duplicates, set allowDuplicates = TRUE.
#' nirsData <- add_markers(nirsData, allowDuplicates = TRUE)
#'
#' # If we remove the duplicates, we will retain only the first marker value for that particular datapoint.
#' # This is will mean that only one of the markers will be linked to this exact datapoint.
#' # This might be useful if you are only interested in the first marker that was sent, or if you sent additional markers accidentally.
#' # The output will have the same number of rows as the input.
#'
#' #To remove duplicates, set removeDuplicates = TRUE.
#'
#' nirsData <- add_markers(nirsData, removeDuplicates = TRUE)

add_markers <- function(nirsData, allowDuplicates = FALSE, removeDuplicates = FALSE){
  nirFileName <- nirsData$fileName[1]

  fullMrkPath <- paste(str_sub(nirFileName, 1, nchar(nirFileName)-4),".mrk", sep = "")  # name the marker file. If it exists, it should have exact same name as nir file

  # check the marker file exists. If not, stop the function

  if (!file.exists(fullMrkPath)){
   print( paste0("Marker file does not exist. Please make sure the marker file has the same name and location (full path) as the NIRS file with a .mrk extension. In this case, it should be: ", fullMrkPath, ". Returning data without any changes.")





          )
  } else {

  print(paste0("Marker file path: " , fullMrkPath))


  # read the marker file
  mrk1 <- read_lines(fullMrkPath)

  # convert to tibble
  mrk1 <- mrk1 %>% as_tibble()


  mrk1 <- dplyr::mutate(mrk1, id = row_number()) # overall row number is added to the file data. This helps to identify when the data actually begins in the file

  # getting the actual marker data
  mStartRow <-  mrk1 %>% dplyr::filter(str_detect(.$value, 'Freq Code:')) %>% dplyr::select(id)
  markerValues <- mrk1 %>% dplyr::filter(id > mStartRow$id & id <= max(id))

  # organising the marker data and adding the dataRow column to match with the same column in the nirs_data

  markerValues <- tidyr::separate(markerValues, col = value, sep = "\t", into = c("t","marker", "dataRow"), extra = "merge")

  # the dataRow column is converted to integer
  markerValues$dataRow <- as.integer(markerValues$dataRow)



  # select only the dataRow and marker columns
  mrk2 <- markerValues %>% dplyr::select(dataRow, marker)

  ## check for duplicate dataRow values

  if (any(duplicated(mrk2$dataRow))){

    if (!removeDuplicates && !allowDuplicates){
      print("Duplicate dataRow values found in the marker file. This means more than one marker was recorded within 500ms. This makes it impossible to link just one of the markers with the nir value at that time, so you might want to check the marker file and remove duplicates. Returning data without any changes for now. To allow duplicates, add the parameter allowDuplicates = TRUE. This will mean there will be extra rows in your nirsData output, which may not be what you want. To remove duplicates, add the parameter removeDuplicates = TRUE.")}

    if (removeDuplicates && allowDuplicates){
      stop("Both removeDuplicates and allowDuplicates are set to TRUE. This is not possible. Please set only one of these parameters to TRUE.")}

    if (removeDuplicates){
      print("Duplicate dataRow values found in the marker file. This means more than one marker was recorded within 500ms. This makes it impossible to link just one of the markers with the nir value at that time. Removing duplicates and retaining only the first marker value for that particular datapoint.")

      # if their are duplicates, remove them and retain only the first marker value for that particular datapoint
      mrk2 <- mrk2 %>% distinct(dataRow, .keep_all = TRUE)

      # merge the marker data with the nirs data

      nirsData <- dplyr::left_join(nirsData, mrk2, by = "dataRow")
    }


    if (allowDuplicates){
      print("Duplicate dataRow values found in the marker file. This means more than one marker was recorded within 500ms. This makes it impossible to link just one of the markers with the nir value at that time. Allowing duplicates. This will mean there will be extra rows in your nirsData output, which may not be what you want.")

      # merge the marker data with the nirs data

      nirsData <- dplyr::left_join(nirsData, mrk2, by = "dataRow")
    }

  } else {
    # merge the marker data with the nirs data
    nirsData <- dplyr::left_join(nirsData, mrk2, by = "dataRow")

}



  }

  nirsData
}
