#' @title Add markers to NIRS data
#' @description Adds markers to the data. Handles duplicate timestamps automatically:
#' If removeDuplicates is TRUE, it keeps only the first marker.
#' Otherwise, it collapses duplicates (e.g., "Start+ConditionA") to preserve data integrity.
#' @param nirsData Dataframe from import_nirs()
#' @param removeDuplicates (Logical) If TRUE, only the first marker at a timestamp is kept.
#' Default is FALSE (which collapses duplicates into one string).
#' @export
add_markers <- function(nirsData, removeDuplicates = FALSE){

  nirFileName <- nirsData$fileName[1]
  # Handle path logic (swap extension)
  fullMrkPath <- sub("\\.nir$", ".mrk", nirFileName)

  if (!file.exists(fullMrkPath)){
    warning(paste0("Marker file not found: ", fullMrkPath))
    return(nirsData)
  }

  # Read Marker File
  mrk_raw <- readLines(fullMrkPath, warn = FALSE)

  # Filter for valid data lines
  markerValues <- tibble::tibble(value = mrk_raw) %>%
    dplyr::filter(grepl("\t", value)) %>%
    dplyr::mutate(id = dplyr::row_number())

  # Separate Columns
  markerValues <- tidyr::separate(markerValues, col = value,
                                  sep = "\t",
                                  into = c("t","marker", "dataRow"),
                                  extra = "drop", fill = "right")

  mrk2 <- markerValues %>%
    dplyr::select(dataRow, marker) %>%
    dplyr::mutate(dataRow = as.integer(dataRow)) %>%
    dplyr::filter(!is.na(dataRow))

  # --- SIMPLIFIED LOGIC ---
  if (any(duplicated(mrk2$dataRow))) {

    if (removeDuplicates) {
      # User explicitly wants to clean the data
      print("Duplicate markers found. Removing extras (keeping first).")
      mrk2 <- mrk2 %>% dplyr::distinct(dataRow, .keep_all = TRUE)

    } else {
      # Default Safe Behavior: Collapse
      print("Duplicate markers found. Collapsing them (e.g. 'A+B') to preserve time-series.")

      mrk2 <- mrk2 %>%
        dplyr::group_by(dataRow) %>%
        dplyr::summarise(marker = paste(marker, collapse = "+")) %>%
        dplyr::ungroup()
    }
  }

  # Join back to data
  nirsData <- dplyr::left_join(nirsData, mrk2, by = "dataRow")
  return(nirsData)
}
