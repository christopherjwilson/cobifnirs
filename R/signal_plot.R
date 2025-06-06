#' @title Generate a plot to view the raw signal data from each optode.
#' @description This function will generate a plot to view the raw signal data from each optode. The output is a plot with the signal data on the y-axis and the time on the x-axis. The plot is meant as a quick way to check the quality of the signal data. Can be used in conjunction with the \code{\link{signal_summary}} function to determine low quality channels.
#' @param nirsData (dataframe)  NIRS data that has been imported using the \code{\link{import_nirs}} function.
#' @return A plot with the raw signal data from each optode.
#' @import ggplot2
#' @import ggnewscale
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#' \dontrun{
#' # To generate a signal plot from a single NIRS file
#' nirsData <- import_nirs("path/to/nir/file.nir")
#' signalPlot <- signal_plot(nirsData)
#'
#' # To generate a signal plot from multiple NIRS files which have been imported using the import_nirs function
#' nirsData <- lapply(nirsFiles, import_nirs, path = myPath)
#' signalPlot <- lapply(nirsData, signal_plot)
#'}
#'
#' @seealso \code{\link{import_nirs}}
#'

signal_plot <- function(nirsData, path = NULL){

  print("creating plot...")
  print("if there are markers in the data, a vertical line will be added to the plot at the marker time")

  # if there is a marker column in the data, add a vertical line to the plot at the marker time

  if("nirValue" %in% colnames(nirsData)){
    p <- nirsData %>% ggplot(aes(x = t, y = nirValue, color = freq)) +
      geom_line() +
      ggnewscale::new_scale_color() +
      {if("marker" %in% colnames(nirsData))
        geom_vline(data = nirsData %>% filter(!is.na(marker)), aes(xintercept = t, linetype  = marker, colour = marker), alpha = 0.1)} +
      facet_wrap(.~ optode) +
      theme_minimal() +
      {if("participantID" %in% colnames(nirsData))
        ggtitle(paste("Participant ID: ", unique(nirsData$participantID)))}

    if (!is.null(path)){
      print("saving plot...")
      fn <- nirsData$participantID[1]
      ggsave(filename = paste0(path, "/",fn,".png" ), plot = p, device = "png", width = 10, height = 10,  bg = "white")
    }


  } else if ("hbo" %in% colnames(nirsData)){

    nirsData <- nirsData %>%
      tidyr::pivot_longer(cols = c("hbo", "hbr"), names_to = "type", values_to = "value")


    p <- nirsData %>% ggplot(aes(x = t, y = value, color = type)) +
      geom_line() +
      ggnewscale::new_scale_color() +
      {if("marker" %in% colnames(nirsData))
        geom_vline(data = nirsData %>% filter(!is.na(marker)), aes(xintercept = t, linetype  = marker, colour = marker), alpha = 0.1)} +
      facet_wrap(.~ optode) +
      theme_minimal() +
      {if("participantID" %in% colnames(nirsData))
        ggtitle(paste("Participant ID: ", unique(nirsData$participantID)))}

    if (!is.null(path)){
      print("saving plot...")
      fn <- nirsData$participantID[1]
      ggsave(filename = paste0(path, "/",fn,".png" ), plot = p, device = "png", width = 10, height = 10,  bg = "white")
    }
  }
  print("plot created")

  return(p)
}
