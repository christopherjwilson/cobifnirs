#' @title Create change in optical density values from nirs data
#' @description This function takes in nirs data and creates Delta OD (change in optical density) values for each optode. This is based on the formula `deltaOD = -log10(I_t/IRef)`. This is a preliminary step before calculating HbO and HbR values, using the MBLL law, using the \code{\link{apply_mbll}} function. It can use a reference baseline or the average of all of the nirs values (for that optode) as a reference. By default, it will use the baseline values from the COBI data file import, as these should have been stored in the nirs data object when the data was imported. This conversion is based on the two frequency values of the nirs data being 730nm and 850nm, which should also be specified in the nirs data object. The output will be a data frame containing the Delta OD values for each optode, for each frequency. The column names will be delta_od_730 and delta_od_850.
#' @param nirsData A data frame containing nirs data
#' @param reference A string indicating whether to use the baseline values from the COBI data file import as a reference, or to use the average of all of the nirs values as a reference. Can be "average" or "baseline". Default is "baseline".
#' @return A data frame containing the Delta OD values for each optode, for each frequency. The column names will be delta_od_730 and delta_od_850.
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' # single nir data file
#' delta_od <- create_delta_od(nirsData = nirsData, reference = "baseline")
#'
#' # multiple nir data files
#' delta_od_all <- lapply(nirsData, create_delta_od, reference = "baseline")
#' }
#'
#' @seealso \code{\link{apply_mbll}}
#' @seealso \code{\link{import_nirs}}
#'

# create change in oxygenation levels
create_delta_od <- function(nirsData, reference = "baseline"){



  if (reference == "baseline")  {
    #Converts intensities into optical density changes. Changes are relative
    # to the average intensity or a reference intensity for each channel.

    #Optical density from light intensity:
    # deltaOD = -log10(I_t/IRef)


    delta_od_730 <- nirsData %>% dplyr::filter(freq == "850") %>% group_by(optode) %>% reframe(delta_od_850 = -log10(as.numeric(nirValue) / as.numeric(baselineValue)), t = t, dataRow = dataRow, startTime = startTime,fileName = fileName  ) %>% ungroup()

    delta_od_850 <- nirsData %>% dplyr::filter(freq == "730") %>% group_by(optode) %>% reframe(delta_od_730 = -log10(as.numeric(nirValue) /as.numeric(baselineValue)), t = t, dataRow = dataRow, startTime = startTime,fileName = fileName    ) %>% ungroup()

    delta_od_all <- left_join(delta_od_730,delta_od_850)

    # restore markers

    if("marker" %in% colnames(nirsData)){
      # Get a clean map of Time -> Marker
      marker_map <- nirsData %>%
        select(t, marker) %>%
        distinct()

      # Join it back to the result
      delta_od_all <- delta_od_all %>%
        left_join(marker_map, by = "t")
    }

  } else {

    # if there is no reference baseline, or if we don't want to use that right now, we can use the average of all of the nirs values as a reference.

    # Creating delta od values for each optode at the same time as calculating mean and then joining the two frequency values together
    nirsData$nirValue <- as.numeric(nirsData$nirValue)

    nirsData <- nirsData %>%
      group_by(optode, freq) %>%
      mutate(channelMean = mean(abs(nirValue))) %>% ungroup()

    print(paste("the mean of the channel is:", nirsData$channelMean))

    delta_od_730 <- nirsData %>% dplyr::filter(freq == "850") %>% group_by(optode) %>% reframe(delta_od_850 = -log10(nirValue) / channelMean, t = t, dataRow = dataRow, startTime = startTime,fileName = fileName   ) %>% ungroup()

    delta_od_850 <- nirsData %>% dplyr::filter(freq == "730") %>% group_by(optode) %>% reframe(delta_od_730 = -log10(nirValue) / channelMean, t = t, dataRow = dataRow, startTime = startTime,fileName = fileName    ) %>% ungroup()

    delta_od_all <- left_join(delta_od_730,delta_od_850)

    # restore markers

    if("marker" %in% colnames(nirsData)){
      # Get a clean map of Time -> Marker
      marker_map <- nirsData %>%
        select(t, marker) %>%
        distinct()

      # Join it back to the result
      delta_od_all <- delta_od_all %>%
        left_join(marker_map, by = "t")
    }
  }

  delta_od_all

}
# test
