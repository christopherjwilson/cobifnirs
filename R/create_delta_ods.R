#' @title Calculate change in optical density (delta od) from NIRS data
#' @description Calculates the change in oxygen density (delta od) for each channel from NIRS data, based on a reference value.
#' @param data a data frame containing the NIRS data. This should be in the format of the output from the function \code{\link{import_nirs}}.
#' @param reference a character string specifying the reference value to use for calculating delta od. This can be either 'baseline' or 'mean'. If 'baseline', the baseline value for each channel is used as the reference value. If 'mean', the mean value for each channel is used as the reference value.
#' @return a data frame which adds the delta od values for each channel to a new column.
#' @examples
#' data <- read_nirs_data("data.csv")
#' data <- create_delta_ods(data, reference = "baseline")
#' @export



# create change in oxygenation levels
create_delta_ods <- function(nirsData, reference = "baseline"){



  if (reference == "baseline")  {
    #Converts intensities into optical density changes. Changes are relative
    # to the average intensity or a reference intensity for each channel.

    #Optical density from light intensity:
    # deltaOD = -log10(I_t/IRef)


    delta_od_730 <- nirsData %>% dplyr::filter(freq == "850") %>% group_by(optode) %>% reframe(delta_od_850 = -log10(abs(as.numeric(nirValue)) / abs(as.numeric(baselineValue))), t = t, dataRow = dataRow, startTime = startTime,fileName = fileName  ) %>% ungroup()

    delta_od_850 <- nirsData %>% dplyr::filter(freq == "730") %>% group_by(optode) %>% reframe(delta_od_730 = -log10(abs(as.numeric(nirValue)) /abs(as.numeric(baselineValue))), t = t, dataRow = dataRow, startTime = startTime,fileName = fileName    ) %>% ungroup()

    delta_od_all <- left_join(delta_od_730,delta_od_850)

  } else {

    # if there is no reference baseline, or if we don't want to use that right now, we can use the averge of all of the nirs values as a reference.

    # FROM NIRSIMPLE: delta_OD = -log10(I_t/I_average)
    # Creating delta od values for each optode at the same time as calculating mean and then joining the two frequency values together
    nirsData$nirValue <- as.numeric(nirsData$nirValue)

    nirsData <- nirsData %>%
      group_by(optode, freq) %>%
      mutate(channelMean = mean(abs(nirValue))) %>% ungroup()

    print(paste("the mean of the channel is:", nirsData$channelMean))

    delta_od_730 <- nirsData %>% dplyr::filter(freq == "850") %>% group_by(optode) %>% reframe(delta_od_850 = -log10(abs(nirValue) / channelMean), t = t, dataRow = dataRow, startTime = startTime,fileName = fileName   ) %>% ungroup()

    delta_od_850 <- nirsData %>% dplyr::filter(freq == "730") %>% group_by(optode) %>% reframe(delta_od_730 = -log10(abs(nirValue) / channelMean), t = t, dataRow = dataRow, startTime = startTime,fileName = fileName    ) %>% ungroup()

    delta_od_all <- left_join(delta_od_730,delta_od_850)

  }

  delta_od_all

}

