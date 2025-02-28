#' @title Apply lowpass filter to fnirs data.
#' @description This function will apply a lowpass filter to the fnirs data. The frequency of the filter and the window can be specified in the function. The output is a dataframe with the lowpass filter applied to the data, and the original data is preserved with the suffix "_pre_lp" added to the column names. The function uses a fir1 filter from the gsignal package, with a filter length of 2. The filter is scaled by dividing by the sum of the coefficients, and the starting point values are created using the filter_zi function. The filter is applied to the data, and the delay is calculated using the grpdelay function. The delay is then compensated for by shifting the data. The output is a dataframe with the lowpass filter applied to the data.
#' @param oxyData (dataframe)  NIRS data that has been imported using the \code{\link{import_nirs}} function and processed using the \code{\link{create_delta_od}} and \code{\link{apply_mbll}} functions.
#' @param freq (numeric) The frequency of the lowpass filter. The default is 0.1 (10Hz).
#' @param w (numeric) The window of the lowpass filter. The default is 20.
#' @return A dataframe with the lowpass filter applied to the data. Original data is preserved with the suffix "_pre_lp" added to the column names. The hbo and hbr columns are replaced with the lowpass filtered values.
#' @import dplyr
#' @import gsignal
#' @export
#' @examples
#' \dontrun{
#' # Assuming that the fnirs data has been imported using the \code{\link{import_nirs}} function and processed using the \code{\link{create_delta_od}} and \code{\link{apply_mbll}} functions.
#' apply_lowpass(oxyData, freq = 0.1, w = 20)
#' }

apply_lowpass <- function(oxyData, freq = 0.1, w = 20) {

  fs = 2
  h <- gsignal::fir1(w, freq/(fs / 2), "low") # create filter


  hn <- h / sum(h) # scale the filter by dividing by the sum of coefficients
  myzi <- gsignal::filter_zi(hn) # create the starting point values


  #  save original nirsData

  oxyData <- oxyData %>%
    dplyr::group_by(optode) %>%
    dplyr::mutate(hbo_pre_lp = hbo, hbr_pre_lp = hbr) %>%
    ungroup()

  # create the new filtered hbo and hbr values

  oxyData <- oxyData %>%
    dplyr::group_by(optode) %>%
    dplyr::mutate(lp_hbo_value = unlist(gsignal::filter(hn,hbo,myzi * hbo[1])[1])) %>%
    ungroup()

  oxyData <- oxyData %>%
    dplyr::group_by(optode) %>%
    dplyr::mutate(lp_hbr_value = unlist(gsignal::filter(hn,hbr,myzi * hbr[1])[1])) %>%
    ungroup()


  # find the filter delay so it can be compensated for
  gd <- gsignal::grpdelay(hn)
  delay <- mean(gd$gd)

  # compensate for the delay
  oxyData <- oxyData %>%
    dplyr::group_by(optode) %>%
    dplyr::mutate(hbo = c(lp_hbo_value[(delay +1):length(lp_hbo_value)], rep(NA, delay)) ) %>%
    ungroup()


  oxyData <- oxyData %>%
    dplyr::group_by(optode) %>%
    dplyr::mutate(hbr = c(lp_hbr_value[(delay +1):length(lp_hbr_value)], rep(NA, delay)) ) %>%
    ungroup()

  # remove the uncompensated value

  oxyData <- oxyData %>%
    dplyr::select(-lp_hbo_value, -lp_hbr_value)

  oxyData

}
