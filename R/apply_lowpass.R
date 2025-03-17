#'@title Apply lowpass filter to fnirs data.
#'@description This function will apply a lowpass filter to the fnirs data. The frequency of the filter and the window can be specified in the function. The output is a dataframe with the lowpass filter applied to the data, and the original data is preserved with the suffix "_pre_lp" added to the column names. The function uses a fir1 filter from the gsignal package, with a filter length of 2. The filter is scaled by dividing by the sum of the coefficients, and the starting point values are created using the filter_zi function. The filter is applied to the data, and the delay is calculated using the grpdelay function. The delay is then compensated for by shifting the data. The output is a dataframe with the lowpass filter applied to the data.
#'@param oxyData (dataframe)  NIRS data that has been imported using the \code{\link{import_nirs}} function.
#'@param freq (numeric) The frequency of the lowpass filter. The default is 0.1 (10Hz).
#'@param w (numeric) The window of the lowpass filter. The default is 20.
#'@return A dataframe with the lowpass filter applied to the data. Original data is preserved with the suffix "_pre_lp" added to the column names. The nirValue column is replaced with the lowpass filtered values.
#'@import dplyr
#'@import gsignal
#' @export
#'@examples
#' \dontrun{
#' # Assuming that the fnirs data has been imported using the \code{\link{import_nirs}} function.
#' apply_lowpass(nirData, freq = 0.1, w = 20)
#' }

apply_lowpass <- function(nirData, freq = 0.1, w = 20) {

  fs = 2
  h <- gsignal::fir1(w, freq/(fs / 2), "low") # create filter


  hn <- h / sum(h) # scale the filter by dividing by the sum of coefficients
  myzi <- gsignal::filter_zi(hn) # create the starting point values


  #  save original nirsData

  nirData <- nirData %>%
    dplyr::group_by(optode, freq) %>%
    dplyr::mutate(nirValue_pre_lp = nirValue) %>%
    ungroup()

  # create the new filtered hbo and hbr values

  nirData <- nirData %>%
    dplyr::group_by(optode, freq) %>%
    dplyr::mutate(lp_nir_value = unlist(gsignal::filter(hn,nirValue,myzi * nirValue[1])[1])) %>%
    ungroup()


  # find the filter delay so it can be compensated for
  gd <- gsignal::grpdelay(hn)
  delay <- mean(gd$gd)

  # compensate for the delay
  nirData <- nirData %>%
    dplyr::group_by(optode, freq) %>%
    dplyr::mutate(nirValue = c(lp_nir_value[(delay +1):length(lp_nir_value)], rep(NA, delay)) ) %>%
    ungroup()



  # remove the uncompensated value

  nirData <- nirData %>%
    dplyr::select(-lp_nir_value, -lp_nir_value)

  nirData

}
