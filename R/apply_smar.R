#' @title Apply a Sliding Window Moving Average  to the fnirs data.
#' @description This function will apply a sliding window Motion Artifact Removal algorithm to the fnirs data. The window size and smoothing type can be specified. The defaut is a window value of 10 (5sec at 2Hz) and a simple smoothing approach. The output is a dataframe with the smoothing applied to the data, and the original data is preserved with the suffix "_pre_SMAR" added to the column names. The function uses the movavg function from the pracma package to apply the sliding window Motion Artifact Removal algorithm to the data. The output is a dataframe with the Motion Artifact Removal algorithm applied to the data.



apply_smar <- function(oxyData, n = 10, type = "s") {
  oxyData <-  oxyData %>%
    dplyr::group_by(optode) %>%
    dplyr::mutate(hbo_pre_SMAR = hbo) %>%
    dplyr::ungroup()

  oxyData <-  oxyData %>%
    dplyr::group_by(optode) %>%
    dplyr::mutate(hbr_pre_SMAR = hbr) %>%
    dplyr::ungroup()


  # Sliding-window Motion Artifact Removal algorithm (window size = 10s, upper threshold = 0.025 nm, lower threshold = 0.003nm

  oxyData <-  oxyData %>%
    dplyr::group_by(optode) %>%
    mutate(hbo = pracma::movavg(hbo, n, type)) %>%
    dplyr::ungroup()

  oxyData <-  oxyData %>%
    dplyr::group_by(optode) %>%
    mutate(hbr = pracma::movavg(hbr, n, type)) %>%
    dplyr::ungroup()

  oxyData

}
