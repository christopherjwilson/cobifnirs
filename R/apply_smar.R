#' @title Apply a Sliding Window Motion Artifact Removal algorithm to the fnirs data.
#' @description This function will apply a sliding window Motion Artifact Removal algorithm to the fnirs data. The window size, sample rate, and thresholds can be specified in the function.
#'
#' There are two steps to the algorithm:
#'
#' 1. Identify artifacts based on thresholds. The differences between consecutive values are calculated, and if the difference is greater than the upper threshold or less than the lower threshold, the data point is identified as an artifact. It is then replaced with an interpolated value.
#'
#' 2. Apply simple smoothing to the data using a manual sliding window - centered average. The centered average is calculated for each data point using the window size specified.
#'
#' The output is a dataframe with the sliding window Motion Artifact Removal algorithm applied to the data, and the original data is preserved with the suffix "_pre_smar" added to the column name.
#' @param nirsData (dataframe)  NIRS data that has been imported using the \code{\link{import_nirs}} function.
#' @param window_size_seconds (numeric) The window size in seconds for the sliding window. The default is 10 (5sec at 2Hz).
#' @param sample_rate (numeric) The sample rate of the data. The default is 2.
#' @param upper_threshold (numeric) The upper threshold for identifying artifacts. The default is 0.025.
#' @param lower_threshold (numeric) The lower threshold for identifying artifacts. The default is 0.003.
#' @param perserve_all (logical) If TRUE, each step of the process will be preserved in the data, with _pre_smar (original data) and _pre_smooth (artifacts removed, before overall smoothing) added to the column name. The default is FALSE.
#' @import dplyr
#' @import zoo
#' @return A dataframe with the sliding window Motion Artifact Removal algorithm applied to the data. Original data is preserved with the suffix "_pre_smar" added to the column names. The nirValue column is replaced with the smoothed values.
#' @export

apply_smar <- function(nirsData, window_size_seconds = 5, sample_rate = 2, upper_threshold = 0.025, lower_threshold = 0.003, preserve_all = T) {
  nirsData <-  nirsData %>%
    dplyr::group_by(optode, freq) %>%
    dplyr::mutate(nirValue_pre_smar = nirValue) %>%
    dplyr::ungroup()


  # Calculate window size in samples
  window_size_samples <- window_size_seconds * sample_rate

  # Group data by optode and frequency
  corrected_nirs_data <- nirsData %>%
    group_by(optode, freq) %>%
    mutate(
      # Calculate differences within each group
      diffs = nirValue - lag(nirValue),

      # Identify artifacts based on thresholds
      artifact = !is.na(diffs) & (abs(diffs) > upper_threshold | abs(diffs) < lower_threshold),

      # Replace artifacts with interpolated values (example: linear interpolation) only if artifact is TRUE and diffs is not NA
      nirValue_pre_smooth = ifelse(
        artifact & !is.na(diffs),
        approx(t, nirValue, xout = t[artifact] - 1/sample_rate)$y,  # Adjust time points
        nirValue
      )



    ) %>%
    mutate(
      # if nirValue_pre_smooth is NA, replace with nirValue

      nirValue_pre_smooth = ifelse(is.na(nirValue_pre_smooth), nirValue, nirValue_pre_smooth)


    ) %>%
    ungroup()


#
  corrected_nirs_data <- corrected_nirs_data %>%
    group_by(optode, freq) %>%
    mutate(
      # Calculate the centered average
      nirValue_final = zoo::rollmean(nirValue_pre_smooth, window_size_samples, fill = NA, align = "center")
    ) %>%
    mutate(nirValue_final = ifelse(is.na(nirValue_final), nirValue_pre_smooth, nirValue_final)) %>%
    ungroup()

## final value replaces the original value

  corrected_nirs_data <- corrected_nirs_data %>%
    mutate(nirValue = nirValue_final) %>%
    select(-nirValue_final)

  if (preserve_all) {
    return(corrected_nirs_data)
  } else {
    corrected_nirs_data <- corrected_nirs_data %>%
      select(-diffs, -nirValue_pre_smooth, -artifact)
  }



  return(corrected_nirs_data)
}
