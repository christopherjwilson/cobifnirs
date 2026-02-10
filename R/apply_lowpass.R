#' @title Apply Lowpass Filter (Zero-Phase)
#' @description Applies a zero-phase Butterworth lowpass filter to the data.
#' Uses forward-backward filtering (filtfilt) to prevent any phase delay or shift.
#' @param nirData (dataframe) NIRS data imported via import_nirs().
#' @param cutoff (numeric) The cutoff frequency in Hz. Default is 0.1 Hz.
#' @param order (numeric) The order of the filter. Default is 4 (standard for fNIRS).
#' @return Dataframe with filtered values. Original data is saved as *_pre_lp.
#' @import dplyr
#' @importFrom gsignal butter filtfilt
#' @export
apply_lowpass <- function(nirData, cutoff = 0.1, order = 4) {

  # Estimate Sample Rate (fs)
  # We calculate this dynamically to be safe
  timestamps <- sort(unique(nirData$t))
  fs <- 1 / mean(diff(timestamps))

  # Design Butterworth Filter
  # Wn = cutoff / (fs/2) [Normalized frequency]
  Wn <- cutoff / (fs / 2)

  # Check for valid Nyquist
  if (Wn >= 1) {
    stop("Cutoff frequency is too high for the sampling rate (must be < fs/2).")
  }

  # Create Filter Coefficients (Butterworth)
  # This creates 'b' (numerator) and 'a' (denominator)
  filt <- gsignal::butter(order, Wn, type = "low")

  # Apply Filter (Zero-Phase)
  # using filtfilt (Forward-Backward filtering)
  print(paste0("Applying 4th-order Butterworth Lowpass at ", cutoff, "Hz (Zero-Phase)"))

  # Save original and overwrite 'nirValue' with filtered data
  nirData <- nirData %>%
    dplyr::group_by(optode, freq) %>%
    dplyr::mutate(
      nirValue_pre_lp = nirValue,
      nirValue = gsignal::filtfilt(filt, nirValue) # Automatic zero-phase
    ) %>%
    dplyr::ungroup()

  return(nirData)
}
