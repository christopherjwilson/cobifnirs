#' @title Generate Canonical HRF (Double Gamma)
#' @description Generates a standard double-gamma Hemodynamic Response Function.
#' This "ideal" wave shape is used to model what the brain response should look like.
#' @param tr Time repetition (sampling interval in seconds). E.g., 0.5 for 2Hz.
#' @param duration Length of the HRF kernel in seconds (default 32s).
#' @return A numeric vector representing the HRF.
#' @export
get_canonical_hrf <- function(tr, duration = 32) {
  t <- seq(0, duration, by = tr)

  # Standard Glover (1999) parameters for fNIRS/fMRI
  a1 <- 6; b1 <- 1; c1 <- 1/6
  a2 <- 16; b2 <- 1; c2 <- 1/6
  ratio <- 1/6

  # Double Gamma Equation
  hrf <- (t^(a1-1) * b1^a1 * exp(-b1*t)) / gamma(a1) -
    ratio * (t^(a2-1) * b2^a2 * exp(-b2*t)) / gamma(a2)

  # Normalize amplitude to 1
  hrf <- hrf / max(hrf)
  return(hrf)
}

#' @title Create Design Matrix Regressor
#' @description Converts event markers into a continuous time-series regressor by convolving
#' a stick function with the canonical HRF. This handles the *overlap* between trials.
#' @param times Vector of time points from your data.
#' @param event_times Vector of specific event onset times (in seconds).
#' @param hrf The HRF vector from get_canonical_hrf().
#' @return A numeric vector of the same length as 'times'.
#' @export
create_regressor <- function(times, event_times, hrf) {
  # 1. sampling rate estimation
  tr <- mean(diff(times))
  n_scans <- length(times)

  # 2. Create Stick Function (0 = silence, 1 = event)
  stick_vec <- rep(0, n_scans)

  # Find indices of events
  event_indices <- round((event_times - min(times)) / tr) + 1
  event_indices <- event_indices[event_indices <= n_scans & event_indices > 0]

  stick_vec[event_indices] <- 1

  # 3. Convolve
  # 'open' convolution expands length, so we trim it back to n_scans
  convolved <- stats::convolve(stick_vec, hrf, type = "open")
  convolved <- convolved[1:n_scans]

  # Normalize again to keep Beta interpretable
  if(max(abs(convolved)) > 0) convolved <- convolved / max(abs(convolved))

  return(convolved)
}
