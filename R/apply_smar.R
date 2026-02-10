#' @title Apply Motion Artifact Correction (Despike & Interpolate)
#' @description Identifies motion artifacts (spikes) based on the rate of change
#' relative to the signal's standard deviation (or IQR). It replaces these spikes
#' with linear interpolation, preserving the signal timing without aggressive smoothing.
#' @param nirsData (dataframe) NIRS data imported via import_nirs().
#' @param iqr_multiplier (numeric) How many Interquartile Ranges (IQR) above the norm counts as a spike?
#' Default is 1.5 (standard statistical outlier definition). Higher = less sensitive.
#' @return Dataframe with artifacts removed and interpolated.
#' @import dplyr
#' @importFrom zoo na.approx
#' @export
apply_smar <- function(nirsData, iqr_multiplier = 1.5) {

  print(paste0("Running Artifact Correction (IQR Multiplier: ", iqr_multiplier, ")"))

  # 1. Calculate Rate of Change (Derivative)
  # We look at how fast the signal is changing. Real brain signals are slow;
  # artifacts are instantaneous jumps.
  corrected_data <- nirsData %>%
    group_by(optode, freq) %>%
    mutate(
      # save original data for audit trail
      nirValue_pre_smar = nirValue,

      # Calculate velocity (difference between samples)
      velocity = c(0, diff(nirValue)),

      # 2. Define Dynamic Thresholds (Robust Statistics)
      # We use IQR (Interquartile Range) instead of SD because huge artifacts
      # distort SD, making the threshold too high to catch them.
      vel_q1 = quantile(velocity, 0.25, na.rm = TRUE),
      vel_q3 = quantile(velocity, 0.75, na.rm = TRUE),
      vel_iqr = vel_q3 - vel_q1,

      # Define cutoff bounds
      lower_limit = vel_q1 - (iqr_multiplier * vel_iqr),
      upper_limit = vel_q3 + (iqr_multiplier * vel_iqr),

      # 3. Identify Artifacts
      # If velocity jumps outside these bounds, it's a spike.
      is_artifact = (velocity < lower_limit) | (velocity > upper_limit),

      # 4. Interpolate
      # Step A: Set artifacts to NA
      nirValue_clean = ifelse(is_artifact, NA, nirValue),

      # Step B: Linear Interpolation to fill NAs
      # na.approx fills the gaps by drawing a line between the good points
      nirValue = zoo::na.approx(nirValue_clean, na.rm = FALSE, rule = 2) # rule=2 keeps ends
    ) %>%

    # Cleanup
    select(-velocity, -vel_q1, -vel_q3, -vel_iqr, -lower_limit, -upper_limit, -is_artifact, -nirValue_clean) %>%
    ungroup()

  return(corrected_data)
}
