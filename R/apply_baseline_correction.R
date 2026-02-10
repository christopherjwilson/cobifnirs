#' @title Apply Local Baseline Correction (Robust)
#' @description Aligns trials to zero using relative time.
#' @param epoched_data Dataframe from get_event_data().
#' @param baseline_duration (numeric) Length of baseline in *SECONDS* (e.g., 5).
#' This usually matches the 'lead' time you used in get_event_data.
#' @return Dataframe with 'hbo_corr', 'hbr_corr', and 'rel_time'.
#' @import dplyr
#' @export
apply_baseline_correction <- function(epoched_data, baseline_duration = 5) {

  print(paste("Correcting baseline using first", baseline_duration, "seconds..."))

  corrected_data <- epoched_data %>%
    group_by(trial, optode) %>%
    mutate(
      # 1. Create Relative Time (0 = Start of the Epoch)
      # This ensures we ignore the absolute 't' which might be confusing
      start_t = min(t, na.rm = TRUE),
      rel_time = t - start_t,

      # 2. Identify Baseline based on TIME, not rows
      # We add a tiny buffer (0.001) to handle floating point inequality
      is_baseline = rel_time < (baseline_duration + 0.001),

      # 3. Calculate Baseline
      base_hbo = mean(hbo[is_baseline], na.rm = TRUE),
      base_hbr = mean(hbr[is_baseline], na.rm = TRUE),

      # 4. Correct
      hbo_corr = hbo - base_hbo,
      hbr_corr = hbr - base_hbr
    ) %>%
    ungroup() %>%
    select(-is_baseline, -start_t) # Keep 'base_hbo' if you want to debug, otherwise remove

  return(corrected_data)
}
