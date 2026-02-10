#' @title Prepare Data for Multi-Level Modeling (MLM)
#' @description Full pipeline: Epochs data -> Corrects Baseline (Robust) -> Averages Response Window.
#' @param oxy_data Continuous data from apply_mbll().
#' @param event_name String matching a marker in your data (e.g. "Stimulus_A").
#' @param sampling_rate (numeric) The sampling rate of the device (e.g., 2 Hz). Required to convert seconds to samples for epoching.
#' @param lead_seconds (numeric) Seconds before marker to include (used for baseline).
#' @param lag_seconds (numeric) Seconds after marker to include.
#' @param response_window_seconds Vector c(start, end) in seconds relative to marker (e.g. c(5, 15)).
#' @return Dataframe with 1 row per trial, ready for lmer().
#' @import dplyr
#' @export
prepare_mlm_data <- function(oxy_data, event_name, sampling_rate, lead_seconds, lag_seconds, response_window_seconds) {

  # Convert Time -> Samples for the Epoching function
  lead_samples <- round(lead_seconds * sampling_rate)
  lag_samples  <- round(lag_seconds * sampling_rate)

  #  EPOCH: Slice continuous data into trials
  print(paste("Step 1: Epoching", event_name, "(-", lead_seconds, "s to +", lag_seconds, "s)..."))

  trials_list <- get_event_data(oxy_data,
                                eventStart = event_name,
                                eventName = event_name,
                                lead = lead_samples,
                                lag = lag_samples)

  trials_df <- dplyr::bind_rows(trials_list)

  #  Zero the baseline using Robust Relative Time
  # We use the full 'lead' duration as the baseline
  print("Step 2: Removing baseline drift (using relative time)...")
  trials_clean <- apply_baseline_correction(trials_df, baseline_duration = lead_seconds)

  #  AVERAGE: Collapse to single value per trial
  print(paste("Step 3: Averaging response window (", response_window_seconds[1], "s to", response_window_seconds[2], "s)..."))

  # filter by 'rel_time' (seconds), not row numbers.
  # This makes the averaging immune to sorting errors.
  mlm_data <- trials_clean %>%
    group_by(trial, event, optode) %>%
    filter(rel_time >= response_window_seconds[1] & rel_time <= response_window_seconds[2]) %>%
    summarise(
      mean_hbo = mean(hbo_corr, na.rm = TRUE),
      mean_hbr = mean(hbr_corr, na.rm = TRUE),

      # Add Peak metrics if you need them later
      peak_hbo = max(hbo_corr, na.rm = TRUE),
      .groups = "drop"
    )

  return(mlm_data)
}
