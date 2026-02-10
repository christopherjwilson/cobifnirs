#' @title Calculate GLM Betas (with Condition Selection)
#' @description Calculates GLM Betas using Stick Functions (Impulses) at event onsets.
#' Allows specifying which markers represent "Conditions" to model, ignoring others (like End markers).
#' @param nirsData Dataframe output from apply_mbll().
#' @param conditions (Optional) Character vector of specific marker names to model.
#' If NULL, it models ALL markers found in the file (risky if you have End markers!).
#' @export
calculate_glm <- function(nirsData, conditions = NULL) {

  optodes <- unique(nirsData$optode)
  tr <- median(diff(sort(unique(nirsData$t))))
  hrf <- get_canonical_hrf(tr = tr)

  # 1. Prepare Events
  # "Explode" the markers (handle the "Start+ConditionA" case we fixed earlier)
  all_events <- nirsData %>%
    dplyr::filter(!is.na(marker)) %>%
    dplyr::select(t, marker) %>%
    tidyr::separate_rows(marker, sep = "\\+") %>%
    dplyr::distinct()

  # 2. Filter for User-Specified Conditions
  if (!is.null(conditions)) {
    # Only keep the markers the user ASKED for
    events_to_model <- all_events %>%
      dplyr::filter(marker %in% conditions)

    if(nrow(events_to_model) == 0) stop("None of the specified conditions were found in the marker data.")

  } else {
    # Default: Model EVERYTHING (Warning: dangerous if you have 'End' markers)
    events_to_model <- all_events
    warning("No 'conditions' specified. Modeling ALL markers found. Ensure 'End' markers are not included!")
  }

  unique_conditions <- unique(events_to_model$marker)
  print(paste("GLM: Modeling onsets for:", paste(unique_conditions, collapse=", ")))

  results_list <- list()

  for(op in optodes) {
    df_single <- nirsData %>% dplyr::filter(optode == op) %>% dplyr::arrange(t)
    if(nrow(df_single) == 0) next

    n_scans <- nrow(df_single)
    design_matrix <- data.frame(Intercept = rep(1, n_scans))

    # 3. Create Regressors (Stick Functions at Onset)
    for(cond in unique_conditions) {
      # Get start times for this specific condition
      cond_times <- events_to_model %>% dplyr::filter(marker == cond) %>% dplyr::pull(t)

      reg <- create_regressor(df_single$t, cond_times, hrf)
      if(length(reg) != n_scans) length(reg) <- n_scans

      design_matrix[[cond]] <- reg
    }

    # Fit Models
    fit_hbo <- stats::lm(df_single$hbo ~ . -1, data = design_matrix)
    fit_hbr <- stats::lm(df_single$hbr ~ . -1, data = design_matrix)

    # Extract
    tmp_res <- data.frame(
      optode = op,
      condition = names(stats::coef(fit_hbo)),
      beta_hbo = stats::coef(fit_hbo),
      beta_hbr = stats::coef(fit_hbr)
    )

    tmp_res <- tmp_res %>% dplyr::filter(condition != "Intercept")
    results_list[[as.character(op)]] <- tmp_res
  }

  final_results <- do.call(rbind, results_list)
  rownames(final_results) <- NULL
  return(final_results)
}
