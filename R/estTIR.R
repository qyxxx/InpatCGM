#' @title Estimate Time in Range (TIR)
#' @description
#' Estimates the mean Time in Range (TIR) using different methods.
#'
#' This function estimates TIR using either the naive estimator, the proposed
#' noninformative follow-up estimator, or the Cox model-based estimator.
#'
#' @param data A data frame containing glucose monitoring data.
#' @param method A string specifying the estimation method: "naive" or "proposed".
#' @param model A string specifying the model type (default: "NULL" for non-model-based approaches, or "cox" for Cox model).
#' @param time A numeric vector of length 2 specifying the start and end time window (default: c(0, 10075)).
#' @param range A numeric vector specifying the glucose range for in-range classification (default: c(70, 180)).
#' @param boot Number of bootstrap iterations (default: NULL, no bootstrapping).
#' @param id Column name for subject IDs (default: "patient_id").
#' @param glucose Column name for glucose values (default: "glucose").
#' @param time_col Column name representing time (default: "time").
#' @param period Additional time offset for stop time in Cox model (default: 5).
#' @param formula A formula string for the Cox model (default: "var1").
#' @return A list containing TIR estimation results, including the estimate, standard error, and confidence intervals.
#' @export
estTIR <- function(data, method = "proposed", model = "NULL",
                   time = c(0, 1440 * 7 - 5), range = c(70, 180),
                   boot = NULL, id = "patient_id", glucose = "glucose",
                   time_col = "time", period = 5, formula = "var1") {
  # Compute value_in_range based on the specified range
  data <- data |>
    dplyr::mutate(value_in_range = sapply(.data[[glucose]], value_in_range, range = range))
  # Generate event column for Cox model (1 if last observation for a subject, otherwise 0)
  data <- data |> dplyr::group_by(.data[[id]]) |> dplyr::mutate(event = dplyr::if_else(.data[[time_col]] == max(.data[[time_col]]),1,0)) |> dplyr::ungroup()
  # Compute stop time for Cox model
  data$time2 <- data[[time_col]] + period

  # Initialize `est` variable before assignment
  est <- NULL
  # Use switch() for cleaner method selection
  est <- switch(method,
                "naive" = {
                  if (model == "NULL") {
                    naive_est(data, min_time = time[1], max_time = time[2],
                              boot = boot, id_col = id, time = time_col,
                              value_in_range = "value_in_range")
                  } else stop("Error: Model not recognized for 'naive' method")
                },
                "proposed" = {
                  switch(model,
                         "NULL" = proposed_est_noninfo(data, min_time = time[1], max_time = time[2],
                                                       boot = boot, id_col = id, time = time_col,
                                                       value_in_range = "value_in_range"),
                         "cox" = proposed_est_cox(data, min_time = time[1], max_time = time[2],
                                                  id_col = id, event_col = "event",
                                                  start_col = time_col, stop_col = "time2",
                                                  formula = formula, boot = boot,
                                                  value_in_range = "value_in_range"),
                         stop("Error: Model not recognized for 'proposed' method")
                  )
                },
                stop("Error: Method not recognized")
  )

  # Ensure `est` is not NULL before assigning the class
  if (is.null(est)) stop("Error: Estimation method failed to return a result")

  # Assign "TIR" class to result
  class(est) <- "TIR"
  return(est)
}


#' @title Computes the naive estimator for mean Time in Range (TIR).
#' @description
#' This function estimates the mean TIR for a given dataset, filtering based on a
#' specified time window. If bootstrapping is enabled, it computes confidence
#' intervals using resampling.
#'
#' @param data A data frame containing long format time-series data.
#' @param min_time The minimum time threshold for filtering (default = 0).
#' @param max_time The maximum time threshold for filtering (default = 10075, equivalent to 1440 * 7 - 5).
#' @param boot Number of bootstrap iterations (default = NULL, no bootstrapping).
#' @param id_col Column name identifying subjects (default = "patient_id").
#' @param time Column name representing time (default = "time").
#' @param value_in_range Column name of the binary indicator (0/1) for time in range (default = "value_in_range").
#' @return A list containing TIR estimation results, including the estimate, standard error, and confidence intervals.
#' @export
naive_est <- function(data,
                      min_time = 0, max_time = (1440 * 7 - 5),
                      boot = NULL,
                      id_col = "patient_id", time = "time", value_in_range = "value_in_range") {
  # Filter data based on time range
  data <- data[data[[time]] <= max_time & data[[time]] >= min_time, ]
  # Compute mean TIR per subject, then overall mean TIR
  TIR <- data|>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::summarise(TIR_i = mean(.data[[value_in_range]], na.rm = TRUE), .groups = "drop") |>
    dplyr::summarise(TIR = mean(TIR_i), .groups = "drop") |>
    dplyr::pull(TIR)

  if (is.null(boot)) {
    return(list(est = TIR))
  } else {
    unique_ids <- unique(data[[id_col]])
    n_ids <- length(unique_ids)
    boot_TIR <- replicate(boot, {
      boot_id <- data.frame(ID = sample((unique_ids), n_ids, replace = T))
      colnames(boot_id) <- id_col
      boot_sample_temp <- boot_id |> dplyr::left_join(data, by = id_col, relationship = "many-to-many")
      count_boot <- NULL
      boot_sample <- boot_sample_temp |>
        dplyr::group_by(.data[[id_col]], .data[[time]]) |>
        dplyr::mutate(count_boot = dplyr::row_number()) |>
        dplyr::ungroup() |>
        dplyr::mutate(ID_boot = ifelse(count_boot > 1, paste0(.data[[id_col]], "BOOT", count_boot), .data[[id_col]])) |>
        dplyr::select(-count_boot)
      naive_est(boot_sample, min_time, max_time, boot = NULL, "ID_boot", time, value_in_range)$est
    })
    return(list(
      est = TIR,
      `std err` = sd(boot_TIR),
      `CI 025` = quantile(boot_TIR, 0.025),
      `CI 975` = quantile(boot_TIR, 0.975),
      boot_TIR = boot_TIR
    ))
  }
}


#' @title Computes the proposed estimator for mean Time in Range (TIR) under the assumption of noninformative follow-up duration.
#' @description
#' This function estimates the mean TIR using a time-stratified averaging
#' approach. If bootstrapping is enabled, it computes confidence intervals
#' using resampling.
#'
#' @param data A data frame containing time-series data.
#' @param min_time The minimum time threshold for filtering (default = 0).
#' @param max_time The maximum time threshold for filtering (default = 10075, equivalent to 1440 * 7 - 5).
#' @param boot Number of bootstrap iterations (default = NULL, no bootstrapping).
#' @param id_col Column name identifying subjects (default = "patient_id").
#' @param time Column name representing time (default = "time").
#' @param value_in_range Column name of the binary indicator (0/1) for time in range (default = "value_in_range").
#' @return A list containing TIR estimation results, including the estimate, standard error, and confidence intervals.
#' @export
proposed_est_noninfo <- function(data,
                                 min_time = 0, max_time = (1440 * 7 - 5),
                                 boot = NULL,
                                 id_col = "patient_id", time = "time", value_in_range = "value_in_range") {
  data <- data[data[[time]] <= max_time & data[[time]] >= min_time, ]
  TIR <- data |>
    dplyr::group_by(.data[[time]]) |>
    dplyr::summarise(avg_val = mean(value_in_range, na.rm = TRUE), .groups = "drop") |>
    dplyr::summarise(TIR = mean(avg_val), .groups = "drop") |>
    dplyr::pull(TIR)

  if (is.null(boot)) {
    return(list(est = TIR))
  } else {
    unique_ids <- unique(data[[id_col]])
    n_ids <- length(unique_ids)
    boot_TIR <- replicate(boot, {
      boot_id <- data.frame(ID = sample((unique_ids), n_ids, replace = T))
      colnames(boot_id) <- id_col
      boot_sample_temp <- boot_id |> dplyr::left_join(data, by = id_col, relationship = "many-to-many")
      count_boot <- NULL
      boot_sample <- boot_sample_temp |>
        dplyr::group_by(.data[[id_col]], .data[[time]]) |>
        dplyr::mutate(count_boot = dplyr::row_number()) |>
        dplyr::ungroup() |>
        dplyr::mutate(ID_boot = ifelse(count_boot > 1, paste0(.data[[id_col]], "BOOT", count_boot), .data[[id_col]])) |>
        dplyr::select(-count_boot)
      proposed_est_noninfo(boot_sample, min_time, max_time, boot = NULL, "ID_boot", time, value_in_range)$est
    })
    return(list(
      est = TIR,
      `std err` = sd(boot_TIR),
      `CI 025` = quantile(boot_TIR, 0.025),
      `CI 975` = quantile(boot_TIR, 0.975),
      boot_TIR = boot_TIR
    ))
  }
}


#' @title Computes the proposed estimator for mean Time in Range (TIR) using a Cox model.
#' @description
#' This function estimates TIR using a Cox proportional hazards model, weighting observations
#' based on their predicted survival. If bootstrapping is enabled, confidence intervals are computed.
#'
#' @param data A data frame containing time-to-event data.
#' @param min_time The minimum time threshold for filtering (default = 0).
#' @param max_time The maximum time threshold for filtering (default = 10075, equivalent to 1440 * 7 - 5).
#' @param id_col Column name identifying subjects (default = "patient_id").
#' @param event_col Column name representing event occurrence (default = "event").
#' @param start_col Column name representing the start time (default = "time").
#' @param stop_col Column name representing the stop time (default = "time2").
#' @param formula Right-hand side of the Cox model formula as a character string (default = "var1").
#' @param boot Number of bootstrap iterations (default = NULL, no bootstrapping).
#' @param value_in_range Column name of the binary indicator (0/1) for time in range (default = "value_in_range").
#' @return A list containing TIR estimation results, including the estimate, standard error, and confidence intervals.
#' @export
proposed_est_cox <- function(data, min_time = 0, max_time = (1440 * 7 - 5),
                             id_col = "patient_id", event_col = "event",
                             start_col = "time", stop_col = "time2", formula = "var1",
                             boot = NULL, value_in_range = "value_in_range") {
  # data <- data.table::as.data.table(data)
  # Fit Cox model
  cox_fit <- survival::coxph(as.formula(paste0("survival::Surv(", start_col, ",", stop_col, ",", "event==1", ") ~ ", formula)), data = data, method="breslow")
  # Baseline cumulative hazard
  baseline_hazard <- survival::basehaz(cox_fit, centered = FALSE)
  # Add time and hazard difference
  baseline_hazard <- baseline_hazard |> dplyr::arrange(time) |> dplyr::mutate(hazard_diff = c(0, diff(hazard)))
  # Predict partial hazard
  data[, "predict_partial_hazard"] <- predict(cox_fit, newdata = data, type = "risk")
  # Merge cumulative hazard with dataset
  data <- merge(data, baseline_hazard, by.x = start_col, by.y = "time", all.x = TRUE)
  data <- data |>
    dplyr::mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))
  # Compute lambda_exp_diff
  data <- data |> dplyr::mutate(lambda_exp_diff = predict_partial_hazard * hazard_diff)
  data <- data |> dplyr::group_by(.data[[id_col]]) |>
    dplyr::mutate(cum_lambda_exp_diff = cumsum(lambda_exp_diff), weight = 1 / exp(-cum_lambda_exp_diff)) |> dplyr::ungroup()

  # Calculate TIR
  data <- data[data[[start_col]] <= max_time & data[[start_col]] >= min_time, ]
  TIR <- data |>
    dplyr::group_by(.data[[start_col]]) |>
    dplyr::summarise(weighted_avg = weighted.mean(.data[[value_in_range]], weight), .groups = "drop") |>
    dplyr::summarise(TIR = mean(weighted_avg), .groups = "drop") |>
    dplyr::pull(TIR)

  if (is.null(boot)) {
    return(list(est = TIR))
  } else {
    unique_ids <- unique(data[[id_col]])
    n_ids <- length(unique_ids)
    boot_TIR <- replicate(boot, {
      boot_id <- data.frame(ID = sample((unique_ids), n_ids, replace = T))
      colnames(boot_id) <- id_col
      boot_sample_temp <- boot_id |> dplyr::left_join(data, by = id_col, relationship = "many-to-many")
      count_boot <- NULL
      boot_sample <- boot_sample_temp |>
        dplyr::group_by(.data[[id_col]], .data[[start_col]]) |>
        dplyr::mutate(count_boot = dplyr::row_number()) |>
        dplyr::ungroup() |>
        dplyr::mutate(ID_boot = ifelse(count_boot > 1, paste0(.data[[id_col]], "BOOT", count_boot), .data[[id_col]])) |>
        dplyr::select(-count_boot, -.data[[id_col]], -predict_partial_hazard, -hazard, -hazard_diff, -lambda_exp_diff) |>
        dplyr::rename(patient_id = ID_boot)
      proposed_est_cox(boot_sample, min_time, max_time, "patient_id", event_col, start_col, stop_col, formula, boot = NULL, value_in_range)$est
    })
    return(list(
      est = TIR,
      `std err` = sd(boot_TIR),
      `CI 025` = quantile(boot_TIR, 0.025),
      `CI 975` = quantile(boot_TIR, 0.975),
      boot_TIR = boot_TIR
    ))
  }
}


#' @title Recursively rounds numeric values in nested lists, data frames, and tibbles.
#' @description
#' This function applies rounding to numeric values within a list, including nested structures.
#' If the input is a numeric vector, it rounds it directly. Non-numeric values remain unchanged.
#'
#' @param data A list, data frame, tibble, or numeric vector to be rounded.
#' @param decimals Number of decimal places to round to (default = 3).
#' @return The input data with all numeric values rounded.
#' @export
round_nested <- function(data, decimals = 3) {
  if (is.list(data)) {
    return(lapply(data, round_nested, decimals))
  } else if (is.numeric(data)) {
    return(round(data, decimals))
  }
  return(data)
}


#' @title Check if a number is within the specified range.
#' @description
#' Check if a number is within the specified range.
#' @param x A numeric value to check.
#' @param range A numeric vector of length 2 specifying the lower and upper bounds.
#' @return An integer: 1 if x is within the range, 0 otherwise.
#' @export

# value_in_range <- function(x, range = c(-.Machine$integer.max, .Machine$integer.max)) {
#   if (range[1] <= x & x <= range[2]) {
#     return(1)
#   } else {
#     return(0)
#   }
# }

value_in_range <- function(x, range = c(-.Machine$integer.max, .Machine$integer.max)) {
  if (is.na(x)) return(NA)
  if (range[1] <= x && x <= range[2]) 1 else 0
}


#' @title Print TIR Estimation Results
#' @description
#' Prints Time in Range (TIR) estimation results, ensuring numeric values are rounded.
#'
#' @param x An object of class "TIR" (a list containing estimation results).
#' @param decimals Number of decimal places to round to (default = 3).
#' @param ... Additional arguments (for compatibility with generic print method).
#' @export
#' @method print TIR
print.TIR <- function(x, decimals = 3, ...) {
  # Ensure x is of class "TIR"
  if (!inherits(x, "TIR")) {
    stop("Error: Object is not of class 'TIR'")
  }

  # Remove boot_TIR if it exists
  x$boot_TIR <- NULL

  # Round numeric values within the list
  rounded_x <- round_nested(x, decimals)

  # Convert to data frame for better printing
  DF <- as.data.frame(t(unlist(rounded_x)))  # Transpose for better display

  # Set row name
  rownames(DF) <- "meanTIR"

  # Print formatted output
  print(DF, row.names = TRUE)
}
