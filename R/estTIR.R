#' @title Vectorized Value-in-Range Check
#' @description Checks whether a numeric vector falls within a specified range.
#' @param x A numeric vector.
#' @param range A numeric vector of length 2 indicating the lower and upper bounds.
#' @return An integer vector: 1 if in range, 0 if not, NA if input is NA.
#' @export
value_in_range_vec <- function(x, range = c(-.Machine$integer.max, .Machine$integer.max)) {
  ifelse(is.na(x), NA_integer_, as.integer(x >= range[1] & x <= range[2]))
}

#' @title Wrapper for Estimating Time in Range (TIR)
#' @description Estimates TIR using the specified method (naive or proposed) and model (NULL or cox).
#' @param data A data frame containing glucose data.
#' @param method Estimation method: "naive" or "proposed".
#' @param model Model type for proposed estimator: "NULL" or "cox".
#' @param time A numeric vector of length 2 specifying time window.
#' @param range Numeric vector of length 2 for glucose range.
#' @param boot Number of bootstrap replicates.
#' @param id Subject ID column name.
#' @param glucose Glucose value column name.
#' @param time_col Time variable column name.
#' @param period Time offset for Cox model (default: 5).
#' @param formula Formula string for Cox model.
#' @return A list with TIR estimate, standard error, confidence intervals, and optional bootstrap samples.
#' @export
estTIR <- function(data, method = "proposed", model = "NULL",
                   time = c(0, 1440 * 7 - 5), range = c(70, 180),
                   boot = NULL, id = "patient_id", glucose = "glucose",
                   time_col = "time", period = 5, formula = "var1") {
  data <- data |> dplyr::mutate(value_in_range = value_in_range_vec(.data[[glucose]], range = range))

  data <- data |> dplyr::group_by(.data[[id]]) |>
    dplyr::mutate(event = dplyr::if_else(.data[[time_col]] == max(.data[[time_col]]), 1L, 0L)) |>
    dplyr::ungroup()

  data$time2 <- data[[time_col]] + period

  if (method == "naive") {
    if (model == "NULL") {
      return(naive_est(data, min_time = time[1], max_time = time[2], boot = boot,
                       id_col = id, time = time_col, value_in_range = "value_in_range"))
    } else {
      stop("Invalid model type for naive method")
    }
  } else if (method == "proposed") {
    if (model == "NULL") {
      return(proposed_est_noninfo(data, min_time = time[1], max_time = time[2], boot = boot,
                                  id_col = id, time = time_col, value_in_range = "value_in_range"))
    } else if (model == "Cox") {
      return(proposed_est_cox(data, min_time = time[1], max_time = time[2], boot = boot,
                              id_col = id, event_col = "event",
                              start_col = time_col, stop_col = "time2",
                              formula = formula, value_in_range = "value_in_range"))
    } else {
      stop("Invalid model type for proposed method")
    }
  } else {
    stop("Method must be 'naive' or 'proposed'")
  }
}

#' @title Naive Estimator for Time in Range (TIR)
#' @description Computes the mean Time in Range (TIR) across subjects using a naive estimator. Optionally supports bootstrapping.
#' @param data A data frame containing glucose readings.
#' @param min_time Minimum time value to filter data.
#' @param max_time Maximum time value to filter data.
#' @param boot Number of bootstrap replicates (default: NULL).
#' @param id_col Column name for subject ID.
#' @param time Column name for time variable.
#' @param value_in_range Column name for 0/1 indicator of glucose within range.
#' @return A list with estimated TIR, optional bootstrap standard error and confidence intervals.
#' @export
naive_est <- function(data, min_time = 0, max_time = (1440 * 7 - 5),
                      boot = NULL, id_col = "patient_id", time = "time", value_in_range = "value_in_range") {
  data <- data[data[[time]] <= max_time & data[[time]] >= min_time, ]

  mean_TIR <- mean(
    data |>
      dplyr::group_by(.data[[id_col]]) |>
      dplyr::summarise(TIR_i = mean(.data[[value_in_range]], na.rm = TRUE), .groups = "drop") |>
      dplyr::pull(TIR_i),
    na.rm = TRUE
  )

  if (is.null(boot)) return(list(est = mean_TIR))

  unique_ids <- unique(data[[id_col]])
  n_ids <- length(unique_ids)

  boot_TIR <- parallel::mclapply(seq_len(boot), function(b) {
    boot_ids <- data.frame(ID = sample(unique_ids, n_ids, replace = TRUE))
    names(boot_ids) <- id_col
    boot_sample <- merge(boot_ids, data, by = id_col, all.x = TRUE)

    boot_sample <- boot_sample |>
      dplyr::group_by(.data[[id_col]], .data[[time]]) |>
      dplyr::mutate(count_boot = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::mutate(ID_boot = ifelse(count_boot > 1, paste0(.data[[id_col]], "BOOT", count_boot), .data[[id_col]])) |>
      dplyr::select(-count_boot)

    naive_est(boot_sample, min_time, max_time, boot = NULL, id_col = "ID_boot", time = time, value_in_range = value_in_range)$est
  }, mc.cores = max(1, parallel::detectCores() - 1))

  boot_TIR <- unlist(boot_TIR)
  boot_TIR <- na.omit(boot_TIR)
  list(est = mean_TIR,
       `std err` = sd(boot_TIR),
       `CI 025` = quantile(boot_TIR, 0.025),
       `CI 975` = quantile(boot_TIR, 0.975),
       boot_TIR = boot_TIR)
}

#' @title Proposed Estimator (Non-informative Follow-up)
#' @description Computes the mean TIR using time-stratified averaging. Supports optional bootstrapping.
#' @param data A data frame of glucose time-series.
#' @param min_time Minimum time threshold.
#' @param max_time Maximum time threshold.
#' @param boot Number of bootstrap replicates.
#' @param id_col Column for subject ID.
#' @param time Time column.
#' @param value_in_range Column name for 0/1 indicator for in-range glucose.
#' @return A list containing TIR estimate, standard error, and confidence intervals.
#' @export
proposed_est_noninfo <- function(data, min_time = 0, max_time = (1440 * 7 - 5),
                                 boot = NULL, id_col = "patient_id", time = "time", value_in_range = "value_in_range") {
  data <- data[data[[time]] <= max_time & data[[time]] >= min_time, ]

  TIR <- mean(
    data |>
      dplyr::group_by(.data[[time]]) |>
      dplyr::summarise(avg_val = mean(.data[[value_in_range]], na.rm = TRUE), .groups = "drop") |>
      dplyr::pull(avg_val),
    na.rm = TRUE
  )

  if (is.null(boot)) return(list(est = TIR))

  unique_ids <- unique(data[[id_col]])
  n_ids <- length(unique_ids)

  boot_TIR <- parallel::mclapply(seq_len(boot), function(b) {
    boot_ids <- data.frame(ID = sample(unique_ids, n_ids, replace = TRUE))
    names(boot_ids) <- id_col
    boot_sample <- merge(boot_ids, data, by = id_col, all.x = TRUE)

    boot_sample <- boot_sample |>
      dplyr::group_by(.data[[id_col]], .data[[time]]) |>
      dplyr::mutate(count_boot = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::mutate(ID_boot = ifelse(count_boot > 1, paste0(.data[[id_col]], "BOOT", count_boot), .data[[id_col]])) |>
      dplyr::select(-count_boot)

    proposed_est_noninfo(boot_sample, min_time, max_time, boot = NULL, id_col = "ID_boot", time = time, value_in_range = value_in_range)$est
  }, mc.cores = max(1, parallel::detectCores() - 1))

  boot_TIR <- unlist(boot_TIR)
  boot_TIR <- na.omit(boot_TIR)
  list(est = TIR,
       `std err` = sd(boot_TIR),
       `CI 025` = quantile(boot_TIR, 0.025),
       `CI 975` = quantile(boot_TIR, 0.975),
       boot_TIR = boot_TIR)
}

#' @title Proposed Estimator Using Cox Model
#' @description Estimates Time in Range using survival-weighted averaging based on a Cox model for dropout. Supports bootstrapping.
#' @param data A data frame of glucose and survival information.
#' @param min_time Minimum time value for filtering.
#' @param max_time Maximum time value for filtering.
#' @param id_col Subject ID column.
#' @param event_col Event column for Cox model.
#' @param start_col Start time column.
#' @param stop_col Stop time column.
#' @param formula Right-hand side formula for Cox model.
#' @param boot Number of bootstrap iterations.
#' @param value_in_range Column name for binary glucose range indicator.
#' @return A list with TIR estimate, standard error, and confidence intervals.
#' @export
proposed_est_cox <- function(data, min_time = 0, max_time = (1440 * 7 - 5),
                             id_col = "patient_id", event_col = "event",
                             start_col = "minute_enrollment", stop_col = "time2",
                             formula = "var1", boot = NULL,
                             value_in_range = "value_in_range", period = 5) {
  surv_formula <- as.formula(paste0("survival::Surv(", start_col, ",", stop_col, ",", event_col, ") ~ ", formula))

  cox_fit <- survival::coxph(surv_formula, data = data, method = "breslow",
                             control = survival::coxph.control(iter.max = 50))

  basehaz <- survival::basehaz(cox_fit, centered = FALSE) |>
    dplyr::arrange(time) |>
    dplyr::mutate(hazard_diff = c(0, diff(hazard)))

  idx <- findInterval(data[[start_col]], basehaz$time)
  idx[idx == 0] <- 1  # ensure valid index

  data$hazard <- basehaz$hazard[idx]
  data$hazard_diff <- basehaz$hazard_diff[idx]
  data$predict_partial_hazard <- predict(cox_fit, newdata = data, type = "risk")

  data <- data |>
    dplyr::mutate(across(c(predict_partial_hazard, hazard_diff), ~ tidyr::replace_na(.x, 0)),
                  lambda_exp_diff = predict_partial_hazard * hazard_diff) |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::mutate(cum_lambda_exp_diff = cumsum(lambda_exp_diff),
                  weight = 1 / exp(-cum_lambda_exp_diff)) |>
    dplyr::ungroup()

  data <- data[data[[start_col]] <= max_time & data[[start_col]] >= min_time, ]

  TIR <- mean(
    data |>
      dplyr::group_by(.data[[start_col]]) |>
      dplyr::summarise(weighted_avg = weighted.mean(.data[[value_in_range]], weight), .groups = "drop") |>
      dplyr::pull(weighted_avg),
    na.rm = TRUE
  )

  if (is.null(boot)) return(list(est = TIR))

  unique_ids <- unique(data[[id_col]])
  n_ids <- length(unique_ids)

  boot_TIR <- parallel::mclapply(seq_len(boot), function(b) {
    tryCatch({
      boot_ids <- data.frame(ID = sample(unique_ids, n_ids, replace = TRUE))
      names(boot_ids) <- id_col
      boot_sample <- merge(boot_ids, data, by = id_col, all.x = TRUE)

      boot_sample <- boot_sample |>
        dplyr::group_by(.data[[id_col]], .data[[start_col]]) |>
        dplyr::mutate(count_boot = dplyr::row_number()) |>
        dplyr::ungroup() |>
        dplyr::mutate(ID_boot = ifelse(count_boot > 1,
                                       paste0(.data[[id_col]], "BOOT", count_boot),
                                       .data[[id_col]])) |>
        dplyr::select(-count_boot, -all_of(id_col)) |>
        dplyr::rename(patient_id = ID_boot)

      boot_sample[[stop_col]] <- boot_sample[[start_col]] + period

      proposed_est_cox(boot_sample, min_time, max_time,
                       id_col = "patient_id", event_col = event_col,
                       start_col = start_col, stop_col = stop_col,
                       formula = formula, boot = NULL,
                       value_in_range = value_in_range,
                       period = period)$est
    }, error = function(e) {
      message("Bootstrap replicate ", b, " failed: ", e$message)
      NA_real_
    })
  }, mc.cores = max(1, parallel::detectCores() - 1))

  boot_TIR <- unlist(boot_TIR)
  boot_TIR <- na.omit(boot_TIR)

  if (length(boot_TIR) == 0) {
    warning("All bootstrap replicates failed.")
    return(list(est = TIR, `std err` = NA, `CI 025` = NA, `CI 975` = NA, boot_TIR = NA))
  }

  list(est = TIR,
       `std err` = sd(boot_TIR),
       `CI 025` = quantile(boot_TIR, 0.025),
       `CI 975` = quantile(boot_TIR, 0.975),
       boot_TIR = boot_TIR)
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
