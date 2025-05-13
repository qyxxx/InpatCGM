#' Predict Time-in-Range (TIR) Using Random Forest
#'
#' This function fits a random forest model on training CGM data to predict
#' the time-in-range (TIR) metric, defined as the proportion of glucose readings
#' within a target range (default: 70–180 mg/dL). The model is trained using
#' baseline covariates and applied to a new test dataset to generate predictions.
#'
#' @param train_data A long-format data frame containing glucose measurements,
#' patient identifiers, enrollment time, and covariates.
#' @param test_covariates A data frame of test data with baseline covariates for prediction.
#' Must include the same covariate names as used in training.
#' @param id_col Name of the column containing patient IDs. Default is `"patient_id"`.
#' @param glucose_col Name of the column containing glucose values. Default is `"Glucose"`.
#' @param covariates A character vector specifying the covariate columns to use for modeling.
#' @param lower Lower bound of the target glucose range. Default is `70`.
#' @param upper Upper bound of the target glucose range. Default is `180`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`predictions`}{A data frame with `patient_id` and predicted TIR.}
#'   \item{`importance`}{A data frame of variable importance scores from the random forest model.}
#' }
#'
#' @importFrom dplyr group_by summarise slice ungroup left_join select arrange desc
#' @importFrom grf regression_forest variable_importance predict
#' @examples
#' \dontrun{
#' result <- predictTIR(train_data = training_df,
#'                      test_covariates = test_df,
#'                      covariates = c("age", "sex", "bmi"))
#' head(result$predictions)
#' head(result$importance)
#' }
#' @export

predictTIR <- function(train_data,
                       test_covariates,
                       id_col = "patient_id",
                       glucose_col = "Glucose",
                       covariates,
                       lower = 70,
                       upper = 180) {
  # stopifnot(is.data.frame(train_data), is.data.frame(test_covariates))
  # !!!NOTE: temporarily use grf to implement random forest algorithm
  # ---- 1. Calculate TIR per subject from training data ----
  calculate_TIR <- function(glucose_vec, lower, upper) {
    mean(glucose_vec >= lower & glucose_vec <= upper, na.rm = TRUE)
  }

  TIR_data <- train_data |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::summarise(TIR = calculate_TIR(.data[[glucose_col]], lower, upper), .groups = "drop")

  # ---- 2. Extract baseline covariates ----
  baseline_covariates <- train_data |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  # ---- 3. Merge to create training dataset ----
  data_rf <- dplyr::left_join(baseline_covariates, TIR_data, by = id_col)

  # ---- 4. Prepare inputs for random forest ----
  y <- data_rf$TIR
  X <- data_rf[, covariates, drop = FALSE]

  # # Ensure column names exist
  # if (anyNA(colnames(X)) || ncol(X) != length(covariates)) {
  #   stop("Some covariate names do not match the training data columns.")
  # }

  # ---- 5. Fit random forest model ----
  rf_model <- grf::regression_forest(X, y)

  # ---- 6. Predict TIR on new covariates ----
  X_test <- test_covariates[, covariates, drop = FALSE]

  # # Optional: check for missing columns
  # missing_cols <- setdiff(covariates, names(test_covariates))
  # if (length(missing_cols) > 0) {
  #   stop(paste("Missing covariates in test data:", paste(missing_cols, collapse = ", ")))
  # }

  preds <- predict(rf_model, X_test)$predictions

  # ---- 7. Variable importance ----
  vimp <- grf::variable_importance(rf_model)
  importance_df <- data.frame(
    variable = colnames(X),
    importance = as.numeric(vimp)
  ) |>
    dplyr::arrange(dplyr::desc(importance))

  # ---- 8. Return predictions and variable importance ----
  result <- list(
    predictions = data.frame(
      patient_id = test_covariates[[id_col]],
      Predicted_TIR = preds
    ),
    importance = importance_df
  )

  return(result)
}
