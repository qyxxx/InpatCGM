#' Calculate AGP Metrics for Continuous Glucose Monitoring (CGM) Data
#'
#' This function computes key Ambulatory Glucose Profile (AGP) metrics based on CGM data,
#' including subject ID, follow-up duration, evaluation window, CGM activity percentage,
#' average glucose, glucose management indicator (GMI), and coefficient of variation (CV).
#'
#' @param data A data frame containing CGM readings.
#' @param ID A character string specifying the column name representing the subject ID.
#' @param time A character string specifying the column name representing the timestamp of CGM readings.
#' @param glucose A character string specifying the column name representing glucose values.
#' @param time_interval A numeric vector of length 2 specifying the start and end times (in minutes) for the evaluation window.
#' @param target_glucose (Optional) A numeric value specifying a target glucose threshold. Defaults to NULL.
#' @param shiny A logical value indicating whether to return the output in a formatted table for Shiny applications. Defaults to FALSE.
#'
#' @return A named list containing the following AGP metrics:
#' \describe{
#'   \item{Subject ID}{The unique subject identifier.}
#'   \item{Start Date}{The first recorded CGM reading date.}
#'   \item{End Date}{The last recorded CGM reading date.}
#'   \item{Follow Up Duration}{The number of days between the first and last CGM readings.}
#'   \item{Evaluation Window}{The predefined time interval used for analysis.}
#'   \item{\% Time CGM is Active}{The percentage of available CGM readings within the evaluation window.}
#'   \item{Average Glucose}{The mean glucose level during the evaluation window.}
#'   \item{Glucose Management Indicator (GMI)}{An estimate of HbA1c derived from the mean glucose level.}
#'   \item{Coefficient of Variation (CV)}{A measure of glucose variability, calculated as the standard deviation divided by the mean glucose level.}
#' }
#'
#' If \code{shiny = TRUE}, the function returns a long-format data frame for use in Shiny applications, with two columns:
#' \code{"Metric"} (descriptive names of the metrics) and \code{"Value"} (formatted metric values).
#'
#' @examples
#' \dontrun{
#' # Example dataset
#' data <- data.frame(
#'   id = rep(1, 100),
#'   time = seq.POSIXt(from = as.POSIXct("2023-01-01 00:00"), by = "5 min", length.out = 100),
#'   glucose = rnorm(100, mean = 120, sd = 15)
#' )
#'
#' # Compute AGP metrics
#' AGP_metrics(data, ID = "id", time = "time", glucose = "glucose", time_interval = c(0, 1440))
#' }
#' @export
AGP_metrics <- function (data, ID, time, glucose, time_interval, target_glucose = NULL, shiny = FALSE) {
  # 1 subject id
  subject_id <- data |> dplyr::group_by(.data[[ID]]) |> dplyr::arrange(.data[[time]]) |> dplyr::slice_head(n=1) |> dplyr::pull(.data[[ID]])
  # 2 start date
  start_date <- data |> dplyr::group_by(.data[[ID]]) |> dplyr::arrange(.data[[time]]) |> dplyr::slice_head(n=1) |> dplyr::pull(.data[[time]])
  # 3 end date
  end_date <- data |> dplyr::group_by(.data[[ID]]) |> dplyr::arrange(.data[[time]]) |> dplyr::slice_tail(n=1) |> dplyr::pull(.data[[time]])
  # 4 follow up duration, for this subject
  # following codes may have issue - some subject have usuall CGM readings at very beginning and those readings would be removed
  # follow_up_duration <- data |> dplyr::group_by(.data[[ID]]) |>
  #  dplyr::summarise(follow_up_duration = as.numeric(max(time) - min(time), units = "days")) |> dplyr::pull(follow_up_duration)
  follow_up_duration <- ((data |> dplyr::group_by(.data[[ID]]) |> dplyr::arrange(.data[[time]]) |> dplyr::slice_tail(n=1) |> dplyr::pull(minute_enrollment)) - time_interval[1] + 5)/1440
  # 5 evaluation window,this is the time interval of interest, which defined when input data
  evaluation_window <- (time_interval[2] - time_interval[1] + 5)/1440
  # 6 this is the percent of available CGM readings within the target evaluation window
  cgm_active_percent <- (data |> dplyr::group_by(.data[[ID]]) |> dplyr::summarise(active_readings = dplyr::n()) |> dplyr::pull(active_readings))/((time_interval[2] - time_interval[1])/5 + 1) * 100
  # 7
  avg_glucose <- data |> dplyr::group_by(.data[[ID]]) |> dplyr::summarise(avg_glucose = mean(.data[[glucose]])) |> dplyr::pull(avg_glucose)
  # 8
  gmi <- 3.31 + 0.02392 * avg_glucose
  # 9
  sd_glucose <- data |> dplyr::group_by(.data[[ID]]) |> dplyr::summarise(sd_glucose = sd(.data[[glucose]])) |> dplyr::pull(sd_glucose)
  cv <- (sd_glucose/avg_glucose) * 100

  out = list("Subject ID" = subject_id,
             "Start Date" = as.Date(start_date),
             "End Date" = as.Date(end_date),
             "Follow Up Duration" = follow_up_duration,
             "Evaluation Window" = evaluation_window,
             "% Time CGM is Active" = cgm_active_percent,
             "Average Glucose" = avg_glucose,
             "Glucose Management Indicator (GMI)" = gmi,
             "Coefficient of Variation (CV)" = cv)

  if (shiny) {
    out <- as.data.frame(out, stringsAsFactors = FALSE) |>
      dplyr::transmute(
        "Subject ID" = as.character(`Subject.ID`),
        "Start Date" = as.character(`Start.Date`),
        "End Date" = as.character(`End.Date`),
        "Follow Up Duration" = paste(round(`Follow.Up.Duration`,2), "days"),
        "Evaluation Window" = paste(round(`Evaluation.Window`,2), "days"),
        "% Time CGM is Active" = paste0(round(`X..Time.CGM.is.Active`, 2), "%"),
        "Average Glucose" = paste(round(`Average.Glucose`), "mg/dL"),
        "Glucose Management Indicator (GMI)" = paste0(round(`Glucose.Management.Indicator..GMI.`, 2), "%"),
        "Coefficient of Variation (CV)" = paste0(round(`Coefficient.of.Variation..CV.`, 2), "%")
      ) |>
      tidyr::pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value")
  }

}

