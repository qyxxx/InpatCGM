#' Read and Process CGM Data
#'
#' This function reads a CGM dataset, processes the time column, computes enrollment time in minutes and days,
#' rounds time intervals, removes duplicate and non-numeric glucose values, and filters data within a specified time interval.
#'
#' @param file Character. Path to the CSV file.
#' @param ID Character. Column name representing patient ID.
#' @param time Character. Column name representing date-time values.
#' @param glucose Character. Column name representing glucose readings.
#' @param time_interval Numeric vector of length 2. Time range in minutes for filtering data (default: c(1440, 5760)).
#'
#' @return A cleaned data frame with processed glucose readings.
#' @export
read_CGM_data <- function(file, ID, time, glucose, time_interval = c(1440, 5760)) {
  # Read in the data
  data <- read.csv(file, stringsAsFactors = FALSE)

  # Convert the time column to a date-time object
  data[[time]] <- as.POSIXct(data[[time]], format = "%m/%d/%Y %H:%M")

  # Calculate the time in minutes upon enrollment
  data <- data |>
    dplyr::group_by(.data[[ID]]) |>
    dplyr::arrange(.data[[time]]) |>
    dplyr::mutate(minute_enrollment = as.numeric(difftime(.data[[time]], dplyr::first(.data[[time]]), units = 'mins')))

  # Calculate the time in days upon enrollment
  data <- data |>
    dplyr::mutate(day_enrollment = as.integer(difftime(.data[[time]], dplyr::first(.data[[time]]), units = 'days')) + 1)

  # Round minute_enrollment to the nearest 5 minutes
  data$minute_enrollment <- round(data$minute_enrollment / 5) * 5

  # Remove duplicate rows with the same minute_enrollment per patient
  data <- data |>
    dplyr::group_by(.data[[ID]], minute_enrollment) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  # Filter out non-numeric glucose values
  data <- data |> dplyr::filter(is.numeric(.data[[glucose]]))

  # Remove out-of-bound glucose values
  data <- data |> dplyr::filter(.data[[glucose]] >= 40 & .data[[glucose]] <= 400)

  # Remove rows with missing glucose values
  data <- data |> dplyr::filter(!is.na(.data[[glucose]]))

  # Filter rows within the specified time interval
  data <- data |> dplyr::filter(minute_enrollment >= time_interval[1] & minute_enrollment <= time_interval[2] - 5)

  return(data)
}
