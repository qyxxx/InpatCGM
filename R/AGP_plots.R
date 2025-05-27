#' Plot Time in Range (TIR) for CGM Data
#'
#' This function generates a stacked bar plot visualizing the distribution of glucose levels
#' into three categories: "Low", "Target", and "High". The percentage of time spent in
#' each range is computed for each subject.
#'
#' @param data A data frame containing CGM readings.
#' @param ID A character string specifying the column name representing the subject ID.
#' @param time A character string specifying the column name representing the timestamp of CGM readings.
#' @param glucose A character string specifying the column name representing glucose values.
#' @param target_glucose A numeric vector of length 2 specifying the lower and upper bounds for the target glucose range (e.g., `c(70, 180)`).
#'
#' @return A ggplot object representing a stacked bar chart of glucose distribution.
#'
#' @examples
#' \dontrun{
#' # Example dataset
#' df <- data.frame(
#'   patient_id = rep(1:2, each = 100),
#'   date_time = rep(seq.POSIXt(from = Sys.time(), by = "5 min", length.out = 100), 2),
#'   Glucose = c(rnorm(100, mean = 120, sd = 30), rnorm(100, mean = 150, sd = 40))
#' )
#'
#' # Plot TIR
#' AGP_plotTIR(df, ID = "patient_id",
#' time = "date_time", glucose = "Glucose", target_glucose = c(70, 180))
#' }
#' @export
AGP_plotTIR<- function (data, ID, time, glucose, target_glucose){
  # Ensure required columns exist
  if (!all(c(ID, time, glucose) %in% colnames(data))) {
    stop("Data must contain specified ID, time, and glucose columns.")
  }

  # Categorize glucose levels
  data_AGP_plot <- data |>
    dplyr::mutate(
      glucose_category = dplyr::case_when(
        .data[[glucose]] < target_glucose[1] ~ "Low",
        .data[[glucose]] > target_glucose[2] ~ "High",
        TRUE ~ "Target"
      ),
      glucose_category = factor(glucose_category, levels = c("High", "Target", "Low")) # Correct stacking order
    )

  # Compute percentages per patient
  summary_stats <- data_AGP_plot |>
    dplyr::group_by(.data[[ID]], glucose_category) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data[[ID]]) |>
    dplyr::mutate(percentage = round(100 * count / sum(count), 1)) |>
    dplyr::ungroup()


  # Define colors for each category
  colors <- c(
    #"Very Low" = "darkred",
    "Low" = "red",
    "Target" = "green",
    "High" = "orange"
    #"Very High" = "brown"
  )

  # Create the stacked bar plot
  ggplot2::ggplot(summary_stats, ggplot2::aes(x = "Glucose Distribution", y = percentage, fill = glucose_category)) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(percentage, "%")), position = ggplot2::position_stack(vjust = 0.5), size = 6, color = "black") +
    #ggplot2::labs(title = "Glucose Level Distribution", x = NULL, y = "Percentage of Time", fill = "Glucose Category") +
    ggplot2::labs(title = NULL, x = NULL, y = NULL, fill = "Glucose Category") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "right"
    )

}



#' Plot Ambulatory Glucose Profile (AGP)
#'
#' This function generates an Ambulatory Glucose Profile (AGP) plot summarizing
#' glucose fluctuations over a 24-hour period. The plot includes the median, interquartile
#' range, and variability percentiles (5th-95th). It integrates CGMS2DayByDay functionality.
#'
#' @param data A data frame containing CGM readings.
#' @param ID A character string specifying the column name representing the subject ID.
#' @param time A character string specifying the column name representing the timestamp of CGM readings.
#' @param glucose A character string specifying the column name representing glucose values.
#' @param target_glucose A numeric vector of length 2 specifying the lower and upper bounds for the target glucose range (default: `c(70, 180)`).
#' @param smooth A logical indicating whether to smooth the glucose percentiles using LOESS (default: `TRUE`).
#' @param span A numeric value controlling the smoothness of LOESS fitting (default: `0.3`).
#' @param title A logical indicating whether to include the subject ID as the title (default: `FALSE`).
#'
#' @return A ggplot2 object representing an AGP plot.
#'
#' @export
AGP_plotAGP <- function(data, ID, time, glucose, target_glucose = c(70, 180),
                        smooth = TRUE, span = 0.3, title = FALSE) {

  # Ensure required columns exist
  if (!all(c(ID, time, glucose) %in% colnames(data))) {
    stop("Data must contain specified ID, time, and glucose columns.")
  }

  # Convert time column to POSIXct if necessary
  if (!lubridate::is.POSIXct(data[[time]])) {
    data[[time]] <- as.POSIXct(data[[time]], format = "%Y-%m-%d %H:%M:%S")
  }

  # Select the first subject if multiple IDs are present
  unique_subjects <- unique(data[[ID]])
  if (length(unique_subjects) > 1) {
    warning(paste("Multiple subjects found. Using only subject:", unique_subjects[1]))
    data <- data |> dplyr::filter(.data[[ID]] == unique_subjects[1])
  }

  # Interpolate CGM data onto a standard time grid
  data <- data[complete.cases(data), ]
  dt0 <- as.double(round(median(diff(data[[time]]), na.rm = TRUE)))
  ndays <- ceiling(as.double(difftime(max(data[[time]]), min(data[[time]]), units = "days")) + 1)
  minD <- as.POSIXct(format(min(data[[time]]), "%Y-%m-%d 00:00:00"))
  time_out <- seq(minD, length.out = ndays * (1440 / dt0), by = paste0(dt0, " min"))
  glucose_interp <- stats::approx(x = data[[time]], y = data[[glucose]], xout = time_out, rule = 2)$y
  glucose_matrix <- matrix(glucose_interp, nrow = ndays, byrow = TRUE)

  # Compute glucose percentiles
  quantiles <- apply(glucose_matrix, 2, quantile, probs = c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm = TRUE)

  # Prepare plot data
  times_numeric <- seq(dt0 * 60, 86400, by = dt0 * 60)
  if (smooth) {
    plot_data <- dplyr::tibble(
      times = hms::as_hms(times_numeric),
      median = stats::loess(quantiles[3, ] ~ times_numeric, span = span)$fitted,
      five = stats::loess(quantiles[1, ] ~ times_numeric, span = span)$fitted,
      twentyfive = stats::loess(quantiles[2, ] ~ times_numeric, span = span)$fitted,
      seventyfive = stats::loess(quantiles[4, ] ~ times_numeric, span = span)$fitted,
      ninetyfive = stats::loess(quantiles[5, ] ~ times_numeric, span = span)$fitted
    )
  } else {
    plot_data <- dplyr::tibble(
      times = hms::as_hms(times_numeric),
      median = quantiles[3, ],
      five = quantiles[1, ],
      twentyfive = quantiles[2, ],
      seventyfive = quantiles[4, ],
      ninetyfive = quantiles[5, ]
    )
  }

  # Create AGP plot (FIXED: Replaced size = with linewidth =)
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(ggplot2::aes(times, median), color = "black", linewidth = 1) +
    ggplot2::geom_line(ggplot2::aes(times, five), linetype = "longdash", color = "#325DAA") +
    ggplot2::geom_line(ggplot2::aes(times, ninetyfive), linetype = "longdash", color = "#325DAA") +
    ggplot2::geom_ribbon(ggplot2::aes(times, ymin = seventyfive, ymax = ninetyfive), fill = "#A7BEE7", alpha = 0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(times, ymin = five, ymax = twentyfive), fill = "#A7BEE7", alpha = 0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(times, ymin = twentyfive, ymax = seventyfive), fill = "#325DAA", alpha = 0.5) +
    ggplot2::geom_hline(yintercept = target_glucose[1], color = '#48BA3C') +
    ggplot2::geom_hline(yintercept = target_glucose[2], color = '#48BA3C') +
    ggplot2::scale_x_time(breaks = hms::as_hms(seq(0, 86400, by = 10800)),
                          labels = c('12 am', '3 am', '6 am', '9 am', '12 pm', '3 pm', '6 pm', '9 pm', '12 am')) +
    ggplot2::ylab("Glucose [mg/dL]") + ggplot2::xlab(NULL) +
    ggplot2::theme_minimal()

  # Add title if required
  if (title) {
    p <- p + ggplot2::ggtitle(paste("AGP Plot - Subject", unique_subjects[1]))
  }

  return(p)
}



#' Plot Daily Glucose Profiles for CGM Data with Gaps Handling
#'
#' This function generates a daily glucose profile plot using CGM data. The plot shows
#' glucose fluctuations across multiple days and highlights time spent in hypo- and hyperglycemic ranges.
#'
#' @param data A data frame containing CGM readings.
#' @param ID A character string specifying the column name representing the subject ID.
#' @param time A character string specifying the column name representing the timestamp of CGM readings.
#' @param glucose A character string specifying the column name representing glucose values.
#' @param maxd An integer specifying the maximum number of days to include in the plot (default: `14`).
#' @param target_glucose A numeric vector of length 2 specifying the lower and upper bounds for the target glucose range (default: `c(70, 180)`).
#' @param gap_threshold A numeric value (in minutes) specifying the threshold to break the line when missing data exceeds this gap (default: `45` minutes).
#'
#' @return A ggplot2 object representing the daily glucose profile plot.
#'
#' @export
AGP_plotDaily <- function(data, ID, time, glucose, maxd = 14, target_glucose = c(70, 180), gap_threshold = 30) {

  # Ensure required columns exist
  if (!all(c(ID, time, glucose) %in% colnames(data))) {
    stop("Data must contain specified ID, time, and glucose columns.")
  }

  # Convert time column to POSIXct if necessary
  if (!lubridate::is.POSIXct(data[[time]])) {
    data[[time]] <- as.POSIXct(data[[time]], format = "%Y-%m-%d %H:%M:%S")
  }

  # Select the first subject if multiple IDs are present
  unique_subjects <- unique(data[[ID]])
  if (length(unique_subjects) > 1) {
    warning(paste("Multiple subjects found. Using only subject:", unique_subjects[1]))
    data <- data |> dplyr::filter(.data[[ID]] == unique_subjects[1])
  }

  # Limit to the most recent `maxd` days
  days <- sort(unique(lubridate::date(data[[time]])))
  max_days <- min(length(days), maxd)
  start_day <- ifelse(max_days == maxd, length(days) - maxd + 1, 1)
  selected_days <- days[start_day:length(days)]

  # Process Data for Plotting
  plot_data <- data |>
    dplyr::mutate(each_day = lubridate::date(.data[[time]])) |>
    dplyr::filter(each_day %in% selected_days) |>
    dplyr::mutate(
      day_of_week = as.character(lubridate::wday(.data[[time]], label = TRUE, abbr = FALSE)),
      reltime = hms::as_hms(.data[[time]]),
      gap_time = difftime(.data[[time]], dplyr::lag(.data[[time]]), units = "mins"),
      gap_flag = ifelse(is.na(gap_time) | gap_time > gap_threshold, 1, 0),  # Flag gaps exceeding threshold
      segment = cumsum(gap_flag),  # Create separate segments for each non-missing streak
      gl_level = dplyr::case_when(
        .data[[glucose]] > target_glucose[2] ~ "hyper",
        .data[[glucose]] < target_glucose[1] ~ "hypo",
        TRUE ~ "normal"
      )
    )

  # Define glucose levels for highlighting hypo/hyper ranges
  gl_level <- plot_data |>
    dplyr::group_by(segment) |>
    dplyr::mutate(level_group = rep(1:length(rle(gl_level)$values), rle(gl_level)$lengths)) |>
    dplyr::group_by(level_group) |>
    dplyr::reframe(
      id = .data[[ID]][1],
      time = c(.data[[time]][1] - 10, .data[[time]], .data[[time]][dplyr::n()] + 10),
      reltime = hms::as_hms(c(reltime[1] - 10, reltime, reltime[dplyr::n()] + 10)),
      glucose = as.numeric(c(.data[[glucose]][1], .data[[glucose]], .data[[glucose]][dplyr::n()])),  # Ensure numeric
      day_of_week = c(day_of_week[1], day_of_week, day_of_week[dplyr::n()]),
      each_day = c(each_day[1], each_day, each_day[dplyr::n()]),
      class = gl_level[1], .groups = "drop"
    )

  # Ensure at least one row exists for each class (hyper & hypo)
  if (!any(gl_level$class == "hypo")) {
    gl_level <- dplyr::add_row(gl_level, gl_level[1, ])
    gl_level$class[1] <- "hypo"
    gl_level$glucose[1] <- target_glucose[1]
  }
  if (!any(gl_level$class == "hyper")) {
    gl_level <- dplyr::add_row(gl_level, gl_level[1, ])
    gl_level$class[1] <- "hyper"
    gl_level$glucose[1] <- target_glucose[2]
  }

  # Remove missing values
  plot_data <- plot_data[complete.cases(plot_data), ]

  # Create daily glucose plot
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(ggplot2::aes(reltime, .data[[glucose]], group = interaction(each_day, segment)), color = "black") +

    # Red shading for below target (hypo)
    ggplot2::geom_ribbon(data = gl_level[gl_level$class == "hypo", ],
                         ggplot2::aes(reltime, ymin = glucose, ymax = target_glucose[1]),
                         fill = "red", alpha = 0.5) +

    # Orange shading for above target (hyper)
    ggplot2::geom_ribbon(data = gl_level[gl_level$class == "hyper", ],
                         ggplot2::aes(reltime, ymin = target_glucose[2], ymax = glucose),
                         fill = "orange", alpha = 0.5) +

    # Green lines for target_glucose thresholds
    ggplot2::geom_hline(yintercept = target_glucose[1], color = 'green', linewidth = 1) +
    ggplot2::geom_hline(yintercept = target_glucose[2], color = 'green', linewidth = 1) +

    # Time axis
    ggplot2::scale_x_time(breaks = hms::as_hms(c('00:00:00', '12:00:00', '24:00:00')), labels = c('12 am', '12 pm', '12 am')) +

    # Facet by day
    ggplot2::facet_wrap(~each_day + day_of_week, ncol = 7) +

    # Labels and theme
    ggplot2::ylab("Glucose [mg/dL]") + ggplot2::xlab(NULL) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5))
}
