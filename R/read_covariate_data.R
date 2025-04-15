#' @title Read Covariate Data from File
#' @description
#' This function reads a CSV file and extracts only the specified ID column and covariates of interest.
#'
#' @param file Path to the input CSV file (default: `input$COVfile$datapath`).
#' @param ID Name of the ID column (default: `input$ID`).
#' @param covariate Comma-separated string of covariates to select (default: NULL, use all covariates).
#'
#' @return A data frame containing only the selected ID and covariate columns.
#' @import dplyr
#' @export
read_covariate_data <- function(file,
                                ID,
                                covariate = NULL) {
  # Read the data
  data <- read.csv(file, stringsAsFactors = FALSE)

  # If covariates are specified, select ID and specified covariates
  if (!is.null(covariate) && nzchar(covariate)) {
    covariate_vec <- unlist(strsplit(covariate, "[ ,]+"))
    selected_cols <- unique(c(ID, covariate_vec))
    data <- data |> dplyr::select(dplyr::all_of(selected_cols))
  } else {
    # If covariates are not specified, select all columns
    # (i.e., keep entire dataset)
    data <- data
  }

  return(data)
}
