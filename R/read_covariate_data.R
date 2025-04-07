#' @title Read Covariate Data from File
#' @description
#' This function reads a CSV file and extracts only the specified ID column and covariates of interest.
#'
#' @param file Path to the input CSV file (default: `input$COVfile$datapath`).
#' @param ID Name of the ID column (default: `input$ID`).
#' @param covariate Comma-separated string of covariates to select (default: `input$Covariates_specified`).
#'
#' @return A data frame containing only the selected ID and covariate columns.
#' @import dplyr
#' @export
read_covariate_data <- function(file = input$COVfile$datapath,
                                ID = input$ID,
                                covariate = input$Covariates_specified) {
  # Read the data
  data <- read.csv(file, stringsAsFactors = FALSE)

  # Convert covariates input to a vector
  covariate_vec <- strsplit(covariate, "[ ,]+")[[1]]

  # Select relevant columns
  selected_data <- data |> dplyr::select(all_of(c(ID, covariate_vec)))

  return(selected_data)
}
