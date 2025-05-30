server <- function(input, output, session) {
  #### SECTION INPUT DATA ####
  # input data, data cleaning, output data
  # store raw files
  raw_CGM <- reactiveVal(NULL)
  raw_COV <- reactiveVal(NULL)

  observeEvent(input$CGMfile, {
    req(input$CGMfile)
    raw_CGM(read.csv(input$CGMfile$datapath, stringsAsFactors = FALSE))
  })

  observeEvent(input$COVfile, {
    req(input$COVfile)
    raw_COV(read.csv(input$COVfile$datapath, stringsAsFactors = FALSE))
  })

  # which file to preview
  file_to_preview <- reactiveVal(NULL)

  observeEvent(input$check_CGMfile, {
    file_to_preview(raw_CGM())
  })

  observeEvent(input$check_COVfile, {
    file_to_preview(raw_COV())
  })

  output$file_preview <- DT::renderDataTable({
    req(file_to_preview())
    DT::datatable(file_to_preview(), options = list(pageLength = 5))
  })
  # read in the data
  data_and_error <- eventReactive(input$load_data, {
    tryCatch({
      isolate({
        # Required inputs
        req(input$CGMfile, input$ID, input$Time, input$Glucose)
        id_col <- trimws(input$ID)
        time_col <- trimws(input$Time)
        glucose_col <- trimws(input$Glucose)

        # Handle time interval
        time_interval_input <- trimws(input$Time_Interval)
        time_interval <- as.numeric(strsplit(time_interval_input, ",")[[1]]) * 1440
        if (length(time_interval) != 2 || any(is.na(time_interval))) {
          stop("Invalid time interval. Use format like '1,4'.")
        }

        # Read CGM data
        cgm_data <- InpatCGM::read_CGM_data(
          file = input$CGMfile$datapath,
          ID = id_col,
          time = time_col,
          glucose = glucose_col,
          time_interval = time_interval
        )

        # Read covariates if provided
        if (!is.null(input$COVfile) && nzchar(input$COV_ID)) {
          covariates <- InpatCGM::read_covariate_data(
            file = input$COVfile$datapath,
            ID = input$COV_ID,
            covariate = NULL  # Load all columns initially
          )

          # Optional: restrict covariate columns to specified subset
          if (isTRUE(input$specify_covariates) && nzchar(input$Covariates_specified)) {
            covariate_vec <- trimws(unlist(strsplit(input$Covariates_specified, "[ ,]+")))
            missing_covariates <- setdiff(covariate_vec, colnames(covariates))
            if (length(missing_covariates) > 0) {
              stop(paste0(
                "Error: The following covariates are missing in the covariate file: ",
                paste(missing_covariates, collapse = ", "),
                ". Please check."
              ))
            }
            covariates <- covariates[, c(input$COV_ID, covariate_vec), drop = FALSE]
          }

          # Merge CGM and covariates by input$COV_ID and input$ID
          cgm_data <- dplyr::inner_join(
            cgm_data, covariates,
            by = setNames(trimws(input$COV_ID), id_col)
          )
        }

        list(data = cgm_data, error = NULL)
      })
    }, error = function(e) {
      list(data = NULL, error = e$message)
    })
  })

  output$error_msg <- renderText({
    req(data_and_error())
    if (!is.null(data_and_error()$error)) {
      data_and_error()$error
    } else {
      ""
    }
  })

  # Showing the data
  output$data <- DT::renderDataTable({
    req(data_and_error())
    validate(
      need(is.null(data_and_error()$error), data_and_error()$error)
    )
    DT::datatable(data_and_error()$data) |>
    #DT::formatDate(columns = isolate(input$Time), method = "toLocaleString")
    DT::formatDate(columns = trimws(input$Time), method = "toLocaleString")

  })

  # Downloading the data
  output$downloaddata <- downloadHandler(
    filename = function() {
      "cleaned_data.csv"
    },
    content = function(file) {
      write.csv(data_and_error()$data, file, row.names = FALSE)
    }
  )

  #### SECTION meanTIR Estimation ####

  # Update stratification choices when covariate file is uploaded
  observe({
    req(data_and_error()$data)

    if (!is.null(input$COVfile) && nzchar(input$COV_ID)) {
      covariate_data <- InpatCGM::read_covariate_data(
        file = input$COVfile$datapath,
        ID = input$COV_ID,
        covariate = if (input$specify_covariates && nzchar(input$Covariates_specified)) {
          input$Covariates_specified
        } else {
          NULL
        }
      )

      # Select categorical variables with between 2 and 9 levels (for stratification)
      covariate_choices <- covariate_data |>
        dplyr::select(-all_of(input$COV_ID)) |>
        dplyr::select(where(~ dplyr::n_distinct(.) > 1 & dplyr::n_distinct(.) < 10)) |>
        names()

      updateSelectInput(session, "strat_var",
                        choices = covariate_choices,
                        selected = NULL)
    }
  })

  # Compute TIR (Time in Range) estimation
  TIR_result <- eventReactive(input$computeTIR, {
    dat <- data_and_error()$data
    req(dat)

    estTIR_range <- as.numeric(strsplit(input$estTIR_range, ",")[[1]])
    boot_value <- if (input$use_bootstrap) input$boot_num else NULL

    # Clarify model choices:
    # "NULL" = non-informative missingness
    # "Cox" = follow-up modeled by Cox proportional hazards
    selected_model <- if (input$model == "NULL") 'NULL' else input$model

    if (input$stratify) {
      strat_var <- input$strat_var
      req(strat_var)

      groups <- unique(dat[[strat_var]])
      results <- lapply(groups, function(g) {
        group_data <- dat[dat[[strat_var]] == g, ]

        group_data <- group_data[!is.na(group_data[[input$Glucose]]), ]
        required_cols <- c(input$ID, input$Glucose, "minute_enrollment")

        if (nrow(group_data) == 0) {
          return(list(group = g, result = NA, error = "Group has 0 rows after filtering."))
        }

        if (!all(required_cols %in% names(group_data))) {
          return(list(group = g, result = NA, error = paste("Missing required columns:",
                                                            paste(setdiff(required_cols, names(group_data)), collapse = ", "))))
        }

        est <- tryCatch({
          InpatCGM::estTIR(
            data = group_data,
            method = input$method,
            model = selected_model,
            time = c(-1e8, 1e8),
            range = estTIR_range,
            boot = boot_value,
            id = input$ID,
            glucose = input$Glucose,
            time_col = "minute_enrollment",
            formula = input$cox_formula
          )
        }, error = function(e) e)

        if (inherits(est, "error")) {
          return(list(group = g, result = NA, error = est$message))
        }

        return(list(group = g, result = est, error = NULL))
      })

      # Optional: Perform Wald-type test for equality of group TIRs
      test <- NULL
      if (input$use_bootstrap) {
        valid_results <- Filter(function(r) !is.null(r$result) && !is.null(r$result$boot_TIR), results)
        if (length(valid_results) == length(results)) {
          estimates <- sapply(valid_results, function(r) r$result$est)
          boot_TIR_list <- lapply(valid_results, function(r) r$result$boot_TIR)
          test <- InpatCGM::wald_test_TIR(estimates, boot_TIR_list)
        }
      }

      return(list(results = results, test = test))

    } else {
      # Unstratified analysis
      est <- InpatCGM::estTIR(
        data = dat,
        method = input$method,
        model = selected_model,
        time = c(-1e8, 1e8),
        range = estTIR_range,
        boot = boot_value,
        id = input$ID,
        glucose = input$Glucose,
        time_col = "minute_enrollment",
        formula = input$cox_formula
      )
      return(est)
    }
  })

  # Summary TIR table
  output$TIR_table <- renderTable({
    res <- TIR_result()

    if (input$stratify) {
      results <- lapply(res$results, function(r) {
        if (!is.null(r$error)) return(NULL)

        if (input$use_bootstrap && !is.null(r$result$`std err`)) {
          ci_str <- paste0("(", round(r$result$`CI 025`, 3), ", ", round(r$result$`CI 975`, 3), ")")
          data.frame(
            Group = r$group,
            TIR = round(r$result$est, 3),
            `Standard Error` = round(r$result$`std err`, 3),
            `95% Confidence Interval` = ci_str,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Group = r$group,
            TIR = round(r$result$est, 3),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        }
      })

      return(do.call(rbind, results))
    } else {
      r <- res
      if (input$use_bootstrap && !is.null(r$`std err`)) {
        ci_str <- paste0("(", round(r$`CI 025`, 3), ", ", round(r$`CI 975`, 3), ")")
        data.frame(
          Group = "All",
          TIR = round(r$est, 3),
          `Standard Error` = round(r$`std err`, 3),
          `95% Confidence Interval` = ci_str,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          Group = "All",
          TIR = round(r$est, 3),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
    }
  }, digits = 3)

  # Show error messages from stratified analysis
  output$TIR_errors <- renderPrint({
    res <- TIR_result()
    if (input$stratify) {
      for (r in res$results) {
        if (!is.null(r$error)) {
          cat("Group:", r$group, "\n")
          cat("Error:", r$error, "\n\n")
        }
      }
    }
  })

  # Print hypothesis test for group comparison
  output$TIR_test <- renderPrint({
    res <- TIR_result()
    if (!isTRUE(input$stratify) || !isTRUE(input$use_bootstrap)) return(invisible(NULL))
    if (is.null(res$test)) return(invisible(NULL))

    cat("Wald-type test for equality of mean TIRs across groups:\n")
    cat("  Null hypothesis: μ₁ = μ₂ = ... = μₖ\n")
    cat("  Test statistic:", round(res$test$statistic, 3), "\n")
    cat("  Degrees of freedom:", res$test$df, "\n")
    cat("  p-value:", format.pval(res$test$p.value, digits = 4), "\n")
  })

  # (Optional) Description of selected methods
  output$TIR_description <- renderUI({
    method_text <- switch(input$method,
                          "naive" = "Naive: Ignores missing data; estimates TIR from complete cases only.",
                          "proposed" = "Proposed: Uses a probabilistic model accounting for missingness (Yu, 2024)."
    )
    model_text <- switch(input$model,
                         "NULL" = "Model: Assumes missingness is non-informative.",
                         "Cox" = "Model: Models follow-up duration using Cox regression."
    )
    HTML(paste("<strong>Estimation Method:</strong>", method_text, "<br/>",
               "<strong>Missingness Model:</strong>", model_text))
  })

  #### SECTION IGP ####
  ## INPUT - select ID, selection target glucose range, selection smoothing kernel for the plot
  # input subject ID
  output$AGP_Subject <- renderUI({
    data = data_and_error()$data
    # default subject is the first subject in the data
    subject = data[1, input$ID]
    textInput('AGP_Subject', 'Enter Subject ID', value = subject)
  })

  # input target glucose range
  output$AGP_TargetGlucoseRange <- renderUI({
    textInput('AGP_TargetGlucoseRange', 'Enter Target Glucose Range', value = '70, 180')
  })

  # input smoothing coefficient for plot
  output$AGP_SmoothingCoefficient <- renderUI({
    numericInput('AGP_SmoothingCoefficient', 'Smoothing Coefficient (e.g., 0.3)', value = 0.3, min = 0.01, max = 1, step = 0.01)
  })

  # Reactive expression to safely parse AGP target glucose range
  agp_targetglucoserange <- reactive({
    req(input$AGP_TargetGlucoseRange)
    parts <- strsplit(as.character(input$AGP_TargetGlucoseRange), ",")[[1]]
    as.numeric(trimws(parts))
  })

  ## prepare data and make the statistics and plots
  # subset AGP data -- by subject id
  AGP_data <- reactive({
    req(input$AGP_Subject)  # Ensure input$AGP_Subject is available
    data <- data_and_error()$data  # Store reactive data only once

    # Validate that input$AGP_Subject exists in the dataset
    validate(
      need(input$AGP_Subject %in% data[[input$ID]], "Please check Subject ID")
    )

    # Filter and return data
    data[data[[input$ID]] == input$AGP_Subject, , drop = FALSE]
  })

  # show AGP_statistics
  AGP_Statistics <- reactive({
    time_interval <- as.numeric(strsplit(input$Time_Interval, ",")[[1]]) * 1440
    data <- AGP_data()
    InpatCGM::AGP_metrics(data = data, ID = input$ID, time = input$Time, glucose = input$Glucose, time_interval = time_interval, shiny = TRUE)
  })

  output$AGP_Statistics <- DT::renderDataTable({
    DT::datatable(AGP_Statistics(), options = list(dom = 't'), rownames = FALSE, colnames = "")
  })

  # show AGP_plotTIR
  AGP_plotTIR <- reactive({
    data <- AGP_data()
    InpatCGM::AGP_plotTIR(
      data = data,
      ID = input$ID,
      time = input$Time,
      glucose = input$Glucose,
      target_glucose = agp_targetglucoserange()
    )
  })

  output$AGP_plotTIR <- renderPlot({AGP_plotTIR()})

  # show AGP_plotDayAvg
  AGP_plotDayAvg <- reactive({
    data <- AGP_data()
    InpatCGM::AGP_plotAGP(
      data = data,
      ID = input$ID,
      time = input$Time,
      glucose = input$Glucose,
      target_glucose = agp_targetglucoserange(),
      span = input$AGP_SmoothingCoefficient
    )
  })

  output$AGP_plotDayAvg <- renderPlot({
    AGP_plotDayAvg()
  })

  # show AGP_plotDaily
  AGP_plotDaily <- reactive({
    data <- AGP_data()
    InpatCGM::AGP_plotDaily(
      data = data,
      ID = input$ID,
      time = input$Time,
      glucose = input$Glucose,
      target_glucose = agp_targetglucoserange()
    )
  })

  output$AGP_plotDaily <- renderPlot({
    AGP_plotDaily()
  })

  options(shiny.usecairo = T)

  # Save AGP report as PDF
  output$pdfAGP <- downloadHandler(
    filename = function() { paste0("AGP", ".pdf") },
    content = function(file) {
      cairo_pdf(filename = file, width = 14, height = 20, bg = "transparent")  # Adjusted width/height
      p <- gridExtra::grid.arrange(
        gridExtra::arrangeGrob(
          AGP_plotTIR(),
          gridExtra::tableGrob(AGP_Statistics(), rows = NULL),
          ncol = 2
        ),
        AGP_plotDayAvg(),
        AGP_plotDaily()
        # heights = c(1, 2)  # Adjust row heights
      )
      print(p)
      dev.off()
    }
  )

  # Save AGP report as PNG
  output$pngAGP <- downloadHandler(
    filename = function() { paste0("AGP", ".png") },
    content = function(file) {
      png(file, width = 2800, height = 4000, res = 300)  # Adjust resolution and aspect ratio
      p <- gridExtra::grid.arrange(
        gridExtra::arrangeGrob(
          AGP_plotTIR(),
          gridExtra::tableGrob(AGP_Statistics(), rows = NULL),
          ncol = 2
        ),
        AGP_plotDayAvg(),
        AGP_plotDaily()
        # heights = c(1, 2)  # Adjust row heights
      )
      print(p)
      dev.off()
    }
  )

  # output$htmlAGP <- downloadHandler(
  #   filename = function() { paste0("AGP", ".html") },
  #   content = function(file) {
  #     library(rmarkdown)
  #
  #     # Path to temporary R Markdown file
  #     tempReport <- tempfile(fileext = ".Rmd")
  #
  #     # Write the R Markdown content dynamically
  #     writeLines(
  #       c(
  #         "---",
  #         "title: 'AGP Report'",
  #         "output: html_document",
  #         "---",
  #         "```{r, echo=FALSE, warning=FALSE, message=FALSE}",
  #         "library(gridExtra)",
  #         "p <- gridExtra::grid.arrange(",
  #         "  gridExtra::arrangeGrob(",
  #         "    AGP_plotTIR(),",
  #         "    grid::grid.newpage(),",
  #         "    grid::grid.draw(gridExtra::tableGrob(AGP_Statistics(), rows = NULL)),",
  #         "    ncol = 2",
  #         "  ),",
  #         "  AGP_plotDayAvg(),",
  #         "  AGP_plotDaily(),",
  #         "  heights = c(1, 2)",
  #         ")",
  #         "print(p)",
  #         "```"
  #       ),
  #       tempReport
  #     )
  #
  #     # Render the report to HTML
  #     render(tempReport, output_file = file)
  #   }
  # )

  #### SECTION TIR Prediction ####

  # Reactive storage for selected covariates used in training
  training_covariates <- reactiveVal(NULL)

  # Reactive UI for selecting covariates from cleaned dataset
  output$training_covariate_selector <- renderUI({
    dat <- data_and_error()$data
    req(dat)

    # Exclude ID/time/glucose columns
    excluded_cols <- c(input$ID, input$Time, input$Glucose, "minute_enrollment", "day_enrollment")
    covariate_choices <- setdiff(names(dat), excluded_cols)

    checkboxGroupInput(
      inputId = "selected_training_covariates",
      label = "Choose Covariates for Training:",
      choices = covariate_choices,
      selected = covariate_choices
    )
  })

  # Observe when the user clicks "Train Model"
  observeEvent(input$train_model, {
    dat <- data_and_error()$data
    req(dat, input$selected_training_covariates)

    training_covariates(input$selected_training_covariates)

    showNotification("Model trained using selected covariates.", type = "message")
  })

  # Show required covariates used in training
  output$required_covariates_ui <- renderUI({
    req(training_covariates())
    textAreaInput(
      inputId = "covariate_list_display",
      label = NULL,
      value = paste(training_covariates(), collapse = ", "),
      rows = min(8, length(training_covariates())),
      width = "100%"
    )
  })

  # Reactive storage for uploaded covariate matrix
  pred_covariate_data <- reactiveVal(NULL)

  # Load uploaded covariate matrix
  observe({
    req(input$pred_covariate_file)
    df <- read.csv(input$pred_covariate_file$datapath, stringsAsFactors = FALSE)
    pred_covariate_data(df)
  })

  # Preview uploaded matrix
  output$pred_covariate_preview <- DT::renderDataTable({
    req(pred_covariate_data())
    DT::datatable(pred_covariate_data(), options = list(pageLength = 5))
  })

  # Make TIR prediction
  predicted_TIR <- eventReactive(input$predict_TIR, {
    req(pred_covariate_data(), data_and_error()$data, training_covariates())

    range_vals <- as.numeric(strsplit(input$pred_target_range, ",")[[1]])
    lower <- if (!is.na(range_vals[1])) range_vals[1] else 70
    upper <- if (!is.na(range_vals[2])) range_vals[2] else 180

    if (length(range_vals) != 2 || any(is.na(range_vals))) {
      showNotification("Invalid glucose range. Please enter two numbers like 70,180", type = "error")
      return(NULL)
    }

    tryCatch({
      InpatCGM::predictTIR(
        train_data = data_and_error()$data,
        test_covariates = pred_covariate_data(),
        id_col = input$ID,
        glucose_col = input$Glucose,
        covariates = training_covariates(),
        lower = lower,
        upper = upper
      )
    }, error = function(e) {
      showNotification(paste("Prediction failed:", e$message), type = "error")
      NULL
    })
  })

  # Show predicted results
  output$TIR_prediction_result <- DT::renderDataTable({
    req(predicted_TIR())
    pred_df <- predicted_TIR()$predictions

    # Round all numeric columns to 4 digits
    is_numeric <- sapply(pred_df, is.numeric)
    pred_df[is_numeric] <- lapply(pred_df[is_numeric], function(x) round(x, 4))

    DT::datatable(pred_df, options = list(pageLength = 5))
  })

  # Show variable importance
  output$top_important_vars <- renderTable({
    req(predicted_TIR())
    head(predicted_TIR()$importance, 5)
  })

  # Download predicted results
  output$download_prediction <- downloadHandler(
    filename = function() {
      paste0("predicted_TIR_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(predicted_TIR())
      write.csv(predicted_TIR()$predictions, file, row.names = FALSE)
    }
  )


}
