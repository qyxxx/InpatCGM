server <- function(input, output, session) {
  #### SECTION INPUT DATA ####
  # input data, data cleaning, output data
  # read in the data
  data_and_error <- eventReactive(input$load_data, {
    tryCatch({
      isolate({
        req(input$CGMfile)

        time_interval <- as.numeric(strsplit(input$Time_Interval, ",")[[1]]) * 1440

        cgm_data <- InpatCGM::read_CGM_data(
          file = input$CGMfile$datapath,
          ID = input$ID,
          time = input$Time,
          glucose = input$Glucose,
          time_interval = time_interval
        )

        if (!is.null(input$COVfile)) {
          covariates <- InpatCGM::read_covariate_data(
            file = input$COVfile$datapath,
            ID = input$ID,
            covariate = NULL  # <-- load full columns
          )

          if (input$specify_covariates && nzchar(input$Covariates_specified)) {
            covariate_vec <- unlist(strsplit(input$Covariates_specified, "[ ,]+"))
            covariate_vec <- trimws(covariate_vec)

            missing_covariates <- setdiff(covariate_vec, colnames(covariates))
            if (length(missing_covariates) > 0) {
              stop(paste0(
                "Error: The following covariates are missing in the covariate file: ",
                paste(missing_covariates, collapse = ", "),
                ". Please check."
              ))
            }

            covariates <- covariates[, c(input$ID, covariate_vec), drop = FALSE]
          }

          cgm_data <- dplyr::inner_join(cgm_data, covariates, by = input$ID)
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
      DT::formatDate(columns = isolate(input$Time), method = "toLocaleString")
  })

  # Downloading the data
  output$downloaddata <- downloadHandler(
    filename = function() {
      "cleaned_data.csv"
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )

  #### SECTION meanTIR Estimation ####
  observe({
    req(data())

    if (!is.null(input$COVfile)) {
      covariate_data <- InpatCGM::read_covariate_data(
        file = input$COVfile$datapath,
        ID = input$ID,
        covariate = if (input$specify_covariates && nzchar(input$Covariates_specified)) {
          input$Covariates_specified
        } else {
          NULL
        }
      )

      # Exclude ID and select covariates with 1 < unique levels < 10
      covariate_choices <- covariate_data |>
        dplyr::select(-all_of(input$ID)) |>
        dplyr::select(where(~ dplyr::n_distinct(.) > 1 & dplyr::n_distinct(.) < 10)) |>
        names()

      updateSelectInput(session, "strat_var",
                        choices = covariate_choices,
                        selected = NULL)
    }
  })

  # Compute TIR
  TIR_result <- eventReactive(input$computeTIR, {
    req(data())

    estTIR_range <- as.numeric(strsplit(input$estTIR_range, ",")[[1]])
    boot_value <- if (input$use_bootstrap) input$boot_num else NULL
    selected_model <- if (input$model == "NULL") 'NULL' else input$model

    dat <- data()

    if (input$stratify) {
      strat_var <- input$strat_var
      req(strat_var)

      # Split by groups
      groups <- unique(dat[[strat_var]])
      results <- lapply(groups, function(g) {
        group_data <- dat[dat[[strat_var]] == g, ]

        # Drop rows with missing glucose
        group_data <- group_data[!is.na(group_data[[input$Glucose]]), ]
        # Ensure required columns exist
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
            time = c(-100000000, 100000000),
            range = estTIR_range,
            boot = boot_value,
            id = input$ID,
            glucose = input$Glucose,
            time_col = "minute_enrollment",
            formula = input$cox_formula
          )
        }, error = function(e) e)

        # list(group = g, result = est)
        if (inherits(est, "error")) {
          return(list(group = g, result = NA, error = est$message))
        }

        return(list(group = g, result = est, error = NULL))
      })

      # === Wald test ===
      if (input$use_bootstrap) {
        valid_results <- Filter(function(r) !is.null(r$result) && !is.null(r$result$boot_TIR), results)

        if (length(valid_results) == length(results)) {
          estimates <- sapply(valid_results, function(r) r$result$est)
          boot_TIR_list <- lapply(valid_results, function(r) r$result$boot_TIR)
          test <- InpatCGM::wald_test_TIR(estimates, boot_TIR_list)
        } else {
          test <- NULL
        }
      } else {
        test <- NULL
      }

      return(list(results = results, test = test))

      # return(list(results = results))
      #
    } else {
      # Non-stratified
      est <- InpatCGM::estTIR(
        data = dat,
        method = input$method,
        model = selected_model,
        time = c(-100000000, 100000000),
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

  output$TIR_table <- renderTable({
    res <- TIR_result()

    if (input$stratify) {
      results <- lapply(res$results, function(r) {
        if (!is.null(r$error)) return(NULL)  # Skip groups with error

        if (input$use_bootstrap && !is.null(r$result$`std err`)) {
          ci_str <- paste0("(", round(r$result$`CI 025`, 3), ", ", round(r$result$`CI 975`, 3), ")")
          data.frame(
            Group = r$group,
            TIR = round(r$result$est, 3),
            'Standard Error' = round(r$result$`std err`, 3),
            '95% Confidence Interval' = ci_str,
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

      df <- do.call(rbind, results)
      return(df)

    } else {
      r <- res
      if (input$use_bootstrap && !is.null(r$`std err`)) {
        ci_str <- paste0("(", round(r$`CI 025`, 3), ", ", round(r$`CI 975`, 3), ")")
        data.frame(
          Group = "All",
          TIR = round(r$est, 3),
          'Standard Error' = round(r$`std err`, 3),
          '95% Confidence Interval' = ci_str,
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


  # print error message, if error exists
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

  # show results about hypothesis testing: same mean TIR among groups

  output$TIR_test <- renderPrint({
    res <- TIR_result()

    # Only show test result if stratified + bootstrapped + test exists
    if (!isTRUE(input$stratify) || !isTRUE(input$use_bootstrap)) {
      return(invisible(NULL))
    }

    if (is.null(res$test)) {
      return(invisible(NULL))
    }

    cat("Wald-type test for equality of mean TIRs across groups:\n")
    cat("  Null hypothesis: μ₁ = μ₂ = ... = μₖ\n")
    cat("  Test statistic:", round(res$test$statistic, 3), "\n")
    cat("  Degrees of freedom:", res$test$df, "\n")
    cat("  p-value:", format.pval(res$test$p.value, digits = 4), "\n")
  })

  #### SECTION AGP ####
  ## INPUT - select ID, selection target glucose range, selection smoothing kernel for the plot
  # input subject ID
  output$AGP_Subject <- renderUI({
    data = data()
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
    numericInput('AGP_SmoothingCoefficient', 'Enter Smoothing Coefficient for Plot', value = '0.3')
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
    data <- data()  # Store reactive data only once

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

}
