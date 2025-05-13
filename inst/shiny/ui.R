ui <- fluidPage(
  # title
  titlePanel("TIR Analysis of CGM in Hospital"),
  # Panels
  tabsetPanel(
    # panel - input data
    tabPanel("Data", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   style = "background-color: #ffe0e0; border: 1px solid red;",
                   tags$p(
                     style = "color: red; font-weight: bold;",
                     "⚠️ Important: Please do not upload sensitive or personally identifiable information (PII)."
                   )
                 ),
                 # 1. CGM file
                 fileInput("CGMfile", "Choose CGM File (.csv)", multiple = FALSE, accept = ".csv"),
                 actionButton("check_CGMfile", "Check CGM File"),

                 # 2. CGM columns
                 textInput('ID', 'Subject ID column', value = 'patient_id'),
                 textInput('Time', 'Timestamp column', value = 'date_time'),
                 textInput('Glucose', 'Glucose column', value = 'Glucose'),
                 textInput('Time_Interval', 'Time interval in days', value = '1,4'),

                 # 3. Covariate file (optional)
                 fileInput("COVfile", "Choose Covariate File (.csv)", multiple = FALSE, accept = ".csv"),
                 actionButton("check_COVfile", "Check Covariate File"),
                 # 3b. Covariate ID Column
                 textInput('COV_ID', 'Covariate File: ID column', value = 'patient_id'),

                 # 4. Covariate columns (optional)
                 checkboxInput("specify_covariates", "Specify covariates?", value = FALSE),
                 conditionalPanel(
                   condition = "input.specify_covariates == true",
                   textInput("Covariates_specified", "Covariates (comma-separated)", value = "")
                 ),

                 # 5. Load and download
                 actionButton("load_data", "Load Data"),
                 downloadButton("downloaddata", "Download Cleaned Data")
               ),

               mainPanel(
                 verbatimTextOutput("error_msg"),  # for showing errors
                 h4("File Preview"),
                 DT::dataTableOutput("file_preview"),
                 h4("Processed CGM + Covariate Data"),
                 DT::dataTableOutput("data")
               )
             )),

    # Panel - Mean TIR Estimation
    tabPanel("Mean TIR Estimation", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 # Choose estimation method
                 selectInput("method", "Select TIR Estimation Method", choices = c("naive", "proposed"), selected = "proposed"),

                 # Show Bootstrap option for both methods
                 conditionalPanel(
                   condition = "input.method == 'naive' || input.method == 'proposed'",
                   checkboxInput("use_bootstrap", "Use Bootstrap?", value = FALSE),
                   conditionalPanel(
                     condition = "input.use_bootstrap == true",
                     numericInput("boot_num", "Number of Bootstraps", value = 20, min = 10)
                   )
                 ),

                 # Model selection (only if method is "proposed")
                 conditionalPanel(
                   condition = "input.method == 'proposed'",
                   selectInput("model", "Select Follow-up Duration Model", choices = c("NULL", "cox"), selected = "NULL"),

                   # If "cox" model is selected, specify formula
                   conditionalPanel(
                     condition = "input.model == 'cox'",
                     textInput("cox_formula", "Enter Cox Model Formula (e.g., age + sex + bmi)", value = "")
                   )
                 ),

                 # Range selection
                 textInput("estTIR_range", "Specify Glucose Range (e.g., 70, 180)", value = "70, 180"),

                 # stratify option
                 # Stratify option
                 checkboxInput("stratify", "Stratify by a variable?", value = FALSE),

                 # Show only if stratify is TRUE
                 conditionalPanel(
                   condition = "input.stratify == true",
                   selectInput("strat_var", "Select Stratification Variable", choices = NULL)
                 ),

                 # Compute button
                 actionButton("computeTIR", "Compute TIR")
               ),

               mainPanel(
                 tableOutput("TIR_table"),
                 verbatimTextOutput("TIR_errors"),
                 verbatimTextOutput("TIR_test")
               )
             )),

    # panel - individual metrics - AGP
    tabPanel("IGP", fluid = TRUE,
             helpText("The inpatient glucose profile (IGP)"),
             sidebarLayout(
               sidebarPanel(
                 uiOutput("AGP_Subject"),
                 uiOutput("AGP_TargetGlucoseRange"),
                 uiOutput("AGP_SmoothingCoefficient"),
                 downloadButton(outputId = "pdfAGP", label = "pdf"),
                 downloadButton(outputId = "pngAGP", label = "png")
                 # downloadButton(outputId = "htmlAGP", label = "html")
               ),
               mainPanel(
                 fluidRow(
                   column(6, wellPanel("Percent CGM Readings in Ranges")),
                   column(6, wellPanel("CGM Statistics"))
                 ),
                 fluidRow(
                   column(6, plotOutput("AGP_plotTIR")),
                   column(6, DT::DTOutput("AGP_Statistics"))
                 ),
                 fluidRow(column(12, wellPanel("Inpatient Glucose Profile (IGP)"))),
                 fluidRow(column(12,  plotOutput("AGP_plotDayAvg"))),
                 fluidRow(column(12, wellPanel("Daily Glucose Profiles"))),
                 fluidRow(column(12, plotOutput("AGP_plotDaily")))
               )
             )
    ),
    # panel - TIR prediction
    tabPanel("TIR Prediction", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 h4("1. Upload Covariate File"),
                 fileInput("pred_covariate_file", "Choose Prediction Covariate File (.csv)", multiple = FALSE, accept = ".csv"),
                 actionButton("check_pred_covariate", "Check Uploaded File"),

                 br(),
                 h4("2. Confirm Required Covariates"),
                 helpText("Below is the list of covariates used to train the model. Your uploaded file must include these columns."),
                 uiOutput("required_covariates_ui"),

                 br(),
                 h4("3. Target Glucose Range"),
                 textInput("pred_target_range", "Enter Glucose Range (e.g., 70,180)", value = "70,180"),

                 br(),
                 h4("4. Make Prediction"),
                 actionButton("predict_TIR", "Predict TIR"),

                 br(), br(),
                 downloadButton("download_prediction", "Download Prediction Results")
               ),

               mainPanel(
                 h4("Uploaded Covariate Matrix Preview"),
                 DT::dataTableOutput("pred_covariate_preview"),

                 br(),
                 h4("Prediction Results"),
                 DT::dataTableOutput("TIR_prediction_result"),

                 br(),
                 h4("Top 5 Important Variables (from Training Data)"),
                 helpText("These variables are ranked based on their importance in the random forest model."),
                 tableOutput("top_important_vars")
               )
             )
    )
  )
)
