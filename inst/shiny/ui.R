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

                 # Section 1: Upload CGM File
                 tags$h4("1. Upload CGM File"),
                 fileInput("CGMfile", "Choose CGM File (.csv)", multiple = FALSE, accept = ".csv"),
                 actionButton("check_CGMfile", "Preview CGM Data"),

                 # Section 2: Specify CGM Column Names
                 tags$h4("2. Select CGM Column Names"),
                 textInput('ID', 'Subject ID Column', placeholder = "e.g., patient_id"),
                 textInput('Time', 'Timestamp Column', placeholder = "e.g., date_time"),
                 textInput('Glucose', 'Glucose Value Column', placeholder = "e.g., Glucose"),
                 textInput('Time_Interval', 'Time Interval in Days', placeholder = "e.g., 1,4"),
                 helpText("Make sure the column names match those in your uploaded CGM file."),

                 # Section 3: Optional Covariate File
                 tags$h4("3. (Optional) Upload Covariate File"),
                 fileInput("COVfile", "Choose Covariate File (.csv)", multiple = FALSE, accept = ".csv"),
                 actionButton("check_COVfile", "Preview Covariate File"),
                 textInput('COV_ID', 'Matching ID Column in Covariate File', placeholder = "e.g., patient_id"),
                 helpText("Must match the subject ID column in the CGM file for merging."),

                 # Section 4: Optional Column Selection for Output
                 tags$h4("4. (Optional) Limit Columns in Cleaned Dataset"),
                 checkboxInput("specify_covariates", "Select specific covariates to include in output?", value = FALSE),
                 conditionalPanel(
                   condition = "input.specify_covariates == true",
                   textInput("Covariates_specified", "Covariate Columns (comma-separated)", placeholder = "e.g., age, sex, bmi"),
                   helpText("This controls which columns appear in the merged dataset output. Futher analysis are based on this merged dataset.")
                 ),

                 # Section 5: Load and Download
                 tags$h4("5. Finalize and Export"),
                 actionButton("load_data", "Load and Process Data"),
                 downloadButton("downloaddata", "Download Cleaned Data")
               ),

               mainPanel(
                 verbatimTextOutput("error_msg"),
                 h4("Preview: Uploaded CGM File"),
                 DT::dataTableOutput("file_preview"),

                 h4("Preview: Merged CGM + Covariate Data"),
                 DT::dataTableOutput("data")
               )
             )
    ),

    # Panel - Mean TIR Estimation
    tabPanel("Mean TIR Estimation", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 tags$h4("1. Choose Estimation Method"),
                 selectInput("method", "Estimation Method",
                             choices = c("naive", "proposed"), selected = "proposed"),
                 helpText("naive = ignores missing data; proposed = probabilistic model (Yu, 2024)."),

                 tags$h4("2. Bootstrap Settings"),
                 checkboxInput("use_bootstrap", "Use Bootstrap?", value = FALSE),
                 conditionalPanel(
                   condition = "input.use_bootstrap == true",
                   numericInput("boot_num", "Number of Bootstrap Replicates", value = 20, min = 10),
                   helpText("Used to calculate standard error and confidence intervals.")
                 ),

                 tags$h4("3. Missingness Modeling (proposed only)"),
                 conditionalPanel(
                   condition = "input.method == 'proposed'",
                   selectInput("model", "Follow-up Duration Model",
                               choices = c("NULL", "cox"), selected = "NULL"),
                   helpText("NULL = assumes non-informative missingness; cox = models follow-up time via Cox regression."),

                   conditionalPanel(
                     condition = "input.model == 'cox'",
                     textInput("cox_formula", "Cox Model Formula (e.g., age + sex + bmi)", value = ""),
                     helpText("Specify covariates used to model dropout.")
                   )
                 ),

                 tags$h4("4. Glucose Range for TIR"),
                 textInput("estTIR_range", "Target Glucose Range (mg/dL)", value = "70, 180"),
                 helpText("Specify lower and upper bounds, comma-separated."),

                 tags$h4("5. Stratified Estimation (Optional)"),
                 checkboxInput("stratify", "Stratify by a variable?", value = FALSE),
                 conditionalPanel(
                   condition = "input.stratify == true",
                   selectInput("strat_var", "Stratification Variable", choices = NULL),
                   helpText("Only categorical variables with 2–9 levels are available.")
                 ),

                 tags$h4("6. Run Estimation"),
                 actionButton("computeTIR", "Compute TIR")
               ),

               mainPanel(
                 uiOutput("TIR_description"),
                 tags$h4("Estimated Time in Range"),
                 tableOutput("TIR_table"),

                 tags$h4("Errors (if any)"),
                 verbatimTextOutput("TIR_errors"),

                 tags$h4("Group Comparison Test (Stratified + Bootstrap)"),
                 verbatimTextOutput("TIR_test")
               )
             )
    ),

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
