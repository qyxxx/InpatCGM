ui <- fluidPage(
  # title
  titlePanel("TIR Analysis of CGM in Hospital"),
  # Panels
  tabsetPanel(
    # panel - input data
    tabPanel("Data", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
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
    tabPanel("TIR Prediction", fluid = TRUE)
  )
)
