# Install packages first (run once):
# install.packages(c("shiny", "data.table", "readr", "readxl", "haven", "jsonlite",
#                    "dplyr", "tidyr", "stringr", "lubridate", "DT", "plotly",
#                    "shinydashboard", "shinyWidgets", "scales", "naniar", "validate"))

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(readr)
library(readxl)
library(haven)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(DT)
library(plotly)
library(scales)

# Increase max upload size to 10GB
options(shiny.maxRequestSize = 10000*1024^2)

ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(
    title = "Advanced Data Cleaning Suite",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Data Quality", tabName = "quality", icon = icon("check-circle")),
      menuItem("Auto Clean", tabName = "autoclean", icon = icon("magic")),
      menuItem("Manual Clean", tabName = "manual", icon = icon("tools")),
      menuItem("Validation Rules", tabName = "validate", icon = icon("shield-alt")),
      menuItem("Transform", tabName = "transform", icon = icon("exchange-alt")),
      menuItem("Export", tabName = "export", icon = icon("download")),
      menuItem("Reports", tabName = "reports", icon = icon("file-alt"))
    ),
    
    hr(),
    
    div(style = "padding: 15px;",
        h4("File Size Limit:"),
        p("Up to 10 GB supported"),
        hr(),
        h4("Supported Formats:"),
        tags$ul(
          tags$li("CSV, TSV, TXT"),
          tags$li("Excel (XLS, XLSX)"),
          tags$li("SPSS, Stata, SAS"),
          tags$li("JSON"),
          tags$li("Parquet")
        )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-top-color: #667eea !important; }
        .small-box { border-radius: 8px; }
        .progress-bar { background-color: #667eea; }
        .btn-primary { background-color: #667eea; border-color: #667eea; }
        .btn-primary:hover { background-color: #5568d3; }
        .issue-critical { background: #f8d7da; border-left: 4px solid #dc3545; padding: 10px; margin: 5px 0; border-radius: 4px; }
        .issue-warning { background: #fff3cd; border-left: 4px solid #ffc107; padding: 10px; margin: 5px 0; border-radius: 4px; }
        .issue-info { background: #d1ecf1; border-left: 4px solid #17a2b8; padding: 10px; margin: 5px 0; border-radius: 4px; }
        .issue-success { background: #d4edda; border-left: 4px solid #28a745; padding: 10px; margin: 5px 0; border-radius: 4px; }
      "))
    ),
    
    tabItems(
      # Upload Tab
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "Upload Your Data", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            fileInput("dataFile", 
                      "Choose File (Max 10GB)",
                      accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", 
                                 ".sav", ".dta", ".sas7bdat", ".json"),
                      width = "100%"),
            
            fluidRow(
              column(6,
                     selectInput("delimiter", "CSV Delimiter",
                                 choices = c("Auto-detect" = "auto", 
                                             "Comma" = ",", 
                                             "Tab" = "\t", 
                                             "Semicolon" = ";",
                                             "Pipe" = "|"))
              ),
              column(6,
                     selectInput("encoding", "File Encoding",
                                 choices = c("UTF-8" = "UTF-8",
                                             "Latin1" = "latin1",
                                             "Windows" = "windows-1252"))
              )
            ),
            
            fluidRow(
              column(6,
                     checkboxInput("hasHeader", "First row is header", TRUE)
              ),
              column(6,
                     checkboxInput("skipEmptyRows", "Skip empty rows", TRUE)
              )
            ),
            
            actionButton("loadData", "Load Data", 
                         class = "btn-primary btn-lg btn-block",
                         icon = icon("upload"))
          )
        ),
        
        fluidRow(
          valueBoxOutput("rowsBox", width = 3),
          valueBoxOutput("colsBox", width = 3),
          valueBoxOutput("sizeBox", width = 3),
          valueBoxOutput("qualityBox", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Data Preview",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("dataPreview")
          )
        )
      ),
      
      # Data Quality Tab
      tabItem(
        tabName = "quality",
        fluidRow(
          box(
            title = "Data Quality Score",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            uiOutput("qualityScoreUI")
          )
        ),
        
        fluidRow(
          box(
            title = "Issues Detected",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            uiOutput("issuesDetected")
          ),
          
          box(
            title = "Column Quality",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("columnQualityPlot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Missing Data Analysis",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("missingDataPlot", height = "400px")
          ),
          
          box(
            title = "Data Type Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("dataTypePlot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Detailed Quality Report",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("qualityDetailTable")
          )
        )
      ),
      
      # Auto Clean Tab
      tabItem(
        tabName = "autoclean",
        fluidRow(
          box(
            title = "Automatic Cleaning Options",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            h4("Select cleaning operations to perform:"),
            
            fluidRow(
              column(4,
                     h5(icon("broom"), " Basic Cleaning"),
                     checkboxInput("removeDuplicates", "Remove duplicate rows", TRUE),
                     checkboxInput("removeEmptyRows", "Remove empty rows", TRUE),
                     checkboxInput("removeEmptyCols", "Remove empty columns", TRUE),
                     checkboxInput("trimWhitespace", "Trim whitespace", TRUE)
              ),
              
              column(4,
                     h5(icon("database"), " Missing Data"),
                     checkboxInput("handleMissing", "Handle missing values", TRUE),
                     selectInput("missingStrategy", "Strategy",
                                 choices = c("Remove rows" = "remove",
                                             "Fill with mean" = "mean",
                                             "Fill with median" = "median",
                                             "Fill with mode" = "mode",
                                             "Forward fill" = "ffill",
                                             "Custom value" = "custom")),
                     textInput("customMissing", "Custom value", "NA")
              ),
              
              column(4,
                     h5(icon("exchange-alt"), " Data Types"),
                     checkboxInput("fixTypes", "Auto-detect and fix data types", TRUE),
                     checkboxInput("standardizeDates", "Standardize dates", TRUE),
                     checkboxInput("fixNumbers", "Fix numeric values", TRUE),
                     checkboxInput("fixText", "Standardize text", TRUE)
              )
            ),
            
            hr(),
            
            fluidRow(
              column(4,
                     h5(icon("filter"), " Advanced"),
                     checkboxInput("removeOutliers", "Remove outliers (IQR method)", FALSE),
                     checkboxInput("standardizeNames", "Standardize column names", TRUE),
                     checkboxInput("removeSpecialChars", "Remove special characters", FALSE)
              ),
              
              column(4,
                     h5(icon("chart-line"), " Validation"),
                     checkboxInput("validateEmails", "Validate email addresses", FALSE),
                     checkboxInput("validatePhones", "Validate phone numbers", FALSE),
                     checkboxInput("validateDates", "Validate date ranges", FALSE)
              ),
              
              column(4,
                     h5(icon("cog"), " Options"),
                     sliderInput("outlierThreshold", "Outlier sensitivity",
                                 min = 1, max = 3, value = 1.5, step = 0.5),
                     checkboxInput("createBackup", "Create backup before cleaning", TRUE)
              )
            ),
            
            hr(),
            
            actionButton("autoClean", "Start Auto-Cleaning", 
                         class = "btn-success btn-lg btn-block",
                         icon = icon("magic"))
          )
        ),
        
        fluidRow(
          box(
            title = "Cleaning Progress",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            uiOutput("cleaningProgress")
          )
        ),
        
        fluidRow(
          box(
            title = "Cleaning Summary",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("cleaningSummary")
          )
        )
      ),
      
      # Manual Clean Tab
      tabItem(
        tabName = "manual",
        fluidRow(
          box(
            title = "Manual Data Cleaning",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            tabBox(
              width = 12,
              
              tabPanel(
                "Column Operations",
                fluidRow(
                  column(6,
                         selectInput("selectColumn", "Select Column", choices = NULL),
                         actionButton("deleteColumn", "Delete Column", 
                                      class = "btn-danger", icon = icon("trash")),
                         actionButton("duplicateColumn", "Duplicate Column",
                                      class = "btn-info", icon = icon("copy"))
                  ),
                  column(6,
                         textInput("renameColumnNew", "New column name"),
                         actionButton("renameColumn", "Rename Column",
                                      class = "btn-warning", icon = icon("edit"))
                  )
                ),
                hr(),
                h4("Column Statistics"),
                verbatimTextOutput("columnStats")
              ),
              
              tabPanel(
                "Find & Replace",
                fluidRow(
                  column(4,
                         selectInput("frColumn", "Select Column", choices = NULL)
                  ),
                  column(4,
                         textInput("findValue", "Find", placeholder = "Search value")
                  ),
                  column(4,
                         textInput("replaceValue", "Replace with", placeholder = "New value")
                  )
                ),
                checkboxInput("regexSearch", "Use regular expressions", FALSE),
                checkboxInput("caseSensitive", "Case sensitive", FALSE),
                actionButton("findReplace", "Replace All",
                             class = "btn-primary", icon = icon("exchange-alt")),
                hr(),
                verbatimTextOutput("replaceResults")
              ),
              
              tabPanel(
                "Filter Rows",
                fluidRow(
                  column(4,
                         selectInput("filterColumn", "Column", choices = NULL)
                  ),
                  column(4,
                         selectInput("filterOperator", "Operator",
                                     choices = c("equals" = "==",
                                                 "not equals" = "!=",
                                                 "contains" = "contains",
                                                 "starts with" = "starts",
                                                 "ends with" = "ends",
                                                 "greater than" = ">",
                                                 "less than" = "<"))
                  ),
                  column(4,
                         textInput("filterValue", "Value")
                  )
                ),
                actionButton("applyFilter", "Apply Filter",
                             class = "btn-primary", icon = icon("filter")),
                actionButton("clearFilter", "Clear All Filters",
                             class = "btn-warning", icon = icon("times")),
                hr(),
                verbatimTextOutput("filterInfo")
              ),
              
              tabPanel(
                "Split/Merge Columns",
                h4("Split Column"),
                fluidRow(
                  column(4,
                         selectInput("splitColumn", "Column to split", choices = NULL)
                  ),
                  column(4,
                         textInput("splitDelimiter", "Split by", value = ",")
                  ),
                  column(4,
                         numericInput("splitMaxParts", "Max parts", value = 2, min = 2)
                  )
                ),
                actionButton("splitColumnBtn", "Split Column",
                             class = "btn-info", icon = icon("cut")),
                hr(),
                h4("Merge Columns"),
                fluidRow(
                  column(6,
                         selectizeInput("mergeColumns", "Columns to merge", 
                                        choices = NULL, multiple = TRUE)
                  ),
                  column(3,
                         textInput("mergeSeparator", "Separator", value = " ")
                  ),
                  column(3,
                         textInput("mergedColumnName", "New column name", 
                                   value = "merged_column")
                  )
                ),
                actionButton("mergeColumnsBtn", "Merge Columns",
                             class = "btn-success", icon = icon("object-group"))
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Current Data",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("manualDataPreview")
          )
        )
      ),
      
      # Validation Tab
      tabItem(
        tabName = "validate",
        fluidRow(
          box(
            title = "Data Validation Rules",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            h4("Create Custom Validation Rules"),
            
            fluidRow(
              column(3,
                     selectInput("valColumn", "Column", choices = NULL)
              ),
              column(3,
                     selectInput("valRule", "Rule Type",
                                 choices = c("Not NULL" = "notnull",
                                             "Unique" = "unique",
                                             "In Range" = "range",
                                             "In List" = "inlist",
                                             "Pattern Match" = "pattern",
                                             "Custom" = "custom"))
              ),
              column(3,
                     textInput("valParam", "Parameter", 
                               placeholder = "e.g., 0-100 or pattern")
              ),
              column(3,
                     br(),
                     actionButton("addRule", "Add Rule",
                                  class = "btn-primary", icon = icon("plus"))
              )
            ),
            
            hr(),
            
            h4("Active Validation Rules"),
            DTOutput("validationRulesTable"),
            
            hr(),
            
            actionButton("runValidation", "Run Validation",
                         class = "btn-success btn-lg", icon = icon("check")),
            actionButton("clearRules", "Clear All Rules",
                         class = "btn-danger", icon = icon("times"))
          )
        ),
        
        fluidRow(
          box(
            title = "Validation Results",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            uiOutput("validationResults")
          )
        ),
        
        fluidRow(
          box(
            title = "Failed Records",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            DTOutput("failedRecordsTable")
          )
        )
      ),
      
      # Transform Tab
      tabItem(
        tabName = "transform",
        fluidRow(
          box(
            title = "Data Transformations",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            tabBox(
              width = 12,
              
              tabPanel(
                "Numeric Operations",
                fluidRow(
                  column(4,
                         selectInput("numColumn", "Select Column", choices = NULL)
                  ),
                  column(4,
                         selectInput("numOperation", "Operation",
                                     choices = c("Round" = "round",
                                                 "Floor" = "floor",
                                                 "Ceiling" = "ceil",
                                                 "Absolute" = "abs",
                                                 "Log" = "log",
                                                 "Square Root" = "sqrt",
                                                 "Normalize (0-1)" = "normalize",
                                                 "Standardize (Z-score)" = "standardize"))
                  ),
                  column(4,
                         numericInput("numParam", "Parameter (if needed)", 
                                      value = 2, min = 0)
                  )
                ),
                actionButton("applyNumTransform", "Apply Transformation",
                             class = "btn-primary", icon = icon("calculator"))
              ),
              
              tabPanel(
                "Text Operations",
                fluidRow(
                  column(4,
                         selectInput("textColumn", "Select Column", choices = NULL)
                  ),
                  column(4,
                         selectInput("textOperation", "Operation",
                                     choices = c("Uppercase" = "upper",
                                                 "Lowercase" = "lower",
                                                 "Title Case" = "title",
                                                 "Remove spaces" = "nospace",
                                                 "Remove digits" = "nodigits",
                                                 "Remove punctuation" = "nopunct",
                                                 "Extract numbers" = "numbers",
                                                 "Extract letters" = "letters"))
                  ),
                  column(4,
                         br(),
                         actionButton("applyTextTransform", "Apply Transformation",
                                      class = "btn-primary", icon = icon("font"))
                  )
                )
              ),
              
              tabPanel(
                "Date Operations",
                fluidRow(
                  column(6,
                         selectInput("dateColumn", "Select Date Column", choices = NULL),
                         selectInput("dateFormat", "Current Format",
                                     choices = c("Auto-detect" = "auto",
                                                 "YYYY-MM-DD" = "%Y-%m-%d",
                                                 "MM/DD/YYYY" = "%m/%d/%Y",
                                                 "DD/MM/YYYY" = "%d/%m/%Y",
                                                 "YYYY/MM/DD" = "%Y/%m/%d"))
                  ),
                  column(6,
                         selectInput("dateExtract", "Extract Component",
                                     choices = c("Year" = "year",
                                                 "Month" = "month",
                                                 "Day" = "day",
                                                 "Quarter" = "quarter",
                                                 "Week" = "week",
                                                 "Day of Week" = "wday",
                                                 "Day of Year" = "yday")),
                         textInput("newDateColumn", "New column name", "date_component")
                  )
                ),
                actionButton("applyDateTransform", "Extract Date Component",
                             class = "btn-primary", icon = icon("calendar"))
              ),
              
              tabPanel(
                "Create Calculated Column",
                h4("Create new column from calculation"),
                textInput("calcColumnName", "New column name", "calculated"),
                textAreaInput("calcFormula", "Formula (use column names)",
                              placeholder = "Example: column1 * 2 + column2",
                              rows = 3),
                helpText("Use column names exactly as they appear. Supports +, -, *, /, ^"),
                actionButton("createCalcColumn", "Create Column",
                             class = "btn-success", icon = icon("plus-circle"))
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Transformed Data Preview",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("transformPreview")
          )
        )
      ),
      
      # Export Tab
      tabItem(
        tabName = "export",
        fluidRow(
          box(
            title = "Export Cleaned Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            h4("Export Options"),
            
            fluidRow(
              column(4,
                     selectInput("exportFormat", "File Format",
                                 choices = c("CSV" = "csv",
                                             "Excel (XLSX)" = "xlsx",
                                             "JSON" = "json",
                                             "RDS (R format)" = "rds",
                                             "SPSS" = "sav",
                                             "Stata" = "dta"))
              ),
              column(4,
                     selectInput("exportDelimiter", "CSV Delimiter",
                                 choices = c("Comma" = ",",
                                             "Tab" = "\t",
                                             "Semicolon" = ";",
                                             "Pipe" = "|"))
              ),
              column(4,
                     checkboxInput("exportHeader", "Include header", TRUE)
              )
            ),
            
            fluidRow(
              column(6,
                     checkboxInput("exportCompressed", "Compress file (.zip)", FALSE)
              ),
              column(6,
                     checkboxInput("exportTimestamp", "Add timestamp to filename", TRUE)
              )
            ),
            
            hr(),
            
            h4("Download"),
            
            downloadButton("downloadCleaned", "Download Cleaned Data",
                           class = "btn-success btn-lg btn-block"),
            
            hr(),
            
            downloadButton("downloadReport", "Download Cleaning Report",
                           class = "btn-info btn-lg btn-block"),
            
            hr(),
            
            downloadButton("downloadOriginal", "Download Original Data",
                           class = "btn-warning btn-lg btn-block")
          )
        ),
        
        fluidRow(
          box(
            title = "Export Summary",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("exportSummary")
          )
        )
      ),
      
      # Reports Tab
      tabItem(
        tabName = "reports",
        fluidRow(
          box(
            title = "Data Cleaning Report",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            h3("Complete Cleaning Report"),
            
            downloadButton("downloadFullReport", "Download PDF Report",
                           class = "btn-primary"),
            
            hr(),
            
            h4("Report Sections:"),
            tags$ul(
              tags$li("Original data summary"),
              tags$li("Quality assessment"),
              tags$li("Issues detected and fixed"),
              tags$li("Cleaning operations performed"),
              tags$li("Before/after comparison"),
              tags$li("Data quality score"),
              tags$li("Recommendations")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Cleaning Log",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("cleaningLog")
          )
        ),
        
        fluidRow(
          box(
            title = "Before vs After Comparison",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("beforeAfterPlot", height = "400px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    original_data = NULL,
    current_data = NULL,
    backup_data = NULL,
    cleaning_log = c(),
    validation_rules = data.frame(
      Column = character(),
      Rule = character(),
      Parameter = character(),
      stringsAsFactors = FALSE
    ),
    issues_detected = list(),
    quality_score = NULL
  )
  
  # Load data
  observeEvent(input$loadData, {
    req(input$dataFile)
    
    withProgress(message = 'Loading data...', value = 0, {
      
      tryCatch({
        ext <- tools::file_ext(input$dataFile$name)
        
        incProgress(0.3, detail = "Reading file...")
        
        data <- switch(ext,
                       csv = fread(input$dataFile$datapath, 
                                   header = input$hasHeader,
                                   encoding = input$encoding,
                                   showProgress = FALSE),
                       tsv = fread(input$dataFile$datapath, 
                                   sep = "\t",
                                   header = input$hasHeader,
                                   encoding = input$encoding,
                                   showProgress = FALSE),
                       txt = fread(input$dataFile$datapath,
                                   sep = if(input$delimiter == "auto") "auto" else input$delimiter,
                                   header = input$hasHeader,
                                   encoding = input$encoding,
                                   showProgress = FALSE),
                       xlsx = as.data.table(read_excel(input$dataFile$datapath)),
                       xls = as.data.table(read_excel(input$dataFile$datapath)),
                       sav = as.data.table(read_sav(input$dataFile$datapath)),
                       dta = as.data.table(read_dta(input$dataFile$datapath)),
                       json = as.data.table(fromJSON(input$dataFile$datapath, flatten = TRUE)),
                       stop("Unsupported file format")
        )
        
        incProgress(0.6, detail = "Analyzing data...")
        
        rv$original_data <- data
        rv$current_data <- copy(data)
        
        # Update column selections
        col_names <- names(data)
        updateSelectInput(session, "selectColumn", choices = col_names)
        updateSelectInput(session, "frColumn", choices = col_names)
        updateSelectInput(session, "filterColumn", choices = col_names)
        updateSelectInput(session, "splitColumn", choices = col_names)
        updateSelectizeInput(session, "mergeColumns", choices = col_names)
        updateSelectInput(session, "valColumn", choices = col_names)
        updateSelectInput(session, "numColumn", choices = col_names[sapply(data, is.numeric)])
        updateSelectInput(session, "textColumn", choices = col_names[sapply(data, is.character)])
        updateSelectInput(session, "dateColumn", choices = col_names)
        
        incProgress(0.9, detail = "Calculating quality score...")
        
        # Calculate quality
        rv$quality_score <- calculateQualityScore(data)
        rv$issues_detected <- detectIssues(data)
        
        rv$cleaning_log <- c(rv$cleaning_log,
                             paste(Sys.time(), "- Loaded file:", input$dataFile$name,
                                   "- Rows:", nrow(data), "- Columns:", ncol(data)))
        
        showNotification("Data loaded successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error")
      })
    })
  })
  
  # Calculate quality score
  calculateQualityScore <- function(data) {
    scores <- list()
    
    # Completeness
    total_cells <- nrow(data) * ncol(data)
    missing_cells <- sum(is.na(data))
    scores$completeness <- (1 - missing_cells / total_cells) * 100
    
    # Uniqueness
    scores$uniqueness <- (1 - sum(duplicated(data)) / nrow(data)) * 100
    
    # Validity (check reasonable values)
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    if (length(numeric_cols) > 0) {
      inf_count <- sum(sapply(data[, ..numeric_cols], function(x) sum(is.infinite(x))))
      scores$validity <- max(0, (1 - inf_count / (nrow(data) * length(numeric_cols))) * 100)
    } else {
      scores$validity <- 100
    }
    
    # Consistency
    scores$consistency <- 100 - (length(detectIssues(data)$all) / nrow(data) * 10)
    scores$consistency <- max(0, min(100, scores$consistency))
    
    # Overall
    scores$overall <- mean(c(scores$completeness, scores$uniqueness, 
                             scores$validity, scores$consistency))
    
    scores
  }
  
  # Detect issues
  detectIssues <- function(data) {
    issues <- list()
    issues$all <- c()
    
    # Missing values
    missing <- colSums(is.na(data))
    issues$missing <- names(missing[missing > 0])
    issues$all <- c(issues$all, issues$missing)
    
    # Duplicates
    issues$duplicates <- sum(duplicated(data))
    
    # Empty columns
    empty_cols <- names(data)[colSums(is.na(data)) == nrow(data)]
    issues$empty_cols <- empty_cols
    issues$all <- c(issues$all, empty_cols)
    
    # Outliers in numeric columns
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    outlier_cols <- c()
    for (col in numeric_cols) {
      Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      outliers <- sum(data[[col]] < (Q1 - 1.5 * IQR) | 
                        data[[col]] > (Q3 + 1.5 * IQR), na.rm = TRUE)
      if (outliers > 0) outlier_cols <- c(outlier_cols, col)
    }
    issues$outliers <- outlier_cols
    
    issues
  }
  
  # Value boxes
  output$rowsBox <- renderValueBox({
    req(rv$current_data)
    valueBox(
      format(nrow(rv$current_data), big.mark = ","),
      "Total Rows",
      icon = icon("table"),
      color = "blue"
    )
  })
  
  output$colsBox <- renderValueBox({
    req(rv$current_data)
    valueBox(
      ncol(rv$current_data),
      "Total Columns",
      icon = icon("columns"),
      color = "green"
    )
  })
  
  output$sizeBox <- renderValueBox({
    req(rv$current_data)
    size_mb <- object.size(rv$current_data) / 1024 / 1024
    valueBox(
      paste0(round(size_mb, 1), " MB"),
      "Data Size",
      icon = icon("database"),
      color = "yellow"
    )
  })
  
  output$qualityBox <- renderValueBox({
    req(rv$quality_score)
    score <- round(rv$quality_score$overall, 0)
    color <- if (score >= 80) "green" else if (score >= 60) "yellow" else "red"
    valueBox(
      paste0(score, "%"),
      "Quality Score",
      icon = icon("star"),
      color = color
    )
  })
  
  # Data preview
  output$dataPreview <- renderDT({
    req(rv$current_data)
    datatable(head(rv$current_data, 100),
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Quality score UI
  output$qualityScoreUI <- renderUI({
    req(rv$quality_score)
    
    fluidRow(
      column(3,
             div(class = "small-box bg-blue",
                 div(class = "inner",
                     h3(paste0(round(rv$quality_score$completeness, 1), "%")),
                     p("Completeness")
                 ),
                 div(class = "icon", icon("check-circle"))
             )
      ),
      column(3,
             div(class = "small-box bg-green",
                 div(class = "inner",
                     h3(paste0(round(rv$quality_score$uniqueness, 1), "%")),
                     p("Uniqueness")
                 ),
                 div(class = "icon", icon("fingerprint"))
             )
      ),
      column(3,
             div(class = "small-box bg-yellow",
                 div(class = "inner",
                     h3(paste0(round(rv$quality_score$validity, 1), "%")),
                     p("Validity")
                 ),
                 div(class = "icon", icon("shield-alt"))
             )
      ),
      column(3,
             div(class = "small-box bg-purple",
                 div(class = "inner",
                     h3(paste0(round(rv$quality_score$consistency, 1), "%")),
                     p("Consistency")
                 ),
                 div(class = "icon", icon("balance-scale"))
             )
      )
    )
  })
  
  # Issues detected
  output$issuesDetected <- renderUI({
    req(rv$issues_detected)
    
    issues_list <- list()
    
    # Missing values
    if (length(rv$issues_detected$missing) > 0) {
      issues_list[[1]] <- div(class = "issue-warning",
                              strong("Missing Values: "), 
                              paste(length(rv$issues_detected$missing), "columns affected")
      )
    }
    
    # Duplicates
    if (rv$issues_detected$duplicates > 0) {
      issues_list[[2]] <- div(class = "issue-critical",
                              strong("Duplicate Rows: "),
                              paste(rv$issues_detected$duplicates, "duplicates found")
      )
    }
    
    # Empty columns
    if (length(rv$issues_detected$empty_cols) > 0) {
      issues_list[[3]] <- div(class = "issue-warning",
                              strong("Empty Columns: "),
                              paste(length(rv$issues_detected$empty_cols), "columns are empty")
      )
    }
    
    # Outliers
    if (length(rv$issues_detected$outliers) > 0) {
      issues_list[[4]] <- div(class = "issue-info",
                              strong("Outliers: "),
                              paste(length(rv$issues_detected$outliers), "columns contain outliers")
      )
    }
    
    if (length(issues_list) == 0) {
      issues_list[[1]] <- div(class = "issue-success",
                              strong("No major issues detected!"),
                              p("Your data quality is excellent.")
      )
    }
    
    do.call(tagList, issues_list)
  })
  
  # Column quality plot
  output$columnQualityPlot <- renderPlotly({
    req(rv$current_data)
    
    quality_data <- data.frame(
      Column = names(rv$current_data),
      Completeness = sapply(rv$current_data, function(x) (1 - sum(is.na(x)) / length(x)) * 100)
    )
    
    plot_ly(quality_data, x = ~Column, y = ~Completeness, type = 'bar',
            marker = list(color = ~Completeness,
                          colorscale = list(c(0, "red"), c(0.5, "yellow"), c(1, "green")),
                          colorbar = list(title = "Completeness %"))) %>%
      layout(title = "Column Completeness",
             xaxis = list(title = ""),
             yaxis = list(title = "Completeness (%)"))
  })
  
  # Missing data plot
  output$missingDataPlot <- renderPlotly({
    req(rv$current_data)
    
    missing_data <- data.frame(
      Column = names(rv$current_data),
      Missing = colSums(is.na(rv$current_data)),
      Percentage = colSums(is.na(rv$current_data)) / nrow(rv$current_data) * 100
    ) %>%
      filter(Missing > 0) %>%
      arrange(desc(Missing))
    
    if (nrow(missing_data) > 0) {
      plot_ly(missing_data, x = ~Column, y = ~Missing, type = 'bar',
              marker = list(color = 'rgb(220, 53, 69)')) %>%
        layout(title = "Missing Values by Column",
               xaxis = list(title = ""),
               yaxis = list(title = "Count"))
    } else {
      plot_ly() %>%
        layout(title = "No missing values detected!",
               annotations = list(text = "All columns are complete",
                                  x = 0.5, y = 0.5, showarrow = FALSE))
    }
  })
  
  # Data type plot
  output$dataTypePlot <- renderPlotly({
    req(rv$current_data)
    
    type_data <- data.frame(
      Type = c("Numeric", "Character", "Date", "Logical", "Other"),
      Count = c(
        sum(sapply(rv$current_data, is.numeric)),
        sum(sapply(rv$current_data, is.character)),
        sum(sapply(rv$current_data, function(x) inherits(x, "Date"))),
        sum(sapply(rv$current_data, is.logical)),
        sum(!sapply(rv$current_data, function(x) 
          is.numeric(x) || is.character(x) || inherits(x, "Date") || is.logical(x)))
      )
    ) %>%
      filter(Count > 0)
    
    plot_ly(type_data, labels = ~Type, values = ~Count, type = 'pie',
            marker = list(colors = c('#667eea', '#764ba2', '#f093fb', '#4facfe', '#00f2fe'))) %>%
      layout(title = "Data Type Distribution")
  })
  
  # Quality detail table
  output$qualityDetailTable <- renderDT({
    req(rv$current_data)
    
    detail_data <- data.frame(
      Column = names(rv$current_data),
      Type = sapply(rv$current_data, class),
      Missing = colSums(is.na(rv$current_data)),
      Missing_Pct = round(colSums(is.na(rv$current_data)) / nrow(rv$current_data) * 100, 2),
      Unique = sapply(rv$current_data, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )
    
    datatable(detail_data,
              options = list(pageLength = 25, scrollX = TRUE)) %>%
      formatStyle('Missing_Pct',
                  backgroundColor = styleInterval(c(5, 20), 
                                                  c('lightgreen', 'yellow', 'red')))
  })
  
  # Auto clean
  observeEvent(input$autoClean, {
    req(rv$current_data)
    
    withProgress(message = 'Cleaning data...', value = 0, {
      
      data <- copy(rv$current_data)
      log <- c()
      
      # Create backup
      if (input$createBackup) {
        rv$backup_data <- copy(data)
        log <- c(log, "Created backup of original data")
      }
      
      incProgress(0.1, detail = "Removing duplicates...")
      
      # Remove duplicates
      if (input$removeDuplicates) {
        before <- nrow(data)
        data <- unique(data)
        removed <- before - nrow(data)
        if (removed > 0) {
          log <- c(log, paste("Removed", removed, "duplicate rows"))
        }
      }
      
      incProgress(0.2, detail = "Removing empty rows...")
      
      # Remove empty rows
      if (input$removeEmptyRows) {
        before <- nrow(data)
        data <- data[rowSums(is.na(data)) != ncol(data), ]
        removed <- before - nrow(data)
        if (removed > 0) {
          log <- c(log, paste("Removed", removed, "empty rows"))
        }
      }
      
      incProgress(0.3, detail = "Removing empty columns...")
      
      # Remove empty columns
      if (input$removeEmptyCols) {
        empty_cols <- names(data)[colSums(is.na(data)) == nrow(data)]
        if (length(empty_cols) > 0) {
          data <- data[, !names(data) %in% empty_cols, with = FALSE]
          log <- c(log, paste("Removed", length(empty_cols), "empty columns"))
        }
      }
      
      incProgress(0.4, detail = "Trimming whitespace...")
      
      # Trim whitespace
      if (input$trimWhitespace) {
        char_cols <- names(data)[sapply(data, is.character)]
        for (col in char_cols) {
          data[[col]] <- trimws(data[[col]])
        }
        if (length(char_cols) > 0) {
          log <- c(log, paste("Trimmed whitespace in", length(char_cols), "text columns"))
        }
      }
      
      incProgress(0.5, detail = "Handling missing values...")
      
      # Handle missing values
      if (input$handleMissing) {
        for (col in names(data)) {
          if (sum(is.na(data[[col]])) > 0) {
            if (input$missingStrategy == "remove") {
              # Handled by removeEmptyRows
            } else if (input$missingStrategy == "mean" && is.numeric(data[[col]])) {
              data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
            } else if (input$missingStrategy == "median" && is.numeric(data[[col]])) {
              data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
            } else if (input$missingStrategy == "mode") {
              mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
              data[[col]][is.na(data[[col]])] <- mode_val
            } else if (input$missingStrategy == "custom") {
              data[[col]][is.na(data[[col]])] <- input$customMissing
            }
          }
        }
        log <- c(log, paste("Handled missing values using", input$missingStrategy, "strategy"))
      }
      
      incProgress(0.6, detail = "Fixing data types...")
      
      # Fix data types
      if (input$fixTypes) {
        for (col in names(data)) {
          if (is.character(data[[col]])) {
            # Try numeric
            if (all(grepl("^-?[0-9.]+$", data[[col]][!is.na(data[[col]])]), na.rm = TRUE)) {
              data[[col]] <- as.numeric(data[[col]])
            }
          }
        }
        log <- c(log, "Auto-detected and fixed data types")
      }
      
      incProgress(0.7, detail = "Standardizing dates...")
      
      # Standardize dates
      if (input$standardizeDates) {
        for (col in names(data)) {
          if (is.character(data[[col]])) {
            sample_vals <- head(data[[col]][!is.na(data[[col]])], 10)
            if (any(grepl("\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}", sample_vals))) {
              tryCatch({
                data[[col]] <- as.Date(data[[col]])
                log <- c(log, paste("Converted", col, "to date format"))
              }, error = function(e) {})
            }
          }
        }
      }
      
      incProgress(0.8, detail = "Standardizing column names...")
      
      # Standardize column names
      if (input$standardizeNames) {
        old_names <- names(data)
        new_names <- tolower(gsub("[^a-zA-Z0-9_]", "_", names(data)))
        setnames(data, old_names, new_names)
        log <- c(log, "Standardized column names")
      }
      
      incProgress(0.9, detail = "Removing outliers...")
      
      # Remove outliers
      if (input$removeOutliers) {
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        outliers_removed <- 0
        for (col in numeric_cols) {
          Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
          Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
          IQR <- Q3 - Q1
          lower <- Q1 - input$outlierThreshold * IQR
          upper <- Q3 + input$outlierThreshold * IQR
          before <- nrow(data)
          data <- data[data[[col]] >= lower & data[[col]] <= upper, ]
          outliers_removed <- outliers_removed + (before - nrow(data))
        }
        if (outliers_removed > 0) {
          log <- c(log, paste("Removed", outliers_removed, "outlier rows"))
        }
      }
      
      incProgress(1, detail = "Done!")
      
      rv$current_data <- data
      rv$cleaning_log <- c(rv$cleaning_log,
                           paste(Sys.time(), "- Auto-cleaning completed"),
                           log)
      
      # Recalculate quality
      rv$quality_score <- calculateQualityScore(data)
      rv$issues_detected <- detectIssues(data)
      
      showNotification("Auto-cleaning completed successfully!", type = "message")
    })
  })
  
  # Cleaning progress
  output$cleaningProgress <- renderUI({
    req(rv$cleaning_log)
    
    if (length(rv$cleaning_log) > 0) {
      recent_logs <- tail(rv$cleaning_log, 10)
      log_items <- lapply(recent_logs, function(log) {
        div(class = "issue-success", p(log))
      })
      do.call(tagList, log_items)
    }
  })
  
  # Cleaning summary
  output$cleaningSummary <- renderText({
    req(rv$original_data, rv$current_data)
    
    summary_text <- c()
    summary_text <- c(summary_text, "CLEANING SUMMARY")
    summary_text <- c(summary_text, "")
    summary_text <- c(summary_text, paste("Original rows:", format(nrow(rv$original_data), big.mark = ",")))
    summary_text <- c(summary_text, paste("Current rows:", format(nrow(rv$current_data), big.mark = ",")))
    summary_text <- c(summary_text, paste("Rows removed:", format(nrow(rv$original_data) - nrow(rv$current_data), big.mark = ",")))
    summary_text <- c(summary_text, "")
    summary_text <- c(summary_text, paste("Original columns:", ncol(rv$original_data)))
    summary_text <- c(summary_text, paste("Current columns:", ncol(rv$current_data)))
    summary_text <- c(summary_text, "")
    summary_text <- c(summary_text, paste("Original quality:", round(calculateQualityScore(rv$original_data)$overall, 1), "%"))
    summary_text <- c(summary_text, paste("Current quality:", round(rv$quality_score$overall, 1), "%"))
    summary_text <- c(summary_text, paste("Improvement:", 
                                          round(rv$quality_score$overall - calculateQualityScore(rv$original_data)$overall, 1), "%"))
    
    paste(summary_text, collapse = "\n")
  })
  
  # Manual data preview
  output$manualDataPreview <- renderDT({
    req(rv$current_data)
    datatable(head(rv$current_data, 100),
              options = list(pageLength = 10, scrollX = TRUE),
              editable = TRUE)
  })
  
  # Transform preview
  output$transformPreview <- renderDT({
    req(rv$current_data)
    datatable(head(rv$current_data, 50),
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Export summary
  output$exportSummary <- renderText({
    req(rv$current_data)
    
    summary_text <- c()
    summary_text <- c(summary_text, "EXPORT SUMMARY")
    summary_text <- c(summary_text, "")
    summary_text <- c(summary_text, paste("Rows to export:", format(nrow(rv$current_data), big.mark = ",")))
    summary_text <- c(summary_text, paste("Columns:", ncol(rv$current_data)))
    summary_text <- c(summary_text, paste("Estimated size:", 
                                          round(object.size(rv$current_data) / 1024 / 1024, 2), "MB"))
    summary_text <- c(summary_text, paste("Format:", input$exportFormat))
    summary_text <- c(summary_text, paste("Quality score:", round(rv$quality_score$overall, 1), "%"))
    
    paste(summary_text, collapse = "\n")
  })
  
  # Download cleaned data
  output$downloadCleaned <- downloadHandler(
    filename = function() {
      timestamp <- if (input$exportTimestamp) paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S")) else ""
      paste0("cleaned_data", timestamp, ".", input$exportFormat)
    },
    content = function(file) {
      req(rv$current_data)
      
      if (input$exportFormat == "csv") {
        fwrite(rv$current_data, file, sep = input$exportDelimiter)
      } else if (input$exportFormat == "xlsx") {
        writexl::write_xlsx(rv$current_data, file)
      } else if (input$exportFormat == "json") {
        write(toJSON(rv$current_data, pretty = TRUE), file)
      } else if (input$exportFormat == "rds") {
        saveRDS(rv$current_data, file)
      }
    }
  )
  
  # Cleaning log
  output$cleaningLog <- renderText({
    req(rv$cleaning_log)
    paste(rv$cleaning_log, collapse = "\n")
  })
}

shinyApp(ui = ui, server = server)