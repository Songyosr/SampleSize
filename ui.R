# Define UI for the application
source("helper.R", local = T)

ui <- fluidPage(
  # CSS
  tags$head(
    tags$style(
      #HTML(".my-numeric-input label {display: none;}"),
      HTML(".flex-row { display: flex; flex-direction: row !important;}")
    )
  ),
  
  # Title panel
  titlePanel("Sample Sizer: because (Sample) size matters"),
  

  # Sidebar layout with input options
  sidebarLayout(

    # Sidebar panel for input options
    sidebarPanel(

      # Dropdown for outcome type
      selectInput("outcome",
        label = "Select Outcome Type:",
        choices = list("Mean", "Proportion", "Incidence Rate"),
        selected = "Proportion"
      ),
      
      # Radio buttons for intervention input method
      radioGroupButtons(
        inputId = "method",
        label = NULL,#"Select Comparison method (1 vs. 2):", 
        justified = T,
        choices = c("Group 2" = "direct", 
                    "Difference" = "absolute", 
                    "Ratio" = "relative"),
        selected = "direct"
      ),
      
      # UI for data entry
      uiOutput("input_container"),
    
      
      # For SD
      conditionalPanel(
        condition = "input.outcome === 'Mean'",
        fluidRow(
          column(6,
                 numericInputIcon(
                   inputId = "SDc",
                   label = "SD (Gr.1)",
                   help_text = "Out of range!",
                   value = NULL,
                   min = 0, max = Inf, step = 0.01,
                   icon = icon("chart-simple")
                 ),
                 style = "padding:0.5%;"
          ),
          column(6,
                 numericInputIcon(
                   inputId = "SDi",
                   label = "SD (Gr.2)",
                   help_text = "Out of range!",
                   value = NULL,
                   min = 0, max = Inf, step = 0.01,
                   icon = icon("chart-simple")
                 ),
                 style = "padding:0.5%;"
          ),
          style = "margin:0%; justify-content: space-between",
          class = "flex-row"
        )
      ),
      
      
      
      # Matrix for SD 
      # conditionalPanel(
      #   condition = "input.outcome === 'Mean'",
      #   matrixInput("matrix_sd",
      #     rows = list(names = FALSE),
      #     cols = list(names = TRUE),
      #     class = "numeric",
      #     value = {
      #       k <- matrix(NA, 1, 2)
      #       colnames(k) <- c("SD (Gr.1)", "SD (Gr.2)")
      #       k
      #     }
      #   )
      # ),
      hr(),
      
      # Alternative
      fluidRow(
        column(12, tags$h5("Power & Significance level (%)"), 
               style = "padding-left: 0%;"),
        column(
          6, 
          numericInputIcon(
            inputId = "power",
            label = NULL,
            help_text = "Must be between 0 and 100 (%)",
            min = 0, max = 100, step = 5, value = 80,
            icon = list("Power")
          ),
          style = "padding: 0.5%;"
        ),
        column(
          6, 
          numericInputIcon(
            inputId = "alpha",
            label = NULL,
            help_text = "Must be between 0 and 100 (%)",
            min = 0, max = 100, step = 1, value = 5,
            icon = list("Sig")
          ),
          style = "padding: 0.5%;"
        ),
        style = "margin: 0%;"
      ),
      
      
      

      # Checkbox and input options for cluster design
      wellPanel(
        checkboxInput("cluster", label = "Cluster Design", value = FALSE),
        conditionalPanel(
          condition = "input.cluster == true",
          numericInput("cluster_size", label = "Average Cluster Size", value = NULL, step = 1, min = 1),
          radioButtons("cv_type",
            label = "Coefficient of Variation:",
            choices = list(
              "Homogeneous" = "homogeneous",
              "Heterogenous" = "separate"
            ),
            selected = "homogeneous"
          )
        ),
        conditionalPanel(
          condition = "input.cluster == true && input.cv_type === 'homogeneous'",
          numericInput("CVc", label = "Coefficient of Variation", value = NULL, min = 0)
        ),
        conditionalPanel(
          condition = "input.cluster == true && input.cv_type === 'separate'",
          fluidRow(
            column(6, numericInput("CVc", label = "CV (Gr.1)", value = NULL, min = 0)),
            column(6, numericInput("CVi", label = "CV (Gr.2)", value = NULL, min = 0))
          )
        )
      )
    ),


    # Main panel for displaying result and additional inputs
    mainPanel(
      # Output for calculated sample size
      wellPanel(
        # Additional inputs
        fluidRow(
          column(
            4,
            selectInput("x_aes",
              label = "X-axis",
              choices = c("..." = "Ec"),
              selected = "Ec"
            )
          ),
          column(
            2,
            numericInput("x_step_size", label = "Step Size", value = .1)
          ),
          column(
            6,
            sliderInput("x_step_range",
              label = "Range",
              min = -5, max = 5, step = 1, value = c(-2, 2)
            )
          )
        ),
        fluidRow(
          column(
            4,
            selectInput("color_aes",
              label = "Color",
              choices = c("..." = "Ei"),
              selected = "Ei"
            )
          ),
          column(
            8,
            selectizeInput(
              "col_var",
              "Variation (max 5)",
              choices = NULL,
              multiple = TRUE,
              options = list(create = TRUE, maxItems = 3, placeholder = "Type & Enter")
            )
          )
        ),
        # Calculate button
        fluidRow(
          column(5, actionButton("calculate", label = "Calculate")),
          column(5, downloadButton("downloadData", "Download")),
          column(2, checkboxInput("check", label = "Debugging Mode", value = F))
        )
      ),

      # Data frame table output
      reactableOutput("data_frame"),

      # Plotly chart output
      plotlyOutput("plot")
    )
  ),
  fluidRow(
    style = "text-align: center; color: grey;",
    HTML(paste0(
      "SampleSizer: because (Sample) size matters v0.2.0-alpha - learning how to code edition (less buggy!) <br> Update: ",Sys.Date()," | Created by ",
      a("Songyos Rajborirug (Tony)", href = "https://github.com/Songyosr")
    ))
  )
)
