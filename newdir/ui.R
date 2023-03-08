# Define UI for the application
# cat("ui_1:", ls(), "\n")
source("helper.R", local = T)
# cat("ui_2:", ls(), "\n")

#print("UI.R" %++% " Start")
ui <- fluidPage(
  # Title panel
  titlePanel("Sample Size Calculator"),

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
      uiOutput("method_input"),
      hr(),
      # Matrix inputs
      matrixInput("matrix_e",
        rows = list(names = FALSE),
        cols = list(names = TRUE),
        class = "numeric",
        value = matrix(NA, 1, 2)
      ),
      conditionalPanel(
        condition = "input.outcome === 'Mean'",
        matrixInput("matrix_sd",
          rows = list(names = FALSE),
          cols = list(names = TRUE),
          class = "numeric",
          value = {
            k <- matrix(NA, 1, 2)
            colnames(k) <- c("SD (Gr.1)", "SD (Gr.2)")
            k
          }
        )
      ),
      hr(),
      # Power and significance level inputs
      fluidRow(
        column(2, h5("Power")),
        column(
          4,
          # Numeric input for power
          numericInput("power", label = NULL, value = 0.8, step = 0.01, min = 0, max = 1)
        ),
        column(2, h5("Sig.lvl")),
        column(
          4,
          # Numeric input for significance level
          numericInput("alpha", label = NULL, value = 0.05, step = 0.01, min = 0, max = 1)
        )
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
    # ),

    # Main panel for displaying result
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
      "SampleSizer: because size matters (lmao) v0.0.04 (230220) - learning how to code shiny edition (very buggy!) | Created by ",
      a("Songyos Rajborirug (Tony)", href = "srajbor1@jh.edu")
    ))
  )
)

#print("UI.R" %++% " Stop")
