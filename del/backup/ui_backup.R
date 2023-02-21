# Define UI for the application
fluidPage(
  
  # Row display
  # tags$head(
  #   tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; } 
  #               #inline .form-group { display: table-row;}")
  # ),
   
  titlePanel("Central limit theorem"),
  # Sidebar layout with input options
  sidebarLayout(
    
    # Sidebar panel for input options
    sidebarPanel(
      # Dropdown for outcome type
      selectInput("outcome",
                  label = "Select Outcome Type:",
                  choices = list("Mean", "Proportion", "Incidence Rate"),
                  selected = "Mean"
      ),
      
      # Radio buttons for intervention input method
      uiOutput("comparator_input"),
      hr(),
      wellPanel(
        matrixInput("matrix_e",
                    rows = list(names = F),
                    cols = list(names = T),
                    value = {k = matrix(1:2, 1, 2); 
                             colnames(k) <- c("Mean ", "Mean Pop2"); k}
        ),
        conditionalPanel(condition = "input.outcome === 'Mean'",
        matrixInput("matrix_sd",
                    rows = list(names = F),
                    cols = list(names = T),
                    value = {k = matrix(c(0,0), 1, 2); 
                    colnames(k) <- c("SD Pop1", "SD Pop2"); k}
        ))
      ),
      wellPanel(
        fluidRow(
          column(
            6,
            # Numeric input for baseline mean/proportion/incidence rate in control group
            numericInput("baseline", label = "Baseline Mean", value = NULL)
          ),
          column(
            6,
            # Numeric input for intervention mean/proportion/incidence rate
            numericInput("intervention", label = "Intervention", value = NULL)
          )),
        # If outcome type is "mean", show additional input for baseline standard deviation
        conditionalPanel(
          condition = "input.outcome === 'Mean'",
          numericInput("sd_baseline", label = "Baseline Standard Deviation", value = NULL),
          numericInput("sd_intervention", label = "Intervention Standard Deviation", value = NULL)
        )
        ,
        # column(
        #   6,
        #   
        #   # If outcome type is "mean", show additional input for intervention standard deviation
        #   conditionalPanel(
        #     condition = "input.outcome === 'Mean'",
        #     numericInput("intervention_sd", label = "Intervention Standard Deviation", value = NULL)
        #   )
        # )
      ),
      hr(),
      fluidRow(
        column(2, h5("Power")),
        column(
          4,
          # Numeric input for power
          numericInput("power", label = NULL, value = 0.8)
        ),
        column(2, h5("Sig.lvl")),
        column(
          4,
          # Numeric input for significance level
          numericInput("significance", label = NULL, value = 0.05)
        )
      ),
      
      # Box for "Cluster Design" options
      wellPanel(
        checkboxInput("cluster", label = "Cluster Design", value = FALSE),
        conditionalPanel(
          condition = "input.cluster == true",
          numericInput("avg_cluster_size", label = "Average Cluster Size", value = NULL),
          radioButtons("cv_type",
                       label = "Coefficient of Variation:",
                       choices = list(
                         "Homogeneous" = "homogeneous",
                         "Control and Intervention" = "separate"
                       ),
                       selected = "homogeneous"
          )
        ),
        conditionalPanel(
          condition = "input.cluster == true && input.cv_type === 'homogeneous'",
          numericInput("cv_baseline", label = "Coefficient of Variation", value = NULL)
        ),
        conditionalPanel(
          condition = "input.cluster == true && input.cv_type === 'separate'",
          fluidRow(
            column(6, numericInput("cv_baseline", label = "Control Group CV", value = NULL)),
            column(6, numericInput("cv_intervention", label = "Intervention Group CV", value = NULL))
          )
        )
      ),
      hr(),
      
      # Calculate button
      actionButton("calculate", label = "Calculate"),
      checkboxInput("ckeckbox", label = "check", value = FALSE)
    ),
    
    # Main panel for displaying result
    # Main panel for displaying result and additional inputs
    mainPanel(
      # Output for calculated sample size
      verbatimTextOutput("result"),
      
      # Additional inputs
      fluidRow(
        column(4,
               selectInput("x_axis",
                           label = "X-axis",
                           choices = widget_labels,
                           selected = NULL)),
        column(4,
               numericInput("x_min", label = "Minimum Value", value = NULL)),
        column(4,
               numericInput("x_max", label = "Maximum Value", value = NULL))
      ),
      fluidRow(
        column(4,
               selectInput("color",
                           label = "Color",
                           choices = widget_labels,
                           selected = NULL)),
        column(4,
               numericInput("color_min", label = "Minimum Value", value = NULL)),
        column(4,
               numericInput("color_max", label = "Maximum Value", value = NULL))
      )
    )
    
  )
)
