source("helper.R", local = T)

# Define server logic required for calculation
server <- function(input, output, session) {
  # Input validation --------------------------------------------------------
  ## Create validator --------------------------------------------------------

  # Global validator
  iv <- InputValidator$new()

  # Conditional validator
  # Mean
  iv_m <- InputValidator$new()
  iv_m$condition(~ (input$outcome == "Mean"))
  iv$add_validator(iv_m)

  # Cluster design
  iv_cluster <- InputValidator$new()
  iv_cluster_hetero <- InputValidator$new()

  iv_cluster$condition(~ (input$cluster))
  iv_cluster_hetero$condition(~ (input$cv_type == "separate"))

  iv_cluster$add_validator(iv_cluster_hetero)
  # iv_p$condition(~ (input$outcome == "Proportion"))


  ## Validator Rules ---------------------------------------------------------

  # Expected Matrix
  iv$add_rule("Ei", sv_required())
  iv$add_rule("Ec", sv_required())

  # Power and alpha
  iv$add_rule("power", sv_required())
  iv$add_rule("alpha", sv_required())

  # Cluster blobs
  iv_cluster_hetero$add_rule("CVi", sv_gte(0))
  iv_cluster$add_rule("CVc", sv_gte(0))
  iv_cluster$add_rule("cluster_size", compose_rules(sv_integer(), sv_gte(0)))



  ## Enabling the validators ---------------------------------------------------

  iv$enable() # Global
  # iv_m$enable() # under 'mean'
  # iv_mt_rel$enable() # under 'Relative'
  iv_cluster$enable() # under 'Relative'
  # More to come

  # Data storage declaration ---------------------------------------------------

  ## Stationary Variables -----
  # Comparison method dictionary
  method_labels <- c(
    "direct" = "(Gr.2)",
    "absolute" = "Diff",
    "relative" = "Ratio"
  )

  ## Reactive Variables ------

  # Indicator for debugging mode
  .i <- reactiveVal(F)

  # Central data storage
  shared_val <- reactiveValues(
    # Choices for X and Color aesthetic selection
    labels = c(
      "Mean (Gr.1)" = "Ec",
      "Mean (Gr.2)" = "Ei",
      "SD (Gr.1)" = "SDc",
      "SD (Gr.2)" = "SDi",
      "CV (Gr.1)" = "CVc",
      "CV (Gr.2)" = "CVi",
      "Clusters' size" = "cluster_size",
      "Significance Level" = "alpha",
      "Power" = "power"
    ),
    # Baseline wording for sample size
    sample_label = c("Sample Size (n)" = "c"),
    # Value for focus val
    focus_val = 0
  )



  # Reactive Functions ---------------------------------------------------------

  ## Debugging mode activation ------
  # Activate/deactivate the debugging mode status based on the 'check' input
  observeEvent(input$check, .i(input$check))

  ## UI updating group ---------------------------------------------------------

  ### Update [method] choices -----
  # Define an observe function to update the 'method' radio button choices when the outcome changes
  observeEvent(input$outcome, {
    # Checkpoint before making changes
    check_point(.i(), "updateMethodChoice", 1, 2)

    # Update choices for the radio button based on selected outcome
    if (input$outcome == "Mean") {
      # Test
      updateRadioGroupButtons(session,
        inputId = "method",
        choices = c(
          "Group 2" = "direct",
          "Difference" = "absolute"
        ),
      )
    } else {
      # Test
      updateRadioGroupButtons(session,
        inputId = "method",
        choices = c(
          "Group 2" = "direct",
          "Difference" = "absolute",
          "Ratio" = "relative"
        ),
      )
    }

    # Checkpoint after making changes
    check_point(.i())
  })

  ### Cascading updates: Update expected value labels, matrix column names, and display choices ----
  #### Update expected values label [Ei, Ec] ----
  # Reactive function to update labels based on input$outcome and input$method
  updateExpectedValuesLabel <- reactive({
    # Checkpoint before making changes
    check_point(.i(), "Update Expected valueLabels", 1, 2)

    # Get input values
    outcome <- input$outcome
    method <- method_labels[input$method]
    labels <- shared_val$labels

    # Update the labels
    names(labels)[1:2] <- c(
      paste(outcome, "(Gr.1)"),
      paste(outcome, method)
    )

    # Save the updated labels
    shared_val$labels <- labels

    # Checkpoint after making changes
    check_point(.i())
  })
  
  #--- Render UI for Data imput  ----
  # Need to do this because the updateNumericInputIcon fails to update label
  # We will add the issue to thier page later

  updateUIdataInput <- reactive({
    output$input_container <- renderUI({
      outcome <- input$outcome
      method <- input$method

      Ei <- isolate(input$Ei) %||% 0
      Ec <- isolate(input$Ec) %||% 0
      # Get limits and icon
      labels <- names(shared_val$labels[1:2])
      min_max_Ec <- set_limit(outcome, "direct") |> unname()
      min_max_Ei <- set_limit(outcome, method) |> unname()
      icons <- set_icon(method)

      fluidRow(
        column(6,
          numericInputIcon(
            inputId = "Ec",
            label = labels[1],
            help_text = "Out of range!",
            value = Ec,
            min = min_max_Ec[1], max = min_max_Ec[2], step = 0.1,
            icon = icon("people-group")
          ),
          style = "padding:0.5%;"
        ),
        column(6,
          numericInputIcon(
            inputId = "Ei",
            label = labels[2],
            help_text = "Out of range!",
            value = Ei,
            min = min_max_Ei[1], max = min_max_Ei[2], step = 0.1,
            icon = icons
          ),
          style = "padding:0.5%;"
        ),
        style = "margin:0%; justify-content: space-between",
        class = "flex-row"
      )
    })
  })

  #### Update choice for aesthetic X and color ----
  # Reactive function to update display choices
  updateDisplayChoices <- reactive({
    # Check current status of the function
    check_point(.i(), "updateDisplayChoices", 2)

    # Get input values
    outcome <- input$outcome
    x_aes <- input$x_aes

    # Remove SD choices if user doesn't select mean
    if (outcome == "Mean") {
      choice_labels <- shared_val$labels
    } else {
      choice_labels <- shared_val$labels[-c(3, 4)]
    }

    # Remove cluster-related choice if not the cluster
    if (input$cluster == F) {
      choice_labels <- choice_labels[!choice_labels %in% c("CVc", "CVi", "cluster_size")]
    }

    # Update aes_x and aes_color inputs
    updateSelectInput(session, "x_aes",
      choices = (choice_labels),
      selected = x_aes
    )
    updateSelectInput(session, "color_aes",
      selected = NULL, # input$color_aes,
      choices = choice_labels[choice_labels != x_aes]
    )
    check_point(.i())
  })

  #### Observe changes for expected value, matrix, and display choice ----
  observeEvent(c(input$outcome, input$method), {
    updateExpectedValuesLabel()
    updateUIdataInput()
    # updateMatrixColNames()
    # updateDataLabel()
    updateDisplayChoices()
  })
  observeEvent(c(input$x_aes, input$cluster), updateDisplayChoices())



  ### Update Sample Size labels -------
  # updates the sample label based on the value of the "cluster" input.
  updateSampleLabel <- reactive({
    # Print a debugging message to the console
    check_point(.i(), "updateSampleLabel", 1, 2)

    # If the 'cluster' checkbox is checked, update the sample label to reflect that we're calculating the number of clusters per arm
    if (input$cluster == T) {
      shared_val$sample_label <- c("Number of Clusters (per arm)" = "c")
    } else {
      # Otherwise, update the sample label to reflect that we're calculating the sample size per arm
      shared_val$sample_label <- c("Sample Size (per arm)" = "c")
    }

    # Print a debugging message to the console indicating that the update is complete
    check_point(.i())
  })
  # Observe and implement
  observeEvent(input$cluster, updateSampleLabel())








  ### Update Step size ----
  observeEvent(c(input[[input$x_aes]], input$matrix_e, input$matrix_sd), {
    check_point(.i(), "ObserveEvent StepSize", 1, 2)

    x_aes <- input$x_aes
    # print(shared_val$focus_val)
    k <- as.numeric(input[[input$x_aes]]) 


    step_size <- step_fun(k)
    shared_val$focus_val <- k
    updateNumericInput(session, "x_step_size", value = step_size, step = step_size)

    check_point(.i())
  })

  ### update slider
  observeEvent(c(shared_val$focus_val, input$x_step_size), {
    # Debugging

    check_point(.i(), "SliderUpdating", 1, 2)

    x_aes <- input$x_aes
    # print(shared_val$focus_val)
    # cat(k)

    step_size <- input$x_step_size


    slider_range <- c(-5, 5, -2, 2) * step_size + shared_val$focus_val

    updateSliderInput(session, "x_step_range",
      # label = "Range",
      min = slider_range[1],
      max = slider_range[2],
      step = step_size,
      value = slider_range[3:4]
    )

    check_point(.i())
  })

  ### SampleSize Calculation Cascade ----
  # Define reactive function for calculating the sample size
  calculateSampleSize <- reactive({
    check_point(.i(), "Calculate Sample Size", 1, 2)

    # Extract input values
    check_point(.i(), "Data extraction", 2)
    # if(input$check == T) cat("\n\tData extract: Checking..")
    outcome <- input$outcome
    method <- input$method

    Ec <- as.numeric(input$Ec) # matrix_e[, 1])
    Ei <- as.numeric(input$Ei) # matrix_e[, 2])
    SDc <- as.numeric(input$SDc)#matrix_sd[, 1])
    SDi <- as.numeric(input$SDi)#matrix_sd[, 2])
    CVc <- as.numeric(input$CVc) # / 100
    CVi <- as.numeric(input$CVi) # / 100
    cluster <- input$cluster
    cluster_size <- ifelse(input$cluster, input$cluster_size, NA)
    alpha <- input$alpha / 100
    power <- input$power / 100
    check <- F # input$ckeckbox
    x_aes <- input$x_aes
    x_step_size <- input$x_step_size
    x_step_range <- input$x_step_range
    color_aes <- input$color_aes
    col_var <- as.numeric(input$col_var)
    labels <- shared_val$labels
    sample_label <- shared_val$sample_label

    # Check
    check_point(.i())

    # Data.frame result
    check_point(.i(), "Building DF", 2)
    # if(input$check == T) cat("\n\tDF computed: Checking..")

    df_output <- df_prepare(
      outcome = outcome,
      method = method,
      Ec = Ec,
      Ei = Ei,
      SDc = SDc,
      SDi = SDi,
      CVc = CVc,
      CVi = CVi,
      cluster_size = cluster_size,
      alpha = alpha,
      power = power,
      cluster = cluster,
      ceiling_num = TRUE,
      check = F,
      aes_x_col = x_aes,
      x_step_size = x_step_size, x_step_range = x_step_range,
      aes_color_col = color_aes, col_var = col_var
    ) %>%
      # expand_grid() %>% # print() %>%# str()
      Add_samplesize() %>%
      arrange(!!sym(x_aes), !!sym(color_aes)) %>%
      group_by(!!sym(color_aes))

    # Check
    check_point(.i())

    # Check
    check_point(.i(), "Generating Plot", 2)

    plot_output <-
      df_output %>%
      mutate(text_hover = paste0(
        names(labels[labels == x_aes]), ": ",
        .data[[x_aes]], "<br>",
        names(labels[labels == color_aes]), ": ",
        .data[[color_aes]], "<br><b>",
        names(sample_label), ": ", c, "</b>"
      )) %>%
      plot_ly(
        x = ~ get(x_aes),
        y = ~c,
        # group = ~factor(df_output[[color_aes]]),
        color = ~ factor(get(color_aes)),
        colors = "Dark2"
      ) %>%
      add_trace(
        mode = "lines+markers", type = "scatter",
        marker = list(opacity = 0.8, size = 6),
        hoverinfo = "text",
        text = ~text_hover
      ) %>%
      layout(
        xaxis = list(title = names(labels[labels == x_aes])),
        yaxis = list(title = names(sample_label)),
        legend = list(title = list(text = names(labels[labels == color_aes])))
      )

    # Check
    check_point(.i())
    # if(input$check == T) cat("okay!")

    # Estimate sample for cluster design
    if (cluster) {
      df_output <- df_output %>%
        mutate(N = cluster_size * c)
    }

    # Check
    check_point(.i(), "Remove NA columns", 2)

    df_output <- df_output %>%
      select(any_of(c(labels, sample_label, "Total Pop (per arm)" = "N")))

    check_point(.i())


    # Check
    check_point(.i(), "Producing a reactable", 2)

    # Change it to reactable
    df_output_rt <- reactable(
      df_output,
      defaultPageSize = 5,
      defaultColDef = colDef(
        align = "center",
        minWidth = 70,
        headerStyle = list(background = "#f7f7f8")
      ),
      bordered = TRUE,
      highlight = TRUE
    )
    # Check
    check_point(.i())

    # output
    lst( # text_output,
      df_output,
      df_output_rt,
      plot_output
    )
  })

  #### Call the calculateSampleSize function when the Calculate button is clicked -----
  observeEvent(input$calculate, {
    # check_point(shared_val$check, "\nBefore validate.... \n")
    req(iv$is_valid())
    req(iv_cluster$is_valid())
    out <- calculateSampleSize()

    # Render the data frame
    output$data_frame <- renderReactable(out$df_output_rt)

    # Render the plot
    output$plot <- renderPlotly(out$plot_output)
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(out$df_output, file)
      }
    )
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(NA, file)
    }
  )
}
