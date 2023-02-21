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
  
  # # Proportion  
  # iv_p <- InputValidator$new()
  # iv_p$condition(~ (input$outcome == "Proportion"))
  # iv$add_validator(iv_p)
  # 
  # # Proportion  
  # iv_i <- InputValidator$new()
  # iv_i$condition(~ (input$outcome == "Incidence Rate"))
  # iv$add_validator(iv_i)
  

  # Direct comparison method
  iv_mt_direct <- InputValidator$new()
  iv_mt_direct$condition(~ (input$method == "direct"))
  iv$add_validator(iv_mt_direct)
  
    # proportion
    iv_p_dr <- InputValidator$new()
    iv_p_dr$condition(~ (input$outcome == "Proportion"))
    iv_mt_direct$add_validator(iv_p_dr) # Nested
    
    # Incidence
    iv_i_dr <- InputValidator$new()
    iv_i_dr$condition(~ (input$outcome == "Incidence Rate"))
    iv_mt_direct$add_validator(iv_i_dr) # Nested
    
  # Relative method
  iv_mt_rel <- InputValidator$new()
  iv_mt_rel$condition(~ (input$method == "relative"))
  iv$add_validator(iv_mt_rel)


  # Cluster design
  iv_cluster <- InputValidator$new()
  iv_cluster_hetero <- InputValidator$new()

  iv_cluster$condition(~ (input$cluster))
  iv_cluster_hetero$condition(~ (input$cv_type == "separate"))

  iv_cluster$add_validator(iv_cluster_hetero)
  # iv_p$condition(~ (input$outcome == "Proportion"))


  ## Validator Rules ---------------------------------------------------------

  # Expected Matrix
  iv$add_rule("matrix_e", compose_rules(
    sv_required(),
    sv_numeric(allow_multiple = TRUE)
  ))
  
  # Under all
  # iv_p$add_rule("matrix_e", compose_rules(
  #   sv_required(),
  #   function(x) if(any(x==5)) "Must be between 0 and 1111")
  # )
  #               
  # iv_i$add_rule("matrix_e", compose_rules(
  #   sv_required(),
  #   ~if(!(.[,1] < 0)) "Must be between 0 and Inf")
  # )
  
  # Under direct
  iv_p_dr$add_rule("matrix_e", sv_between(0,1))
  iv_i_dr$add_rule("matrix_e", sv_gte(0, allow_multiple = TRUE))
  
  # Under Relative
  iv_mt_rel$add_rule("matrix_e", function(value) {
    Ei <- value[1,2]
    if (Ei < 0 || is.na(Ei)) {
      "Must not contain `NA` values / Only a non-negative ratio is allowed"
    }
  })

  # SD matrix
  iv_m$add_rule("matrix_sd", compose_rules(
    sv_required(),
    sv_numeric(allow_multiple = TRUE),
    sv_gte(0, allow_multiple = TRUE)
  ))
  # iv_m$add_rule("matrix_sd", sv_numeric(allow_multiple = TRUE))
  # iv_m$add_rule("matrix_sd", sv_between(0, Inf))

  # Power and alpha
  iv$add_rule("power", sv_between(0,1))
  iv$add_rule("alpha", sv_between(0,1))
  
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
    "absolute" = "Difference",
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
      updateRadioButtons(session, "method",
        choices = c(
          "Group 2 info" = "direct",
          "Differences (Gr.1 - Gr.2)" = "absolute"
        ),
        selected = "direct"
      )
    } else {
      updateRadioButtons(session, "method",
        choices = c(
          "Group 2 info" = "direct",
          "Differences (Gr.1 - Gr.2)" = "absolute",
          "Ratio (Gr.1 / Gr.2)" = "relative"
        ),
        selected = "direct"
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

  #### Update matrix column name ----
  # Reactive function to update matrix column names
  updateMatrixColNames <- reactive({
    # Get input values
    matrix_e <- input$matrix_e

    # Check current status of the function
    check_point(.i(), "updateMatrixColNames", 2)

    # Update column names
    colnames(matrix_e) <- names(shared_val$labels[1:2])

    # Update the matrix
    updateMatrixInput(session, "matrix_e",
      value = matrix_e
    )
    check_point(.i())
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
    updateMatrixColNames()
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
    k <- ifelse(x_aes == "Ec", input$matrix_e[, 1],
      ifelse(x_aes == "Ei", input$matrix_e[, 2],
        ifelse(x_aes == "SDc", input$matrix_sd[, 1],
          ifelse(x_aes == "SDi", input$matrix_sd[, 2],
            as.numeric(input[[input$x_aes]])
          )
        )
      )
    )


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

    Ec <- as.numeric(input$matrix_e[, 1])
    Ei <- as.numeric(input$matrix_e[, 2])
    SDc <- as.numeric(input$matrix_sd[, 1])
    SDi <- as.numeric(input$matrix_sd[, 2])
    CVc <- as.numeric(input$CVc) # / 100
    CVi <- as.numeric(input$CVi) # / 100
    cluster <- input$cluster
    cluster_size <- ifelse(input$cluster, input$cluster_size, NA)
    alpha <- input$alpha
    power <- input$power
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
        # header = function(value) gsub(".", " ", value, fixed = TRUE),
        # cell = function(value) format(value, nsmall = 1),
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
    # req(iv_m$is_valid())
    # req(iv_mt_rel$is_valid())
    req(iv_cluster$is_valid())
    # req(iv_in$is_valid())
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
