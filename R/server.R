# ---------

source("helper.R", local = T)

# Define server logic required for calculation
server <- function(input, output, session) {
  # Input validation --------------------------------------------------------
  ## Create validator --------------------------------------------------------

  # Global validator
  iv <- InputValidator$new()
  # Conditional validator
  iv_m <- InputValidator$new()
  iv_m$condition(~ (input$outcome == "Mean"))
  
  iv_mt_rel <- InputValidator$new()
  iv_mt_rel$condition(~ (input$method == "relative"))
  # iv_p$condition(~ (input$outcome == "Proportion"))
  # iv_in$condition(~ (input$outcome == "Incidence Rate"))

  ## Validator Rules ---------------------------------------------------------

  # Expected Matrix
  iv$add_rule("matrix_e", sv_required())
  iv$add_rule("matrix_e", sv_numeric(allow_multiple = TRUE))
  # iv_mt_rel$add_rule("matrix_e", function(value) {
  #   Ei <- value[1,2]
  #   if (Ei < 0 || is.na(Ei)) {
  #     "Ratio should be a non-negative number"
  #   }
  # })

  # SD matrix
  iv_m$add_rule("matrix_sd", sv_required())
  iv_m$add_rule("matrix_sd", sv_numeric(allow_multiple = TRUE))
  iv_m$add_rule("matrix_sd", sv_between(0, Inf))

  ## Enabling the validators ---------------------------------------------------

  iv$enable() # Global
  iv_m$enable() # under 'mean'
  iv_mt_rel$enable() # under 'mean'

  # Static obj. declaration ----------------------------------------------------

  # Comparison method dictionary
  method_labels <- c(
    "direct" = "(Gr.2)",
    "absolute" = "Difference",
    "relative" = "Ratio"
  )
  # Reactive Part declaration ------------------------------------------------------

  ## Variables ---------------------------------------------------------------
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
    #check = F,
    # Value for focus val
    focus_val = 0 
  )

  # Indicator for debugging mode
  .i <- reactiveVal(F)
  
  

  ## Functions ----------------------------------------------------------------

  ### Update Sample Size labels -------
  # updates the sample label based on the value of the "cluster" input.
  updateSampleLabel <- reactive({
    # Print a debugging message to the console
    check_point(.i(), "\nupdateSampleLabel: Checking..")

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
  
  
  ### Update Expected value Labels -----
  # Update labels based on input$outcome and input$method
  updateExpectedValuesLabel <- reactive({
    #.i <- shared_val$check
    check_point(.i(), "\nUpdate Expected value Labels Reactive function: ..")
    # print(shared_val$labels)
    outcome <- input$outcome
    method <- method_labels[input$method]
    labels <- shared_val$labels

    names(labels)[1:2] <- c(
      paste(outcome, "(Gr.1)"),
      paste(outcome, method)
    )

    shared_val$labels <- labels

    check_point(.i())
  })

  ### Update Matrix column name -----
  # Reactive function to update matrix column names
  updateMatrixColNames <- reactive({
    # Get current check point index
    # Get input values
    matrix_e <- input$matrix_e
    #.i <- shared_val$check

    # Check current status of the function
    check_point(.i(), "\nupdateMatrixColNames: Checking..")

    # Update column names
    # print("internal: Matrix")
    # print(coln)
    colnames(matrix_e) <- names(shared_val$labels[1:2])

    updateMatrixInput(session, "matrix_e",
      value = matrix_e
    )
    check_point(.i())
  })

  ### Update Choice for aesthetic X and color ----
  # Reactive function to update the display choices
  updateDisplayChoices <- reactive({
    # Get current check point index


    # Check current status of the function
    check_point(.i(), "\nupdateDisplayChoices reactive function: Checking..")

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

  # Observe and implement for expected value/Matrix/Displaychoice
 
  observeEvent(c(input$outcome, input$method), {
    updateExpectedValuesLabel()
    updateMatrixColNames()
    updateDisplayChoices()
  })
  
  observeEvent(c(input$x_aes, input$cluster), updateDisplayChoices())



  # updates the debugging mode status based on the 'check' input
  # observeEvent(input$check, {
  #   # shared_val$check <- input$check
  #   print(input$check)
  #   k <- input$check
  # 
  #   .i(k)
  # })
  # observe(cat("Hello: ",.i()," \n"))
  
  ### Update Step size ----
  observeEvent(c(input[[input$x_aes]], input$matrix_e, input$matrix_sd), {
    check_point(.i(), "\nObserveEvent StepSize : Checking..")

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

    check_point(.i(), "\nCalculate Sample Size Reactive function: ..")

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


  # Define reactive function for rendering method input options based on selected outcome
  output$method_input <- renderUI({
    outcome <- input$outcome
    method_choices <- list("Group 2" = "direct", "Absolute Difference" = "absolute")
    if (outcome != "Mean") {
      method_choices[["Relative Difference"]] <- "relative"
    }
    radioButtons("method",
      label = "Select Comparison Input:",
      choices = method_choices, selected = "direct"
    )
  })



  # Define reactive function for calculating the sample size
  calculateSampleSize <- reactive({
    # check <- input$check


    check_point(.i(), "\nCalculate Sample Size Reactive function: ..")


    # validate(
    #   # Check blank input
    #   need(anyNA(input$matrix_e), "Group 1 and Group 2 data")#,
    #   #need(anyNA(input$matrix_SD) & input$outcome == "Mean", "Both SD")
    #
    #   # if cluster
    #
    # )

    # Extract input values
    check_point(.i(), "\n\tData extract: Checking..")
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
    # --------------------------------
    # cat(sample_label)

    # Check
    check_point(.i())
    # if(input$check == T) cat("okay!")

    # -----
    # Data.frame result
    check_point(.i(), "\n\tDF computed: Checking..")
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

    # cat("Calculate_afterDf")
    # Check
    check_point(.i())
    # if(input$check == T) cat("okay!")

    # Check
    check_point(.i(), "\n\tPlot generate: Checking..")
    # if(input$check == T) cat("\n\tPlot generate: Checking..")
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
        #   paste0(
        #   names(labels[labels == x_aes]), ": ", !!x_aes,
        #   "<br>", names(labels[labels == color_aes]), ": ", df_output[[color_aes]],
        #   "<br><b>", names(sample_label), ": ", c, "</b>"
        # )
      ) %>%
      # add_lines() %>%
      layout(
        xaxis = list(title = names(labels[labels == x_aes])),
        yaxis = list(title = names(sample_label)),
        legend = list(title = list(text = names(labels[labels == color_aes])))
      )

    # Check
    check_point(.i())
    # if(input$check == T) cat("okay!")

    # Check
    check_point(.i(), "\n\tSelect relevant column: Checking..")
    # if(input$check == T) cat("\n\tSelect relevant column: Checking..")

    if (cluster) {
      df_output <- df_output %>%
        mutate(N = cluster_size * c)
    }
    df_output <- df_output %>%
      select(any_of(c(labels, sample_label, "Total Pop (per arm)" = "N")))

    # cat("Calculate_afterDf2")
    # print(color_aes))
    # print(!!sym(color_aes))
    # Check
    check_point(.i())
    # if(input$check == T) cat("okay!")

    # Check
    check_point(.i(), "\n\tReactable computed: Checking..")
    # if(input$check == T) cat("\n\tReactable computed: Checking..")
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

    # Identify the column labels
    # Check
    check_point(.i())
    # if(input$check == T) cat("okay!")

    # Check
    # check_point(.i(), "\n\tParseing output: Checking..")
    # if(input$check == T) cat("\n\tParseing output: Checking..")
    lst( # text_output,
      df_output, # %>% select(any_of(c(labels, sample_label = "c"))),
      df_output_rt,
      plot_output
    )
  })

  # Call the calculateSampleSize function when the Calculate button is clicked
  observeEvent(input$calculate, {
    # check_point(shared_val$check, "\nBefore validate.... \n")
    req(iv$is_valid())
    req(iv_m$is_valid())
    req(iv_mt_rel$is_valid())
    # req(iv_in$is_valid())
    # Validate data
    # validate(
    #   # Check blank input
    #   need(anyNA(input$matrix_e), "Group 1 and Group 2 data")#,
    #   #need(anyNA(input$matrix_SD) & input$outcome == "Mean", "Both SD")
    #
    #   # if cluster
    #
    # )

    out <- calculateSampleSize()
    # check_point(shared_val$check, "\nAfter validate \n")
    # check_point(shared_val$check, "Pass!\n\n") #cat("okay!\n\n")
    # Render the calculated result
    # output$text <- renderPrint(out$text_output)

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
# print("server.R" %++% " End")