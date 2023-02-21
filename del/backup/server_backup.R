#cat("server_outside:", ls(), "\n")
# ---------
source('helper.R', local = T)
# Define server logic required for calculation
function(input, output, session) {
  
  comparator_translate_labels <- c('direct' = "(Gr.2)",
                                   'absolute' = "Difference",  
                                   'relative' = "Ratio")
  
  shared_val <- reactiveValues(
    display_choices = c("Mean (Gr.1)" = "Ec",
                         "Mean (Gr.2)" = "Ei",
                         "SD (Gr.1)"  = "SDc",
                         "SD (Gr.2)" = "SDi",
                         "Control CV" = "CVc",
                         "Intervention CV" = "CVi",
                         "Clusters' size" = "m",
                         "Significance Level" = "alpha",
                         "Power" = "power")
  )
  #cat("server_inside:", ls(), "\n")
  # display_choices <- c("Mean (Gr.1)" = "Ec",
  #                      "Mean (Gr.2)" = "Ei",
  #                      "SD (Gr.1)"  = "SDc",
  #                      "SD (Gr.2)" = "SDi",
  #                      "Control CV" = "CVc",
  #                      "Intervention CV" = "CVi",
  #                      "Clusters' size" = "m",
  #                      "Significance Level" = "alpha",
  #                      "Power" = "power"
  # )
  
  updateMatrixColNames <- reactive({
    comparator <- comparator_translate_labels[input$comparator]
    outcome <- input$outcome
    matrix_e <- input$matrix_e
    x_aes <- input$x_aes
    display_choices <- shared_val$display_choices 
    #display_choices_update <- display_choices
    
    names(display_choices)[c(1,2)] <- colnames(matrix_e) <- c(
      paste(outcome, "(Gr.1)"), # First column
      paste(outcome, comparator)
    )
    
    shared_val$display_choices <- display_choices 
    
    # Remove SD choices if user dont select mean
    if(outcome != "Mean") display_choices_update <- display_choices[-c(3,4)]
    
    # updarte Matrix
    updateMatrixInput(session, "matrix_e", 
                      value = matrix_e
    )
    
    # Update input
    updateSelectInput(session, "x_aes", 
                      selected = x_aes,
                      #label = "Color",
                      choices = display_choices_update
    )
    
    #print(x_aes)
    #print(display_choices_update[display_choices_update != x_aes])
    
    # Remove choice selected in X-aes
    #display_choices_update <- setdiff(display_choices_update, aes_x)
    updateSelectInput(session, "color_aes", 
                      selected = input$color_aes,
                      choices = display_choices_update[display_choices_update != x_aes]
    )
    
    
    
  })
  # Call updateMatrixColNames function when the comparator option is changed
  observeEvent(c(input$comparator, input$outcome, input$x_aes) , {
    updateMatrixColNames()
  })
  
  # Define reactive function for rendering comparator input options based on selected outcome
  output$comparator_input <- renderUI({
    outcome <- input$outcome
    comparator_choices <- list("Group 2" = "direct", "Absolute Difference" = "absolute")
    if (outcome != "Mean") {
      comparator_choices[["Relative Difference"]] <- "relative"
    }
    radioButtons("comparator",
                 label = "Select Comparison Input:",
                 choices = comparator_choices, selected = "direct"
    )
  })
  
  
  
  # Define reactive function for calculating the sample size
  calculateSampleSize <- reactive({
    # Extract input values
    outcome <- input$outcome
    comparator <- input$comparator
    
    Ec <- as.numeric(input$matrix_e[,1])
    Ei <- as.numeric(input$matrix_e[,2])
    SDc <- as.numeric(input$matrix_sd[,1])
    SDi <- as.numeric(input$matrix_sd[,2])
    CVc <- as.numeric(input$cv_baseline) #/ 100
    CVi <- as.numeric(input$cv_intervention)# / 100
    cluster <- input$cluster
    num_clusters <- ifelse(input$cluster, input$avg_cluster_size, NA)
    significance <- input$significance
    power <- input$power
    check <- input$ckeckbox
    x_aes <- input$x_aes
    x_step_size <- input$x_step_size
    x_step_range <- input$x_step_range
    color_aes <- input$color_aes
    col_var <- as.numeric(input$col_var)
    display_choices <- shared_val$display_choices
    
    # Call the SS_calculation2 function
    result <- SS_calculation2(
      outcome = outcome,
      comparator = comparator,
      Ec = Ec,
      Ei = Ei,
      SDc = SDc,
      SDi = SDi,
      CVc = CVc,
      CVi = CVi,
      m = num_clusters,
      alpha = significance,
      power = power,
      cluster = cluster,
      ceiling_num = TRUE,
      check = check
    )
    
    # Return the calculated result
    text_output <- paste("The calculated result is: ", result)
    
    
    # -----
    # Extract more information
    # Ec <- parse_x(Ec, input$x_step_size, 
    #               from = input$x_step_range[1],
    #               to = input$x_step_range[2])
    # Ei <- unique(c(Ei, input$col_var))
    
    # -----
    # Data.frame result
    df_output <- df_prepare(
      outcome = outcome,
      comparator = comparator,
      Ec = Ec,
      Ei = Ei,
      SDc = SDc,
      SDi = SDi,
      CVc = CVc,
      CVi = CVi,
      m = num_clusters,
      alpha = significance,
      power = power,
      cluster = cluster,
      ceiling_num = TRUE,
      check = check,
      aes_x_col = x_aes, 
      x_step_size = x_step_size, x_step_range = x_step_range,
      aes_color_col = color_aes, col_var = col_var
    ) %>%# str()
      Add_samplesize()
    
    # Identify the column labels
    #x_pos <- names(df_output)[which(names(df_output) == x_aes)]
    #color_pos <- names(df_output)[which(names(df_output) == color_aes)]
    
    # Create Plot
    # plot_output <- df_output %>% 
    #   ggplot(aes(x = .data[[x_aes]], y = c, 
    #              col = factor(.data[[color_aes]]),
    #              group = factor(.data[[color_aes]])
    #   ))+
    #   geom_point(size = 3,
    #              aes(label = paste0(..x.., ..y..)))+
    #   geom_line(lwd = 2, alpha = 0.8) +
    #   #facet_grid(.~alpha)+
    #   theme_bw() +
    #   labs(x = names(display_choices[display_choices == x_aes]),
    #        y = "Sample Size" ,
    #        color = names(display_choices[display_choices == color_aes])
    #        )
    # 
    # 
    # plot_output <-ggplotly(plot_output)
    plot_output <- df_output %>%
      arrange(!!!x_aes, !!!color_aes) %>%
      group_by(!!!color_aes) %>%
      plot_ly(x = ~df_output[[x_aes]], y = ~c, 
              #group = ~factor(df_output[[color_aes]]),
              color = ~factor(df_output[[color_aes]]),
              colors = 'Dark2') %>%
      add_trace(mode = 'lines+markers',
                marker = list(opacity = 0.8),
                  hoverinfo = 'text',
                  text = ~paste0(names(display_choices[display_choices == x_aes]), ': ', df_output[[x_aes]],
                                '<br>', names(display_choices[display_choices == color_aes]), ': ', df_output[[color_aes]],
                                '<br><b>Sample Size: ', c, "</b>"
                                )
                ) %>%
      #add_lines() %>%
      layout(xaxis = list(title = names(display_choices[display_choices == x_aes])), 
             yaxis = list(title = "Sample Size"),
             legend = list(title = list(text = names(display_choices[display_choices == color_aes])))
             )
    
    
    
    lst(text_output, df_output, plot_output)
  })
  
  # Call the calculateSampleSize function when the Calculate button is clicked
  observeEvent(input$calculate, {
    
    out <- calculateSampleSize()
    # Render the calculated result
    # output$text <- renderPrint(out$text_output)
    
    # Render the data frame
    output$data_frame <- renderTable(out$df_output)
    
    # Render the plot
    output$plot <- renderPlotly(out$plot_output)
    
    
  })
}
