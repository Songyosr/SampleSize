# ---- Check for packages availability - Install & call them
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, shiny, shinyvalidate, shinyWidgets, plotly, reactable)

# library(tidyverse)
# library(shiny)
# library(shinyvalidate)
# library(shinyWidgets)
# library(plotly)
# library(reactable)

# Helper function -----

# %++% ------
# Short form for paste0
"%++%" <- function(LHS, RHS) paste0(LHS, RHS)

# %||% ------
# Short form for NULL filtering gate
"%||%" <- function(LHS, RHS){
  if(is.null(LHS)) RHS else LHS
}
# e.g.
# NULL %||% 123; "A" %||% 123

# step_fun ------
# Estimate the step size
step_fun <- function(s, dec = -1, num = 1) {
  # Compute the nearest power of 10 that is less than or equal to s, with an
  # optional decimal shift specified by dec. The default of -1 means that the
  # decimal point is shifted one place to the left, i.e., 25 -> 2.0.
  exponent <- floor(log10(s) + dec)
  power_of_10 <- 10^exponent
  
  # Scale the power of 10 by the numeric multiplier num and return the result
  step_size <- power_of_10 * num
  step_size
}

# parse_x ------
# parse X value variation from the slider input
parse_x <- function(value, step_size, from = -2, to = 2) {
  # Compute the range of x values directly
  x_range <- seq(from = from, to = to, by = step_size)
  
  # Compute the x values corresponding to the range and the current value
  x_values <- c(value, x_range)
  
  # Return unique non-NA x values
  unique(x_values[!is.na(x_values)])
}


# check_point -----
# A helper function to check a given condition and print a message if the condition is true
check_point <- function(condition, msg = NULL, h = 1, n =1) {
  if (condition){
    if (is.null(msg)) msg <- "... Ok"
    else {
      msg <- paste0(c(rep("\n", n), rep("   ", h-1),
                    "Checking [ ", msg, " ]"), collapse = "")
    }
    cat(msg)
  } 
}
# check_point(1,"print"); check_point(1,"subprint", 2);check_point(1)

## Utility function
# set limit ------
set_limit <- function(outcome = "Mean", method = "direct"){
  if(outcome == "Mean" | method == "oddsRatio"){
    return(c('min' = -Inf, "max" = Inf))
  } else if(method == "relative") {
    return(c('min' = 0, "max" = Inf))
  } else {
    base <- c(-1*(method != "direct") ,1) 
    times <- ifelse(outcome != "Incidence Rate", 1, Inf)
    tmp <- base*times
    tmp[is.nan(tmp)] <- 0
    setNames(tmp, c('min', 'max'))
  }
}

# set_icon -----
# Function to set icon for each type of method
set_icon <- function(method){
  case_when(
    method == "direct" ~ list(icon("people-group")),
    method == "absolute" ~ list(icon("plus-minus")),
    method == "relative" ~ list(icon("divide")),
    TRUE ~ list(icon("question"))
  )
}

# Example for set_* ----
# mt <- c("direct", "absolute", "relative")
# oc <- c("Mean", "Proportion", "Incidence Rate") 
# test_dt <- expand_grid(mt, oc)
# test_dt %>% rowwise() %>% 
#   mutate(min_max = paste(set_limit(oc, mt), collapse = ","),
#          icons = set_icon(mt))  %>%
#   pivot_wider(names_from = oc, values_from =  c(min_max, icons))



# SS_calculation2 ------
# Function to calculate the sample size required
SS_calculation2 <- function(outcome = c("Proportion", "Mean", "Incidence Rate"),
                            method = c("direct", "absolute", "relative"),
                            Ec, Ei = NA,
                            AbsoluteEff = NA, RelativeEff = NA,
                            SDi = NULL, SDc = NULL,
                            cluster_size = NA, CVi = NA, CVc = NA,
                            alpha = 0.05, power = 0.8,
                            cluster = FALSE, ceiling_num = FALSE,
                            check = FALSE) {
  # Some boring statistical calculations to calculate the sample size required
  # But hey, at least it's better than calculating your taxes, right?
  outcomes <- match.arg(outcome)
  method <- match.arg(method)
  Ec <- as.numeric(Ec)
  if (is.null(SDi)) SDi <- SDc
  if (!cluster && !all(is.na(cluster_size), is.na(CVi), is.na(CVc))) {
    stop("We only accept 'm' and 'CV*' in a cluster design. Please set 'cluster = TRUE'.")
  }
  if (!cluster) {
    cluster_size <- 1
    CVc <- 0
  }
  if (is.na(CVi)) CVi <- CVc
  
  # Calculate expected value in intervention group
  if (method == "absolute") {
    Ei <- as.numeric(Ei) + Ec
  } else if (method == "relative") {
    Ei <- Ec * as.numeric(Ei)
  }
  
  # More boring calculations
  # Might need to update soon to a more generalized formula
  # Also we might need to consider unequal size between two group
  zalpha <- abs(qnorm(alpha / 2))
  zbeta <- abs(qnorm(power))
  a_top <- switch(outcome,
                  "Mean" = SDi^2 + SDc^2,
                  "Proportion" = {
                    pbar <- (Ei + Ec) / 2
                    2 * pbar * (1 - pbar)
                  },
                  "Incidence Rate" = Ei + Ec
  )
  a <- a_top / cluster_size
  b <- CVi^2 * Ei^2 + CVc^2 * Ec^2
  c <- (Ei - Ec)^2
  N <- ((0 + cluster) + (zalpha + zbeta)^2 * ((a + b) / c))
  # More boring calculations
  if (ceiling_num) {
    return(ceiling(N))
  } else {
    return(round(N, 2))
  }
}

# df_prepare ------
# A function to prepare a data frame for plotting based on given arguments.
df_prepare <- function(outcome = NA, method = NA, Ec = NA, Ei = NA,
                       SDc = NA, SDi = NA, CVc = NA, CVi = NA, cluster_size = NA,
                       alpha = 0.05, power = 0.8, cluster = FALSE, ceiling_num = TRUE, check = FALSE,
                       aes_x_col = "Ec", x_step_size = 0, x_step_range = c(-2, 2),
                       aes_color_col = "Ei", col_var = NA, expand = T, ...) {
  
  # If outcome is not mean: set SDi, SDc values to NA
  if(outcome != 'Mean') {SDc <- SDi <- NA}
  
  # If not a clusterd design : set CVi, CVc, cluster_size values to NA
  if(cluster == F) {CVc <- CVi <- cluster_size <- NA}
  
  # Combine all arguments into a list
  arg_list <- lst(
    outcome, method, Ec, Ei, SDc, SDi, CVc, CVi, cluster_size, alpha, power, cluster,
    ceiling_num, check, ...
  )
  
  # Calculate x_step_size based on the given step size
  x_step_size <- (x_step_size)
  
  # Update x_step_range using the calculated x_step_size
  arg_list[[aes_x_col]]  <- parse_x(arg_list[[aes_x_col]],
                                                   step_size = x_step_size,
                                                   from = x_step_range[1], to = x_step_range[2]
  )
  
  # Add the variable to the color variable list, and remove any NAs
  arg_list[[aes_color_col]] <- na.omit(unique(c(arg_list[[aes_color_col]], col_var)))
  
  # Use expand_grid with the remaining arguments
  if (expand) {
    return(expand_grid(!!!arg_list))
  }
}

# Add_samplesize ------
# A function to add a column of sample sizes to a given data frame based on input arguments
Add_samplesize <- function(data, ...) {
  tmp <- data %>%
    rowwise() %>%
    mutate(
      c = SS_calculation2(
        outcome = outcome[1],
        method = method[1],
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
        ceiling_num = F,
        check = check
      )
    ) %>%
    ungroup()
  
  # Return only columns without any missing values
  tmp[, !sapply(tmp, anyNA)]
}





