library(tidyverse)
library(shiny)
library(shinyMatrix)
library(shinyvalidate)
library(plotly)
library(reactable)
# print("Helper.R")
# Short form for paste0
"%++%" <- function(LHS, RHS) paste0(LHS, RHS)

# Estimate the step size
step_fun <- function(s) {
  tmp <- (log(s, 10) + 1) |> floor()
  10^(tmp - 2)
}
# parse X value variation from the slider input
parse_x <- function(value, step_size, from = -2, to = 2) {
  from <- (from - value) / step_size
  to <- (to - value) / step_size
  k <- unique(c(value, value + seq(from = from, to = to) * step_size)) %>% na.omit()
  # print("\nvalue: " %++% value)
  # cat("\nk: " %++% k)
  k
}
# parse_x(0.8, 0.1, 0, 3)

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
  # Check inputs
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

  # Print inputs for debugging
  if (check) {
    cat("Inputs: \n")
    cat("Ec: ", Ec, "\n")
    cat("Ei: ", Ei, "\n")
    cat("SDi: ", SDi, "\n")
    cat("SDc: ", SDc, "\n")
    cat("cluster_size: ", cluster_size, "\n")
    cat("CVi: ", CVi, "\n")
    cat("CVc: ", CVc, "\n")
  }

  # Calculate expected value in intervention group
  if (method == "absolute") {
    Ei <- as.numeric(Ei) + Ec
  } else if (method == "relative") {
    Ei <- Ec * as.numeric(Ei)
  }

  # Print expected values for debugging
  if (check) {
    cat("Expected values: \n")
    cat("Choice: ", method, "\n")
    cat("Ec: ", Ec, "\n")
    cat("Ei: ", Ei, "\n")
    # cat("Ei - Ec: ", Ei - Ec, "\n")
  }

  # Calculate variance component 'a'
  a_top <- switch(outcome,
    "Mean" = SDi^2 + SDc^2,
    "Proportion" = {
      pbar <- (Ei + Ec) / 2
      2 * pbar * (1 - pbar)
    },
    "Incidence Rate" = Ei + Ec
  )
  a <- a_top / cluster_size

  # Calculate variance component 'b' and 'c'
  b <- CVi^2 * Ei^2 + CVc^2 * Ec^2
  c <- (Ei - Ec)^2

  # Print variance components for debugging
  if (check) {
    cat("Variance components: \n")
    cat("a: ", a, "\n")
    cat("b: ", b, "\n")
    cat("c: ", c, "\n")
  }

  # Calculate sample size
  zalpha <- abs(qnorm(alpha / 2))
  zbeta <- abs(qnorm(power))
  N <- ((0 + cluster) + (zalpha + zbeta)^2 * ((a + b) / c))

  # Return result
  if (ceiling_num) {
    return(ceiling(N))
  } else {
    return(round(N, 2))
  }
}


# ------
# A function to prepare a data frame for plotting based on given arguments.
df_prepare <- function(outcome = NA, method = NA, Ec = NA, Ei = NA,
                       SDc = NA, SDi = NA, CVc = NA, CVi = NA, cluster_size = NA,
                       alpha = 0.05, power = 0.8, cluster = FALSE, ceiling_num = TRUE, check = FALSE,
                       aes_x_col = "Ec", x_step_size = 0, x_step_range = c(-2, 2),
                       aes_color_col = "Ei", col_var = NA, expand = T, ...) {
  # Combine all arguments into a list
  arg_list <- lst(
    outcome, method, Ec, Ei, SDc, SDi, CVc, CVi, cluster_size, alpha, power, cluster,
    ceiling_num, check, ...
  )

  # Calculate x_step_size

  # Update x_step_range using the calculated x_step_size
  arg_list[[aes_x_col]] <- x_step_range <- parse_x(arg_list[[aes_x_col]],
    step_size = x_step_size,
    from = x_step_range[1], to = x_step_range[2]
  )


  arg_list[[aes_color_col]] <- na.omit(unique(c(arg_list[[aes_color_col]], col_var)))

  # Use expand_grid with the remaining arguments
  if (expand) {
    return(expand_grid(!!!arg_list))
  }
}




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
  tmp[, !sapply(tmp, anyNA)]
}


# names(df_prepare())

check_point <- function(condition, message = "Pass!") {
  # print("AAAAA")
  if (condition) cat(message)
}


# E.g.
# df_prepare(
#   outcome = "Proportion",
#   method = "direct",
#   Ei = c(0.6, 0.7, 0.8, 0.9), #
#   Ec = 0.5,
#   CVc = 0.8,
#   m = c(60),
#   alpha = 0.025,
#   cluster = T
#  ) %>%# str()
#  Add_samplesize()
#   rowwise() %>%
#   mutate(
#     c = SS_calculation2(
#     outcome = outcome[1],
#     method = method[1],
#     Ec = Ec,
#     Ei = Ei,
#     SDc = SDc,
#     SDi = SDi,
#     CVc = CVc,
#     CVi = CVi,
#     m = m,
#     alpha = alpha,
#     power = power,
#     cluster = cluster,
#     ceiling_num = T,
#     check = check
#   ))
# tmp


# object preparation

# cat("helper:", ls(), "\n")
