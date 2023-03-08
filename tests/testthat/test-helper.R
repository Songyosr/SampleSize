library(testthat)

# -- SS_calculation 2
# Define test cases
test_that("SS_calculation2 returns expected sample sizes", {
  # Test case 1: Proportion, Direct, cluster = TRUE
  expect_equal(
    SS_calculation2(outcome = "Proportion", method = "direct", Ec = 0.5, CVc = 0.1, CVi = 0.1, cluster = TRUE),
    93
  )
  
  # Test case 2: Mean, Absolute, cluster = TRUE
  expect_equal(
    SS_calculation2(outcome = "Mean", method = "absolute", 
                    Ec = 10, Ei = 5, SDc = 3, SDi = 3, CVc = 0.1, CVi = 0.1, cluster = TRUE),
    16
  )
  
  # Test case 3: Incidence Rate, Relative, cluster = FALSE
  expect_equal(
    SS_calculation2(outcome = "Incidence Rate", method = "relative", 
                    Ec = 10, Ei = 1.5, RelativeEff = 0.5, cluster = FALSE),
    78
  )
})


test_that("step_fun returns the correct step size", {
  expect_equal(step_fun(25), 1)  # default dec and num
  expect_equal(step_fun(0.1), 0.001)  # small input value
  expect_equal(step_fun(9876543210), 1e9)  # large input value
  expect_equal(step_fun(25, dec = -2), 0.1)  # negative dec value
  expect_equal(step_fun(25, num = 2), 2)  # positive num value
})

# Define test cases
test_that("Custom utility functions work as expected", {
  # Test case 1: %++% concatenates two strings
  expect_equal("%++%"("hello", "world"), "helloworld")
  
  # Test case 2: %||% returns the non-NULL value
  expect_equal(NULL %||% 123, 123)
  expect_equal("A" %||% 123, "A")
  
  # Test case 3: parse_x generates the expected X values
  expect_equal(parse_x(10, 0.5, 9, 11), c(8, 9, 10, 11, 12))
  expect_equal(parse_x(5, 1, from = 0, to = 4), c(0, 5, 4, 3, 2, 1))
  
  # Test case 4: check_point prints the correct message when the condition is true
  expect_silent(check_point(TRUE, "Test message", h = 2, n = 3))
  
  # Test case 5: set_limit returns the expected limit range
  expect_equal(set_limit("Mean", "direct"), c(min = -Inf, max = Inf))
  expect_equal(set_limit("Proportion", "absolute"), c(min = -1, max = 1))
  expect_equal(set_limit("Incidence Rate", "relative"), c(min = 0, max = Inf))
  
  # Test case 6oyt: set_icon returns the expected icon for each method
  expect_equal(set_icon("direct"), list(icon("people-group")))
  expect_equal(set_icon("absolute"), list(icon("plus-minus")))
  expect_equal(set_icon("relative"), list(icon("divide")))
  expect_equal(set_icon("unknown"), list(icon("question")))
})
