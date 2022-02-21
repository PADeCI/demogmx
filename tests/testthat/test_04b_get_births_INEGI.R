
# Activate Libraries ------------------------------------------------------
library(demogmx)
library(dplyr)
library(testthat)


# Start unit tests --------------------------------------------------------
## Correct inputs ---------------------------------------------------------
test_that("correct inputs are valid", {
  # Valid parameter set
  expect_silent(get_births_INEGI(v_state = c("National", "Mexico City"),
                                 v_year = 2000,
                                 v_sex = c("Female", "Total"),
                                 year_groups = TRUE))
})


### State inputs ----------------------------------------------------------
test_that("state inputs are correct", {
  # Error in v_state
  expect_error(get_births_INEGI(v_state = "national", #National <- correct
                                v_year = 2000,
                                v_sex = c("Female", "Total"),
                                year_groups = TRUE),
               regexp = paste('v_state must be a character element or vector containing at least one of the next names:',
                              paste(unique(df_births_INEGI$state), collapse = ", "),
                              sep = "\n\n") )

  # Not an exact match
  expect_error(get_births_INEGI(v_state = c("National", "Ags"), # Aguascalientes
                                v_year = 2000,
                                v_sex = c("Female", "Total"),
                                year_groups = TRUE),
               regexp = paste('v_state must be a character element or vector containing at least one of the next names:',
                              paste(unique(df_births_INEGI$state), collapse = ", "),
                              sep = "\n\n"))

  # Not a character
  expect_error(get_births_INEGI(v_state = 65,
                                v_year = 2000,
                                v_sex = c("Female", "Total"),
                                year_groups = TRUE),
               regexp = paste('v_state must be a character element or vector containing at least one of the next names:',
                              paste(unique(df_births_INEGI$state), collapse = ", "),
                              sep = "\n\n"))

  # Multiple errors
  expect_error(get_births_INEGI(v_state = c("national", "Ags"), # Aguascalientes
                                v_year = 2000,
                                v_sex = c("Female", "Total"),
                                year_groups = TRUE),
               regexp = paste('v_state must be a character element or vector containing at least one of the next names:',
                              paste(unique(df_births_INEGI$state), collapse = ", "),
                              sep = "\n\n"))
})


### Year inputs -----------------------------------------------------------
test_that("year inputs are correct", {
  # Outside bounds [1985, 2020]
  expect_error(get_births_INEGI(v_state = c("National", "Aguascalientes"),
                                v_year = 1975,
                                v_sex = c("Female", "Total"),
                                year_groups = TRUE),
               regexp = "v_year must be a integer value or vector with values between 1985 and 2020")

  # Not an integer
  expect_error(get_births_INEGI(v_state = c("National", "Aguascalientes"),
                                v_year = 1985.6,
                                v_sex = c("Female", "Total"),
                                year_groups = TRUE),
               regexp = "v_year must be a integer value or vector with values between 1985 and 2020")

  expect_error(get_births_INEGI(v_state = c("National", "Aguascalientes"),
                                v_year = c(2000, 2010.6),
                                v_sex = c("Female", "Total"),
                                year_groups = TRUE),
               regexp = "v_year must be a integer value or vector with values between 1985 and 2020")

  # Not a number
  expect_error(get_births_INEGI(v_state = c("National", "Aguascalientes"),
                                v_year = "aa",
                                v_sex = c("Female", "Total"),
                                year_groups = TRUE),
               regexp = "v_year must be a integer value or vector with values between 1985 and 2020")
})

### Sex inputs ------------------------------------------------------------
test_that("sex inputs are correct", {
  # Not an exact match
  expect_error(get_births_INEGI(v_state = c("National", "Aguascalientes"),
                                v_year = c(1985, 1995, 2005, 2015),
                                v_sex = "Tot",
                                year_groups = TRUE),
               regexp = "v_sex must be a character element or vector containing at least one of the next names: 'Female', 'Male', 'Total'")

  # Not a character
  expect_error(get_births_INEGI(v_state = c("National", "Aguascalientes"),
                                v_year = c(1985, 1995, 2005, 2015),
                                v_sex = 55,
                                year_groups = TRUE),
               regexp = "v_sex must be a character element or vector containing at least one of the next names: 'Female', 'Male', 'Total'")

  # Multiple errors
  expect_error(get_births_INEGI(v_state = c("National", "Aguascalientes"),
                                v_year = c(1985, 1995, 2005, 2015),
                                v_sex = c(55, "Fem"),
                                year_groups = TRUE),
               regexp = "v_sex must be a character element or vector containing at least one of the next names: 'Female', 'Male', 'Total'")
})



### Year groups inputs ----------------------------------------------------
test_that("year groups are correct", {
  # age groups is not a logical variable
  expect_error(get_births_INEGI(v_state = c("National", "Aguascalientes"),
                                v_year = c(1995, 2005, 2015),
                                v_sex = c("Female", "Total"),
                                year_groups = 55),
               regexp = "year_groups must be a logical value (TRUE / FALSE)",
               fixed = TRUE)

  # Length of year_groups greater than one
  expect_warning(get_births_INEGI(v_state = c("National", "Aguascalientes"),
                                  v_year = c(1995, 2005, 2015),
                                  v_sex = c("Female", "Total"),
                                  year_groups = c(TRUE, FALSE)))
})


## Correct outputs --------------------------------------------------------
### All possible vars -----------------------------------------------------

test_that("the output is correct when all the available vars are selected", {

  df_gbi <- get_births_INEGI(v_state = unique(df_births_INEGI$state),
                             v_year = seq(from = 1985, to = 2020, by = 1),
                             v_sex = c("Female", "Male", "Total"),
                             year_groups = FALSE) # It has to be FALSE

  # It should have the same number of rows of the expanded migration dataset
  expect_equal(dim(df_gbi)[1], dim(df_births_INEGI)[1])
})



### Correct dimensions ----------------------------------------------------
test_that("the ungrouped output has the correct dimensions", {
  # Ungrouped
  df_gbi <- get_births_INEGI(v_state = c("Guerrero", "Mexico City"),
                             v_year = seq(from = 1985, to = 2020, by = 1),
                             v_sex = c("Female", "Total"),
                             year_groups = FALSE)

  years  <- 36
  states <- 2
  sexes  <- 2
  size <- years*states*sexes

  expect_equal(dim(df_gbi)[1], size)

  # Grouped by years
  df_gbi <- get_births_INEGI(v_state = c("Guerrero", "Mexico City"),
                             v_year = seq(from = 1985, to = 2020, by = 10),
                             v_sex = c("Female", "Total"),
                             year_groups = TRUE)

  years_grp  <- 4
  states_grp <- 2
  sexes      <- 2
  size_grp <- years_grp*states_grp*sexes

  expect_equal(dim(df_gbi)[1], size_grp)

  # Only one year
  df_gbi <- get_births_INEGI(v_state = c("Guerrero", "Mexico City"),
                             v_year = 2010,
                             v_sex = c("Female", "Total"),
                             year_groups = FALSE)

  years <- 1
  size <- years*states*sexes

  expect_equal(dim(df_gbi)[1], size)

  # Only one year and on state
  df_gbi <- get_births_INEGI(v_state = "Guerrero",
                             v_year = 2010,
                             v_sex = c("Female", "Total"),
                             year_groups = FALSE)

  years <- 1
  states <- 1
  size <- years*states*sexes

  expect_equal(dim(df_gbi)[1], size)
})
