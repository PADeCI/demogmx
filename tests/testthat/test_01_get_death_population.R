
# Activate Libraries ------------------------------------------------------
library(demogmx)
library(dplyr)
library(testthat)


# Start unit tests --------------------------------------------------------

## Correct inputs ---------------------------------------------------------
test_that("correct inputs are valid", {
  # Valid parameter set
  expect_silent(get_death_population(v_state = c("National", "Mexico City"),
                                     v_year = 1976,
                                     v_sex = "Total",
                                     v_age = c(10, 25, 55, 75),
                                     age_groups = TRUE))
  })


### State inputs ----------------------------------------------------------
test_that("state inputs are correct", {
  # Error in v_state
  expect_error(get_death_population(v_state = "national", #National <- correct
                                    v_year = 1976,
                                    v_sex = "Total",
                                    v_age = c(10, 25, 55, 75),
                                    age_groups = TRUE),
               regexp = paste('v_state must be a character element or vector containing at least one of the next names:',
                              paste(unique(df_mortrate_state_age_sex$state), collapse = ", "),
                              sep = "\n\n") )

  # Not an exact match
  expect_error(get_death_population(v_state = c("National", "Ags"), # Aguascalientes
                                    v_year = 1976,
                                    v_sex = "Total",
                                    v_age = c(10, 25, 55, 75),
                                    age_groups = TRUE),
               regexp = paste('v_state must be a character element or vector containing at least one of the next names:',
                              paste(unique(df_mortrate_state_age_sex$state), collapse = ", "),
                              sep = "\n\n"))

  # Not a character
  expect_error(get_death_population(v_state = 65,
                                    v_year = 1976,
                                    v_sex = "Total",
                                    v_age = c(10, 25, 55, 75),
                                    age_groups = TRUE),
               regexp = paste('v_state must be a character element or vector containing at least one of the next names:',
                              paste(unique(df_mortrate_state_age_sex$state), collapse = ", "),
                              sep = "\n\n"))

  # Multiple errors
  expect_error(get_death_population(v_state = c("national", "Ags"), # Aguascalientes
                                    v_year = 1976,
                                    v_sex = "Total",
                                    v_age = c(10, 25, 55, 75),
                                    age_groups = TRUE),
               regexp = paste('v_state must be a character element or vector containing at least one of the next names:',
                              paste(unique(df_mortrate_state_age_sex$state), collapse = ", "),
                              sep = "\n\n"))
  })


### Year inputs -----------------------------------------------------------
test_that("year inputs are correct", {
  # Outside bounds [1970, 2050]
  expect_error(get_death_population(v_state = c("National", "Aguascalientes"),
                                    v_year = 1905,
                                    v_sex = "Total",
                                    v_age = c(10, 25, 55, 75),
                                    age_groups = TRUE),
               regexp = "v_year must be a integer value or vector with values between 1970 and 2050")

  # Not an integer
  expect_error(get_death_population(v_state = c("National", "Aguascalientes"),
                                    v_year = 1985.6,
                                    v_sex = "Total",
                                    v_age = c(10, 25, 55, 75),
                                    age_groups = TRUE),
               regexp = "v_year must be a integer value or vector with values between 1970 and 2050")

  expect_error(get_death_population(v_state = c("National", "Aguascalientes"),
                                    v_year = c(2000, 2010.6),
                                    v_sex = "Total",
                                    v_age = c(10, 25, 55, 75),
                                    age_groups = TRUE),
               regexp = "v_year must be a integer value or vector with values between 1970 and 2050")

  # Not a number
  expect_error(get_death_population(v_state = c("National", "Aguascalientes"),
                                    v_year = "aa",
                                    v_sex = "Total",
                                    v_age = c(10, 25, 55, 75),
                                    age_groups = TRUE),
               regexp = "v_year must be a integer value or vector with values between 1970 and 2050")
  })



### Sex inputs ------------------------------------------------------------
test_that("sex inputs are correct", {
  # Not an exact match
  expect_error(get_death_population(v_state = c("National", "Aguascalientes"),
                                    v_year = c(2000, 2010, 2025),
                                    v_sex = "Tot",
                                    v_age = c(10, 25, 55, 75),
                                    age_groups = TRUE),
               regexp = "v_sex must be a character element or vector containing at least one of the next names: 'Female', 'Male', 'Total'")

  # Not a character
  expect_error(get_death_population(v_state = c("National", "Aguascalientes"),
                                    v_year = c(2000, 2010, 2025),
                                    v_sex = 55,
                                    v_age = c(10, 25, 55, 75),
                                    age_groups = TRUE),
               regexp = "v_sex must be a character element or vector containing at least one of the next names: 'Female', 'Male', 'Total'")

  # Multiple errors
  expect_error(get_death_population(v_state = c("National", "Aguascalientes"),
                                    v_year = c(2000, 2010, 2025),
                                    v_sex = c(55, "Fem"),
                                    v_age = c(10, 25, 55, 75),
                                    age_groups = TRUE),
               regexp = "v_sex must be a character element or vector containing at least one of the next names: 'Female', 'Male', 'Total'")
  })


### Age inputs -----------------------------------------------------------
test_that("Age inputs are correct", {
  # Age inputs are not integers
  expect_error(get_death_population(v_state = c("National", "Aguascalientes"),
                                    v_year = c(2000, 2010, 2025),
                                    v_sex = "Total",
                                    v_age = 10.6,
                                    age_groups = TRUE),
               regexp = "v_age must be a integer value or vector with values between 0 and 109")

  # Age inputs out of bounds
  expect_error(get_death_population(v_state = c("National", "Aguascalientes"),
                                    v_year = c(2000, 2010, 2025),
                                    v_sex = "Total",
                                    v_age = -6,
                                    age_groups = TRUE),
               regexp = "v_age must be a integer value or vector with values between 0 and 109")

  expect_error(get_death_population(v_state = c("National", "Aguascalientes"),
                                    v_year = c(2000, 2010, 2025),
                                    v_sex = "Total",
                                    v_age = 120,
                                    age_groups = TRUE),
               regexp = "v_age must be a integer value or vector with values between 0 and 109")

  # Multiple errors
  expect_error(get_death_population(v_state = c("National", "Aguascalientes"),
                                    v_year = c(2000, 2010, 2025),
                                    v_sex = "Total",
                                    v_age = c(-6, 15.6),
                                    age_groups = TRUE),
               regexp = "v_age must be a integer value or vector with values between 0 and 109")
  })


### Age groups inputs ------------------------------------------------------
test_that("age groups are correct", {
  # age groups is not a logical variable
  expect_error(get_death_population(v_state = c("National", "Aguascalientes"),
                                    v_year = c(2000, 2010, 2025),
                                    v_sex = "Total",
                                    v_age = seq(0, 15),
                                    age_groups = 55),
               regexp = "age_groups must be a logical value (TRUE / FALSE)",
               fixed = TRUE)

  # length of age_groups greater than one
  expect_warning(get_death_population(v_state = c("National", "Aguascalientes"),
                                    v_year = c(2000, 2010, 2025),
                                    v_sex = "Total",
                                    v_age = seq(0, 15),
                                    age_groups = c(TRUE, FALSE)))


})



## Correct outputs --------------------------------------------------------
### All possible vars -----------------------------------------------------
test_that("the output is correct when all the available vars are selected", {

  df_gdp <- get_death_population(v_state = unique(df_mortrate_state_age_sex$state),
                       v_year = unique(df_mortrate_state_age_sex$year),
                       v_sex = unique(df_mortrate_state_age_sex$sex),
                       v_age = unique(df_mortrate_state_age_sex$age),
                       age_groups = FALSE) # It has to be FALSE

  # It should have the same number of rows of the original dataset
  expect_equal(dim(df_gdp)[1], dim(df_mortrate_state_age_sex)[1])
})

### Correct dimensions ----------------------------------------------------
test_that("the ungrouped output has the correct dimensions", {
  # Ungrouped
  df_gdp <- get_death_population(v_state = c("National", "Michoacan"),
                                 v_year  = c(1980, 1990, 2000),
                                 v_sex   = c("Female", "Male", "Total"),
                                 v_age   = c(10, 20, 30, 40, 50, 75),
                                 age_groups = FALSE)

  expect_equal(dim(df_gdp)[1], 108)

  # Grouped
  df_gdp <- get_death_population(v_state = c("National", "Michoacan"),
                                 v_year  = c(1980, 1990, 2000),
                                 v_sex   = c("Female", "Male", "Total"),
                                 v_age   = c(10, 20, 30, 40, 50, 75),
                                 age_groups = TRUE)

  expect_equal(dim(df_gdp)[1], 108)

  # Only males
  df_gdp <- get_death_population(v_state = c("National", "Michoacan"),
                                 v_year  = c(1980, 1990, 2000),
                                 v_sex   = "Male",
                                 v_age   = c(10, 20, 30, 40, 50, 75),
                                 age_groups = FALSE)

  expect_equal(dim(df_gdp)[1], 36)

  # Only males and one year
  df_gdp <- get_death_population(v_state = c("National", "Michoacan"),
                                 v_year  = 2000,
                                 v_sex   = "Male",
                                 v_age   = c(10, 20, 30, 40, 50, 75),
                                 age_groups = FALSE)

  expect_equal(dim(df_gdp)[1], 12)

  # Only males, one year and one age
  df_gdp <- get_death_population(v_state = c("National", "Michoacan"),
                                 v_year  = 2000,
                                 v_sex   = "Male",
                                 v_age   = 46,
                                 age_groups = FALSE)

  expect_equal(dim(df_gdp)[1], 2)
})
