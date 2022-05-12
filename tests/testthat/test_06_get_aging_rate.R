
# Activate Libraries ------------------------------------------------------
library(demogmx)
library(dplyr)
library(testthat)


# Start unit tests --------------------------------------------------------
## Correct inputs ---------------------------------------------------------
test_that("correct inputs are valid", {
  # Valid parameter set
  expect_silent(get_aging_rate(v_state = c("National", "Mexico City"),
                               v_year = 1999,
                               v_sex = "Total",
                               v_age = c(10, 25, 55, 75)))
})


### State inputs ----------------------------------------------------------
test_that("state inputs are correct", {
  # Error in v_state
  expect_error(get_aging_rate(v_state    = "national", #National <- correct
                              v_year     = 1999,
                              v_sex      = "Total",
                              v_age      = c(10, 25, 55, 75)),
               regexp = paste('v_state must be a character element or vector containing at least one of the next names:',
                              paste(unique(df_mortrate_state_age_sex$state), collapse = ", "),
                              sep = "\n\n"))

  # Not an exact match
  expect_error(get_aging_rate(v_state = c("National", "Ags"), # Aguascalientes
                              v_year = 1999,
                              v_sex = "Total",
                              v_age = c(10, 25, 55, 75)),
               regexp = paste('v_state must be a character element or vector containing at least one of the next names:',
                              paste(unique(df_mortrate_state_age_sex$state), collapse = ", "),
                              sep = "\n\n"))

  # Not a character
  expect_error(get_aging_rate(v_state = 65, # <- Numeric
                              v_year = 1999,
                              v_sex = "Total",
                              v_age = c(10, 25, 55, 75)),
               regexp = paste('v_state must be a character element or vector containing at least one of the next names:',
                              paste(unique(df_mortrate_state_age_sex$state), collapse = ", "),
                              sep = "\n\n"))

  # Multiple errors
  expect_error(get_aging_rate(v_state = c("national", "Ags"), # Aguascalientes
                              v_year = 1999,
                              v_sex = "Total",
                              v_age = c(10, 25, 55, 75)),
               regexp = paste('v_state must be a character element or vector containing at least one of the next names:',
                              paste(unique(df_mortrate_state_age_sex$state), collapse = ", "),
                              sep = "\n\n"))
})


### Year inputs -----------------------------------------------------------
test_that("year inputs are correct", {
  # Outside bounds [1985, 2020]
  expect_error(get_aging_rate(v_state = c("National", "Aguascalientes"),
                              v_year = 1905,
                              v_sex = "Total",
                              v_age = c(10, 25, 55, 75)),
               regexp = "v_year must be a integer value or vector with values between 1985 and 2020")

  # Not an integer
  expect_error(get_aging_rate(v_state = c("National", "Aguascalientes"),
                              v_year = 1985.6,
                              v_sex = "Total",
                              v_age = c(10, 25, 55, 75)),
               regexp = "v_year must be a integer value or vector with values between 1985 and 2020")

  expect_error(get_aging_rate(v_state = c("National", "Aguascalientes"),
                              v_year = c(2000, 2010.6),
                              v_sex = "Total",
                              v_age = c(10, 25, 55, 75)),
               regexp = "v_year must be a integer value or vector with values between 1985 and 2020")

  # Not a number
  expect_error(get_aging_rate(v_state = c("National", "Aguascalientes"),
                              v_year = "aa",
                              v_sex = "Total",
                              v_age = c(10, 25, 55, 75)),
               regexp = "v_year must be a integer value or vector with values between 1985 and 2020")
})



### Sex inputs ------------------------------------------------------------
test_that("sex inputs are correct", {
  # Not an exact match
  expect_error(get_aging_rate(v_state = c("National", "Aguascalientes"),
                              v_year = c(2000, 2010, 2020),
                              v_sex = "Tot",
                              v_age = c(10, 25, 55, 75)),
               regexp = "v_sex must be a character element or vector containing at least one of the next names: 'Female', 'Male', 'Total'")

  # Not a character
  expect_error(get_aging_rate(v_state = c("National", "Aguascalientes"),
                              v_year = c(2000, 2010, 2020),
                              v_sex = 55,
                              v_age = c(10, 25, 55, 75)),
               regexp = "v_sex must be a character element or vector containing at least one of the next names: 'Female', 'Male', 'Total'")

  # Multiple errors
  expect_error(get_aging_rate(v_state = c("National", "Aguascalientes"),
                              v_year = c(2000, 2010, 2020),
                              v_sex = c(55, "Fem"),
                              v_age = c(10, 25, 55, 75)),
               regexp = "v_sex must be a character element or vector containing at least one of the next names: 'Female', 'Male', 'Total'")
})


### Age inputs -----------------------------------------------------------
test_that("Age inputs are correct", {
  # Age inputs are not integers
  expect_error(get_aging_rate(v_state = c("National", "Aguascalientes"),
                              v_year = c(2000, 2010, 2020),
                              v_sex = "Total",
                              v_age = 10.6),
               regexp = "v_age must be a integer value or vector with values between 0 and 89")

  # Age inputs out of bounds
  expect_error(get_aging_rate(v_state = c("National", "Aguascalientes"),
                              v_year = c(2000, 2010, 2020),
                              v_sex = "Total",
                              v_age = -6),
               regexp = "v_age must be a integer value or vector with values between 0 and 89")

  expect_error(get_aging_rate(v_state = c("National", "Aguascalientes"),
                              v_year = c(2000, 2010, 2020),
                              v_sex = "Total",
                              v_age = 120),
               regexp = "v_age must be a integer value or vector with values between 0 and 89")

  # Multiple errors
  expect_error(get_aging_rate(v_state = c("National", "Aguascalientes"),
                              v_year = c(2000, 2010, 2020),
                              v_sex = "Total",
                              v_age = c(-6, 15.6)),
               regexp = "v_age must be a integer value or vector with values between 0 and 89")
})


## Correct outputs ---------------------------------------------------------
### All possible vars -----------------------------------------------------

test_that("the output is correct when all the available vars are selected", {

  expect_silent(get_aging_rate(v_state    = unique(df_births_INEGI$state),
                               v_year     = seq(from = 1985, to = 2020, by = 1),
                               v_sex      = unique(df_mortrate_state_age_sex$sex),
                               v_age      = seq(from = 0, to = 89, by = 1)
                               )
                )
})


### Correct dimensions ----------------------------------------------------
test_that("the ungrouped output has the correct dimensions", {
  # Ungrouped
  df_ar <- get_aging_rate(v_state = c("Guerrero", "Mexico City"),
                          v_year = seq(from = 1985, to = 2020, by = 1),
                          v_sex = unique(df_mortrate_state_age_sex$sex),
                          v_age = seq(from = 0, to = 89, by = 10)
                          )

  years  <- 36
  states <- 2
  sexes  <- 3
  ages   <- 9
  size <- years*states*sexes*ages

  expect_equal(dim(df_ar)[1], size)

  # Grouped by ages
  df_ar <- get_aging_rate(v_state = c("Guerrero", "Mexico City"),
                          v_year = seq(from = 1985, to = 2020, by = 1),
                          v_sex = unique(df_mortrate_state_age_sex$sex),
                          v_age = seq(from = 0, to = 89, by = 10))

  years_grp  <- 36
  states_grp <- 2
  sexes_grp  <- 3
  age_grp    <- 9
  size_grp <- years_grp*states_grp*sexes_grp*age_grp

  expect_equal(dim(df_ar)[1], size_grp)

  # Only males
  df_ar <- get_aging_rate(v_state = c("Guerrero", "Mexico City"),
                          v_year = seq(from = 1985, to = 2020, by = 1),
                          v_sex = "Male",
                          v_age = seq(from = 0, to = 89, by = 10))

  sexes <- 1
  size <- years*states*sexes*ages

  expect_equal(dim(df_ar)[1], size)

  # Only males and one year
  df_ar <- get_aging_rate(v_state = c("Guerrero", "Mexico City"),
                          v_year = 2010,
                          v_sex = "Male",
                          v_age = seq(from = 0, to = 89, by = 10))

  years <- 1
  sexes <- 1
  size <- years*states*sexes*ages

  expect_equal(dim(df_ar)[1], size)

  # Only males, one year and one age
  df_ar <- get_aging_rate(v_state = c("Guerrero", "Mexico City"),
                          v_year = 2010,
                          v_sex = "Male",
                          v_age = 45)

  years <- 1
  sexes <- 1
  ages  <- 1
  size <- years*states*sexes*ages

  expect_equal(dim(df_ar)[1], size)
})
