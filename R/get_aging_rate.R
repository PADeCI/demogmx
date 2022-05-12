##************************************************************************
## Script Name: get_migration
##
## Created: March, 2022
## Authors:
## - David Garibay-Treviño, MSc
##
## GitHub:
## - du-gartre
##
##************************************************************************


#' Load aging rate data
#'
#' \code{get_aging_rate} is a function that allows the user to get a demographic
#' dataset regarding the aging rate based on the given parameters.
#'
#' @param v_state Character vector specifying the state(s) that the function
#' will return.
#' @param v_year Numeric vector that specifies the year(s) to return. Must have
#' numbers between 1985 and 2020. Default is 2021.
#' @param v_sex Character vector selecting sex. Options: Female, Male and Total.
#' @param v_age Numeric vector that specifies the age(s) to return. Must have
#' values between 0 and 89.
#'
#' @return A demographic dataset containing the selected year
#' (group, when \code{year_groups = TRUE}), state, state code (CVE_GEO), sex,
#' the aging population and the aging rate.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' get_aging_rate(v_state = "National",
#'                v_year = seq(1991, 1995),
#'                v_sex = "Total",
#'                v_age = c(0, 89))
#'
get_aging_rate <- function(v_state    = "National",
                           v_year     = 2020,
                           v_sex      = c("Female", "Male", "Total"),
                           v_age      = c(0, 15, 24, 36)) {
  # # Sanity Checks -----------------------------------------------------------
  # # Check if selected state(s) is(are) part of the available options set
  if (!all(v_state %in% unique(df_births_INEGI$state))) {
    stop("v_state must be a character element or vector containing at least one of the next names:\n\n",
         paste(unique(df_mortrate_state_age_sex$state), collapse = ", "))
  }
  # Check if selected sex is part of the available options set
  if (!all(v_sex %in% unique(df_mortrate_state_age_sex$sex))) {
    stop(stop("v_sex must be a character element or vector containing at least one of the next names: 'Female', 'Male', 'Total'"))
  }
  # Check selected year
  if (!all(v_year %in% seq(1985, 2020))) {
    stop("v_year must be a integer value or vector with values between 1985 and 2020")
  }
  # Check selected age(s)
  if (!all(v_age %in% seq(0, 89))) {
    stop("v_age must be a integer value or vector with values between 0 and 89")
  }


  if (0 %in% v_age) {
    # Execute auxiliary functions ---------------------------------------------
    # Obtain births
    df_births <- get_births_INEGI(v_state = v_state, v_year = v_year,
                                  v_sex = v_sex, year_groups = FALSE) %>%
      mutate(age = 0)

    # Obtain population
    df_pop <- get_population(v_state = v_state, v_year = v_year, v_sex = v_sex,
                             v_age = v_age, age_groups = FALSE)
    # Obtain mortality
    df_mort <- get_deaths(v_state = v_state, v_year = v_year, v_sex = v_sex,
                          v_age = v_age, age_groups = FALSE)
    # Obtain migration (emigration and immigration)
    df_mig <- get_migration(v_state = v_state, v_year = v_year, v_sex = v_sex,
                            v_age = v_age, v_type = "Total",
                            age_groups = FALSE)

    # Merge births, population, migration and mortality dataframes
    # When age == 0
    df_aux_age_0 <- df_births %>%
      left_join(x = ., y = dplyr::filter(df_pop, age == 0),
                by = c("year", "state", "CVE_GEO","sex", "age")) %>%
      left_join(x = ., y = dplyr::filter(df_mort, age == 0),
                by = c("year", "state", "CVE_GEO","sex", "age")) %>%
      left_join(x = ., y = dplyr::filter(df_mig, age == 0),
                by = c("year", "state", "CVE_GEO","sex", "age")) %>%
      # Calculate aging rate
      mutate(aging_pop = births + immigrants - emigrants - deaths) %>%
      mutate(aging_rate = aging_pop/population) %>%
      select(year, state, CVE_GEO, sex, age, aging_pop, aging_rate) %>%
      dplyr::filter(complete.cases(.))

    # Merge the data for the rest of the ages
    df_aux_1 <- dplyr::filter(df_pop, age != 0) %>%
      left_join(x = ., y = dplyr::filter(df_mort, age != 0),
                by = c("year", "state", "CVE_GEO","sex", "age")) %>%
      left_join(x = ., y = dplyr::filter(df_mig, age != 0),
                by = c("year", "state", "CVE_GEO","sex", "age")) %>%
      # Calculate aging rate
      mutate(aging_pop = population + immigrants - emigrants - deaths) %>%
      mutate(aging_rate = aging_pop/population) %>%
      select(year, state, CVE_GEO, sex, age, aging_pop, aging_rate) %>%
      dplyr::filter(complete.cases(.))

    # Obtain dataframe for age 0 and the rest of the ages
    df_outcome <- dplyr::bind_rows(df_aux_age_0, df_aux_1)

  } else {

    # Obtain population
    df_pop <- get_population(v_state = v_state, v_year = v_year, v_sex = v_sex,
                             v_age = v_age, age_groups = FALSE)
    # Obtain mortality
    df_mort <- get_deaths(v_state = v_state, v_year = v_year, v_sex = v_sex,
                          v_age = v_age, age_groups = FALSE)
    # Obtain migration (emigration and immigration)
    df_mig <- get_migration(v_state = v_state, v_year = v_year, v_sex = v_sex,
                            v_age = v_age, v_type = "Total",
                            age_groups = FALSE)

    df_outcome <- df_pop %>%
      left_join(x = ., y = df_mort,
                by = c("year", "state", "CVE_GEO","sex", "age")) %>%
      left_join(x = ., y = df_mig,
                by = c("year", "state", "CVE_GEO","sex", "age")) %>%
      # Calculate aging rate
      mutate(aging_pop = population + immigrants - emigrants - deaths) %>%
      mutate(aging_rate = aging_pop/population) %>%
      select(year, state, CVE_GEO, sex, age, aging_pop, aging_rate) %>%
      dplyr::filter(complete.cases(.))
  }


  return(df_outcome)
}
