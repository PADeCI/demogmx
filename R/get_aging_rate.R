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
#' @param age_groups Logical. Specifies whether to aggregate the output by
#' age groups.
#'
#' @return A demographic dataset containing the selected year
#' (group, when \code{year_groups = TRUE}), state, state code (CVE_GEO), sex,
#' the aging population and the aging rate.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' df_aging <- get_aging_rate(v_state = "National", v_year = seq(1985, 2020),
#' v_sex = "Total", v_age = c(0, 89), age_groups = T)
get_aging_rate <- function(v_state    = "National",
                           v_year     = 2020,
                           v_sex      = c("Female", "Male", "Total"),
                           v_age      = c(0, 15, 24, 36),
                           age_groups = FALSE) {
  # Sanity Checks -----------------------------------------------------------
  # Check if selected state(s) is(are) part of the available options set
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
  # Check age groups option
  if (!is.logical(age_groups) | length(age_groups) != 1) {
    stop("age_groups must be a logical value (TRUE / FALSE) of length 1")
  }


  # Execute auxiliary functions ---------------------------------------------
  # Obtain births
  df_births <- get_births_INEGI(v_state = v_state, v_year = v_year,
                                v_sex = v_sex, year_groups = FALSE)
  # Obtain population
  df_pop <- get_population(v_state = v_state, v_year = v_year, v_sex = v_sex,
                           v_age = v_age, age_groups = age_groups)
  # Obtain mortality
  df_mort <- get_deaths(v_state = v_state, v_year = v_year, v_sex = v_sex,
                        v_age = v_age, age_groups = age_groups)
  # Obtain migration (emigration and immigration)
  df_mig <- get_migration(v_state = v_state, v_year = v_year, v_sex = v_sex,
                          v_age = v_age, v_type = "Total",
                          age_groups = age_groups)

  # Manipulate data ---------------------------------------------------------
  # Obtain character string based in whether the age is grouped or not
  str_age_grp <- ifelse(test = age_groups, yes = "age_group", no = "age")

  # Merge population, mortality, and migration dataframes
  df_aux_1 <- df_pop %>%
    left_join(x = ., y = df_mort,
              by = c("year", "state", "CVE_GEO","sex", str_age_grp)) %>%
    left_join(x = ., y = df_mig,
              by = c("year", "state", "CVE_GEO","sex", str_age_grp))

  # Calculate the aging population and the aging rate
  df_outcome <- df_aux_1 %>%
    mutate(aging_pop = population + immigrants - emigrants - deaths) %>%
    mutate(aging_rate = aging_pop/population) %>%
    select(year, state, CVE_GEO, sex, all_of(str_age_grp), aging_pop, aging_rate) %>%
    dplyr::filter(complete.cases(.))

  return(df_outcome)
}
