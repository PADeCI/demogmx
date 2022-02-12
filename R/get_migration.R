##************************************************************************
## Script Name: get_migration
## Purpose:
##
## Created: February, 2022
## Authors:
## - David Garibay-Treviño, MSc
##
## GitHub:
## - du-gartre
##
##************************************************************************

#' Get migration
#'
#' \code{get_migration} Function that allows user to get a demographic dataset
#' regarding migration information based on the parameter set given by the
#' user
#'
#' @param v_state State(s) of desired data.
#' @param v_year Year(s) of desired data. Must have numbers between 1970 and
#'  2050.
#' @param v_sex Vector selecting sex. Options: Female, Male and Total.
#' @param v_age Specifies the age bins to aggregate and return.
#' @param age_groups Specifies whether to aggregate the output by age groups.
#' @import tidyr
#' @return A demographic dataset based on specified parameters.
#' @param export
#'
#' @examples
#' get_population(v_states =  "Chiapas", v_year = 2015, v_sex = "Total",
#' v_age = c(0, 15, 45, 75), age_groups = FALSE)
#'
#' get_population(v_states = c("Aguascalientes", "Campeche"),
#' v_year = c(2010, 2021), v_sex = "Male", v_age = c(0, 15, 45, 75),
#' age_groups = TRUE)
#'
#' @export
get_migration <- function(v_states   = "Aguascalientes",
                          # v_year   = "2020-2025",
                          v_year     = c(2000, 2010, 2025),
                          v_sex      = c("Female", "Male", "Total"),
                          # v_age    = "20-24",
                          v_age      = c(0, 15, 24, 36),
                          v_type     = c("Interstate", "International", "Total"),
                          age_groups = TRUE) {

  # Import base data --------------------------------------------------------

  # Sanity Checks -----------------------------------------------------------
  # Check if selected state(s) is(are) part of the available options set
  if (!all(v_states %in% unique(df_migration$state))) {
    stop("v_states must be a character element or vector containing at least one of the next names: \n \n",
         paste(levels(df_migration$state), collapse = ", "))
  }
  # Check if selected sex is part of the available options set
  if (!all(v_sex %in% unique(df_migration$sex))) {
    stop(stop("v_sex must be a character element or vector containing at least one of the next names: ",
              paste(unique(df_mortrate_state_age_sex$sex), collapse = ", ")))
  }
  # Check selected year
  if (!all(v_year %in% seq(1970, 2050))) {
    stop("v_year must be a numeric value or vector with values between 1970 and 2050")
  }
  # Check selected age(s)
  if (!all(v_age %in% seq(0,89))) {
    stop("v_age must be a numeric value or vector with values between 0 and 109")
  }
  # Check type of migration
  if (!all(v_type %in% c("Interstate", "International", "Total"))) {
    stop("v_age must be a numeric value or vector with values between 0 and 109")
  }


  # Whether to group data by age --------------------------------------------
  if (age_groups) {
    df_outcome <- df_migration_expanded %>%
      mutate(age_group = cut(x = age, breaks = c(v_age, Inf),
                             include.lowest = TRUE)) %>%
      filter(year  %in% v_year,
             state %in% v_states,
             age   %in% v_age,
             sex   %in% v_sex,
             type  %in% v_type,
             complete.cases(.)) %>%
      # group_by(year, state, CVE_GEO, sex, age_group) %>%
      group_by(year, state, CVE_GEO, sex, age_group, type) %>%
      summarise(emigrants  = sum(emigrants),
                immigrants = sum(immigrants),
                population = sum(population))
  } else {
    df_outcome <- df_migration_expanded %>%
      filter(year  %in% v_year,
             state %in% v_states,
             age   %in% v_age,
             sex   %in% v_sex,
             type  %in% v_type) %>%
      select(year, state, CVE_GEO, sex, age, emigrants,
             immigrants, type, population)
  }
  return(df_outcome)
}
