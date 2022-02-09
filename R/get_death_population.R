##************************************************************************
## Script Name: get_death_population
## Purpose:
##
## Created: February, 2022
## Authors:
## - Mariana Fernandez
## - David Garibay-Treviño, MSc
##
## GitHub:
## - marianafdz465
## - du-gartre
##
##************************************************************************


#' Get death population
#'
#' \code{get_death_population} function that allows the user to
#' get a dataset of the population and deaths based on different parameters
#'
#' @param v_states State(s) of desired data.
#' @param v_year Year(s) of desired data. Must have numbers between 1970 and
#' 2050. Default is 2021.
#' @param v_sex Vector selecting sex. Options: Female, Male and Total.
#' @param v_age Specifies the age bins to aggregate and return.
#' @param age_groups Specifies whether to aggregate the output by age groups.
#' @import dplyr
#' @return A demographic dataset based on the selected parameters.
#' @param export
#'
#' @examples
#' get_death_population(v_states = c("Aguascalientes", "Campeche"),
#' v_year = c(2021, 2046), v_sex = c("Female", "Total"),  age_groups = TRUE)
#'
#' get_death_population(v_states = "National",  v_year = c(2021, 2046, 2050),
#' v_sex = "Total", age_groups= FALSE)
#'
#' @export
get_death_population <- function( v_states = "National",
                                  v_year   = "2021",
                                  v_sex    = "Total",
                                  v_age    = c("0","5","15","25","45", "55","65","70"),
                                  age_groups = TRUE) {
  # Import base data --------------------------------------------------------
  load("data/df_mortrate_state_age_sex.Rdata")

  # Sanity Checks -----------------------------------------------------------
  # Check if selected state(s) is(are) part of the available options set
  if (!all(v_states %in% unique(df_mortrate_state_age_sex$state))) {
    stop("v_states must be a character element or vector containing at least one of the next names: \n \n",
         paste(levels(df_mortrate_state_age_sex$state), collapse = ", "))
  }
  # Check if selected sex is part of the available options set
  if (!all(v_sex %in% unique(df_mortrate_state_age_sex$sex))) {
    stop(stop("v_sex must be a character element or vector containing at least one of the next names: ",
              paste(unique(df_mortrate_state_age_sex$sex), collapse = ", ")))
  }
  # Check selected year
  if (!all(v_year %in% seq(1970, 2050))) {
    stop("v_year must be a numeric value or vector with values between 1970 and 2050")
  }
  # Check selected age(s)
  if (!all(v_age %in% seq(0, 109))) {
    stop("v_age must be a numeric value or vector with values between 0 and 109")
  }

  # Filter data based on parameters -----------------------------------------
  df_mort_outcome <- df_mortrate_state_age_sex %>%
    filter(state %in% v_states,
           year %in% v_year,
           sex %in% v_sex) %>%
    ungroup()

  if (age_groups == FALSE) {
    df_outcome <- df_mort_outcome %>%
      filter(age %in% v_age)
  } else {
    df_outcome <- df_mort_outcome %>%
      mutate(age_group = cut(x = age, breaks = c(v_age, Inf),
                             include.lowest = TRUE)) %>%
      dplyr::filter(complete.cases(.)) %>%
      # group_by(year, state, CVE_GEO, sex, age_group) %>%
      group_by(year, state, sex, age_group) %>%
      summarise(population = sum(population),
                deaths     = sum(deaths))
  }

  return(df_outcome)
}
