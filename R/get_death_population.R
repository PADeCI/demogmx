##************************************************************************
## Script Name: get_death_population
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


#' Load population and death data, disaggregated by year, state, sex and age.
#'
#' \code{get_death_population} function that allows the user to get a
#' demographic dataset of the population and deaths based on the given
#' parameters.
#'
#' @param v_state Character vector specifying the state(s) that the function
#' will return.
#' @param v_year Numeric vector that specifies the year(s) to return. Must have
#' numbers between 1970 and 2050. Default is 2021.
#' @param v_sex Character vector that selects sex. Options: Female, Male and
#' Total.
#' @param v_age Numeric vector that specifies the age(s) to return.
#' @param age_groups Specifies whether to aggregate the output by age groups.
#' @import dplyr
#' @return A demographic dataset based on the selected parameters.
#'
#' @examples
#' get_death_population(v_state = c("Aguascalientes", "Campeche"),
#' v_year = c(2021, 2046), v_sex = c("Female", "Total"),  age_groups = TRUE)
#'
#' get_death_population(v_state = "National",  v_year = c(2021, 2046, 2050),
#' v_sex = "Total", age_groups= FALSE)
#'
#' @export
get_death_population <- function( v_state = "National",
                                  v_year   = "2021",
                                  v_sex    = "Total",
                                  v_age    = c(0,5,15,25,45, 55,65,70),
                                  age_groups = TRUE) {

  # Sanity Checks -----------------------------------------------------------
  # Check if selected state(s) is(are) part of the available options set
  if (!all(v_state %in% unique(df_mortrate_state_age_sex$state))) {
    stop("v_state must be a character element or vector containing at least one of the next names:\n\n",
         paste(unique(df_mortrate_state_age_sex$state), collapse = ", "))
  }
  # Check if selected sex is part of the available options set
  if (!all(v_sex %in% unique(df_mortrate_state_age_sex$sex))) {
    stop(stop("v_sex must be a character element or vector containing at least one of the next names: 'Female', 'Male', 'Total'"))
  }
  # Check selected year
  if (!all(v_year %in% seq(1970, 2050))) {
    stop("v_year must be a integer value or vector with values between 1970 and 2050")
  }
  # Check selected age(s)
  if (!all(v_age %in% seq(0, 109))) {
    stop("v_age must be a integer value or vector with values between 0 and 109")
  }
  # Check age groups option
  if (!is.logical(age_groups)) {
    stop("age_groups must be a logical value (TRUE / FALSE)")
  }

  # Filter data based on parameters -----------------------------------------
  df_mort_outcome <- df_mortrate_state_age_sex %>%
    filter(state %in% v_state,
           year %in% v_year,
           sex %in% v_sex) %>%
    ungroup()

  if (age_groups) {
    df_outcome <- df_mort_outcome %>%
      mutate(age_group = cut(x = age, breaks = c(v_age, Inf),
                             include.lowest = TRUE)) %>%
      dplyr::filter(complete.cases(.)) %>%
      # group_by(year, state, CVE_GEO, sex, age_group) %>%
      group_by(year, state, CVE_GEO, sex, age_group) %>%
      summarise(population = sum(population),
                deaths     = sum(deaths)) %>%
      ungroup()

  } else if (age_groups == FALSE) {
    df_outcome <- df_mort_outcome %>%
      filter(age %in% v_age) %>%
      relocate(CVE_GEO, .after = state)
  }

  return(df_outcome)
}
