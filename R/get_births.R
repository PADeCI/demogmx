##************************************************************************
## Script Name: get_births
##
## Created: February, 2022
## Authors:
## - David Garibay-Treviño, MSc
##
## GitHub:
## - du-gartre
##
##************************************************************************

#' Load births data, with projections from 1970 to 2050, without sex disaggregation.
#'
#' \code{get_births} is a function that allows the user to get a demographic
#' dataset of the number and rate of births based on the given parameters.
#'
#' @param v_state Character vector specifying the state(s) that the function
#' will return.
#' @param v_year Numeric vector that specifies the year(s) to return. Must have
#' numbers between 1970 and 2050. Default is 2021.
#' @param year_groups Logical. Specifies whether to aggregate the output by
#' year groups.
#' @import dplyr
#'
#' @return A demographic dataset containing the selected year
#' (group, when \code{year_groups = TRUE}), state, state code (CVE_GEO),
#' the number of births and the birth rate in each row,
#'
#' @examples
#' get_births(v_state = c("Aguascalientes", "Campeche"),
#'            v_year = c(1995, 2021, 2046),
#'            year_groups = TRUE)
#'
#' get_births(v_state = "National",
#'            v_year = c(1985, 2005, 2046),
#'            year_groups= FALSE)
#'
#' @export
get_births<- function(v_state     = "National",
                      v_year      = 2021,
                      year_groups = TRUE) {
  # Sanity Checks -----------------------------------------------------------
  # Check if selected state(s) is(are) part of the available options set
  if (!all(v_state %in% unique(df_birth_pop_states$state))) {
    stop("v_state must be a character element or vector containing at least one of the next names:\n\n",
         paste(unique(df_birth_pop_states$state), collapse = ", "))
  }
  # Check selected year
  if (!all(v_year %in% seq(1970, 2050))) {
    stop("v_year must be a integer value or vector with values between 1970 and 2050")
  }
  # Check year groups option
  if (!is.logical(year_groups)) {
    stop("year_groups must be a logical value (TRUE / FALSE)")
  }

  # Filter data based on parameters -----------------------------------------
  df_birth_aux <- df_birth_pop_states %>%
    filter(state %in% v_state) %>%
    mutate(birth_rate = births/population)

  if (year_groups) {
    df_outcome <- df_birth_aux %>%
      mutate(year_group = cut(x = year, breaks = c(v_year, Inf),
                              include.lowest = TRUE, dig.lab = 4)) %>%
      filter(complete.cases(.)) %>%
      group_by(state, CVE_GEO, year_group) %>%
      summarise(population = sum(population),
                births     = sum(births),
                birth_rate  = sum(births)/sum(population)) %>%
      ungroup() %>%
      select(year_group, state, CVE_GEO, births, birth_rate) %>%
      arrange(year_group, state, CVE_GEO)

  } else {
    df_outcome <- df_birth_aux %>%
      filter(year %in% v_year) %>%
      select(year, state, CVE_GEO, births, birth_rate) %>%
      arrange(state, CVE_GEO, year)
  }
  return(df_outcome)
}


#' Load births data, disaggregated by sex, from 1985 to 2020.
#'
#' \code{get_births_INEGI} is a function that allows the user to get a
#' demographic dataset of the number and rate of births based on the given
#' parameters. This function returns the recorded births by INEGI and allows to
#' specify the sex.
#'
#' @param v_state Character vector specifying the state(s) that the function
#' will return.
#' @param v_year Numeric vector that specifies the year(s) to return. Must have
#' numbers between 1985 and 2020. Default is 2021.
#' @param v_sex Vector selecting sex. Options: Female, Male and Total.
#' @param year_groups Logical. Specifies whether to aggregate the output by
#' year groups.
#' @import dplyr
#'
#' @return A demographic dataset containing the selected year
#' (group, when \code{year_groups = TRUE}), state, state code (CVE_GEO), sex,
#' the number of births the proportion of births (in relation to the total
#' births in that year), and the birth rate.
#'
#' @examples
#' get_births_INEGI(v_state = c("Aguascalientes", "Campeche"),
#'                  v_year = c(1995, 2005, 2015),
#'                  v_sex = "Male",
#'                  year_groups = TRUE)
#'
#' get_births_INEGI(v_state = "National",
#'                  v_year = c(1985, 2005, 2015),
#'                  v_sex = "Total",
#'                  year_groups = FALSE)
#'
#' @export
get_births_INEGI <- function(v_state     = "National",
                             v_year      = 2010,
                             v_sex       = c("Female", "Male", "Total"),
                             year_groups = TRUE) {
  # Sanity Checks -----------------------------------------------------------
  # Check if selected state(s) is(are) part of the available options set
  if (!all(v_state %in% unique(df_births_INEGI$state))) {
    stop("v_state must be a character element or vector containing at least one of the next names:\n\n",
         paste(unique(df_births_INEGI$state), collapse = ", "))
  }
  # Check selected year
  if (!all(v_year %in% seq(1985, 2020))) {
    stop("v_year must be a integer value or vector with values between 1985 and 2020")
  }
  # Check selected year
  if (!all(v_sex %in% unique(df_births_INEGI$sex))) {
    stop("v_sex must be a character element or vector containing at least one of the next names: 'Female', 'Male', 'Total'")
  }
  # Check year groups option
  if (!is.logical(year_groups)) {
    stop("year_groups must be a logical value (TRUE / FALSE)")
  }

  # Filter data based on parameters -----------------------------------------
  df_birth_aux <- df_births_INEGI %>%
    filter(state %in% v_state)

  if (year_groups) {
    df_outcome <- df_birth_aux %>%
      mutate(year_group = cut(x = year, breaks = c(v_year, Inf),
                              include.lowest = TRUE, dig.lab = 4)) %>%
      filter(complete.cases(.)) %>%
      group_by(state, CVE_GEO, year_group, sex) %>%
      summarise(population  = sum(population),
                births      = sum(births),
                birth_rate  = sum(births)/sum(population)) %>%
      ungroup() %>%
      # To obtain birth proportions by sex
      group_by(state, CVE_GEO, year_group) %>%
      mutate(birth_prop = births/births[sex == "Total"]) %>%
      ungroup() %>%
      filter(sex   %in% v_sex) %>%
      select(year_group, state, CVE_GEO, sex, births,
             birth_prop, birth_rate) %>%
      arrange(year_group, state, CVE_GEO, sex)

  } else {
    df_outcome <- df_birth_aux %>%
      filter(year %in% v_year,
             sex   %in% v_sex) %>%
      select(year, state, CVE_GEO, sex, births, birth_prop, birth_rate) %>%
      arrange(state, CVE_GEO, year, sex)
  }
  return(df_outcome)
}

