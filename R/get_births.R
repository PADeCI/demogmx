##************************************************************************
## Script Name: get_births
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

#' Get births
#'
#' \code{get_births} is a function that allows the user to
#' get a dataset of the number and rate of births based on different parameters.
#'
#' @param v_states State(s) of desired data.
#' @param v_year Year(s) of desired data. Must have numbers between 1970 and
#' 2050. Default is 2021.
#' @param year_groups Specifies whether to aggregate the output by year groups.
#' @import dplyr
#' @return A demographic dataset based on the selected parameters.
#' @param export
#'
#' @examples
#' get_births(v_states = c("Aguascalientes", "Campeche"),
#' v_year = c(1995, 2021, 2046), year_groups = TRUE)
#'
#' get_births(v_states = "National",  v_year = c(1985, 2005, 2046),
#' age_groups= FALSE)
#'
#' @export
get_births<- function(v_states   = "National",
                      v_year     = "2021",
                      year_groups = TRUE) {
  # Import base data --------------------------------------------------------
  # Birth data
  load("data/df_birth_pop_states.Rdata")

  # Sanity Checks -----------------------------------------------------------
  # Check if selected state(s) is(are) part of the available options set
  if (!all(v_states %in% unique(df_birth_pop_states$state))) {
    stop("v_states must be a character element or vector containing at least one of the next names: \n \n",
         paste(unique(df_mortrate_state_age_sex$state), collapse = ", "))
  }
  # Check selected year
  if (!all(v_year %in% seq(1970, 2050))) {
    stop("v_year must be a numeric value or vector with values between 1970 and 2050")
  }

  # Filter data based on parameters -----------------------------------------
  df_birth_aux <- df_birth_pop_states %>%
    filter(state %in% v_states) %>%
    mutate(birthrate = births/population)

  if (year_groups == FALSE) {
    df_outcome <- df_birth_aux %>%
      filter(year %in% v_year)
  } else {
    df_outcome <- df_birth_aux %>%
      mutate(year_group = cut(x = year, breaks = c(v_year, Inf),
                              include.lowest = TRUE, dig.lab = 4)) %>%
      filter(complete.cases(.)) %>%
      group_by(state, year_group) %>%
      summarise(population = sum(population),
                births     = sum(births),
                birthrate  = sum(births)/sum(population))
  }
  return(df_outcome)
}

