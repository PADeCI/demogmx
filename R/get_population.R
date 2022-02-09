##************************************************************************
## Script Name: get_population
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

#' Get population
#'
#' \code{get_population} Function that allows user to get a demographic dataset
#' regarding population and population proportions based on the parameter
#' set given by the user
#'
#' @param v_state State(s) of desired data.
#' @param v_year Year(s) of desired data. Must have numbers between 1970 and
#'  2050.
#' @param v_sex Vector selecting sex. Options: Female, Male and Total.
#' @param v_age Specifies the age bins to aggregate and return.
#' @param age_groups Specifies whether to aggregate the output by age groups.
#'
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
get_population<- function(v_states   = "National",
                          v_year     = "2021",
                          v_sex      = "Total",
                          v_age      = c("0","5","15","25","45", "55","65","70"),
                          age_groups = TRUE) {

  # Execute auxiliary function ----------------------------------------------
  df_pop_aux <- get_death_population(v_states   = v_states,
                                     v_year     = v_year,
                                     v_sex      = v_sex,
                                     v_age      = v_age,
                                     age_groups = age_groups)

  # Obtain death rate and remove population column --------------------------
  df_pop <-  df_pop_aux %>%
    group_by(year, state, sex) %>%
    mutate(proportion = prop.table(population)) %>%
    ungroup() %>%
    select(-deaths)

  return(df_pop)
}

