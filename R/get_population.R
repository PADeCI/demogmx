##************************************************************************
## Script Name: get_population
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
#' \code{get_population} is a function that allows the user to get a demographic
#' dataset regarding population and population proportions based on the
#' given parameters.
#'
#' @param v_state State(s) of desired data.
#' @param v_year Year(s) of desired data. Must have numbers between 1970 and
#'  2050.
#' @param v_sex Vector selecting sex. Options: Female, Male and Total.
#' @param v_age Specifies the age bins to aggregate and return.
#' @param age_groups Specifies whether to aggregate the output by age groups.
#'
#' @return A demographic dataset indicating the year, the state, the state code
#' (CVE_GEO), the age (group, when \code{age_groups = TRUE}), the population and
#' the proportion of each row, in relation to the selected year and sex.
#'
#' @examples
#' get_population(v_state =  "Chiapas", v_year = 2015, v_sex = "Total",
#' v_age = c(0, 15, 45, 75), age_groups = FALSE)
#'
#' get_population(v_state = c("Aguascalientes", "Campeche"),
#' v_year = c(2010, 2021), v_sex = "Male", v_age = c(0, 15, 45, 75),
#' age_groups = TRUE)
#'
#' @export
get_population<- function(v_state   = "National",
                          v_year     = "2021",
                          v_sex      = "Total",
                          v_age      = c("0","5","15","25","45", "55","65","70"),
                          age_groups = TRUE) {

  # Execute auxiliary function ----------------------------------------------
  df_pop_aux <- get_death_population(v_state   = v_state,
                                     v_year     = v_year,
                                     v_sex      = v_sex,
                                     v_age      = v_age,
                                     age_groups = age_groups)

  # Obtain death rate and remove population column --------------------------
  df_pop <-  df_pop_aux %>%
    group_by(year, state, sex) %>%
    mutate(proportion = prop.table(population)) %>%
    ungroup() %>%
    select(-deaths) %>%
    relocate(CVE_GEO, .before = sex)

  return(df_pop)
}

