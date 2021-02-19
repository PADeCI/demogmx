##************************************************************************
## Script Name: Get Death Rate
## Purpose:
##
##
## Created:
## Authors: Mariana Fernandez
##
## GitHub: marianafdz465
##
##
##************************************************************************
##*
load("data/df_mortrate_state_age_sex.Rdata")

#'Get death rate
#'
#'\code{get_death_rate} Get function that allows
#'user to get a dataset with the subset of different params
#'@param v_state State(s) of desired data
#'@param v_year Year(s) of desired data
#'@param select_sex Sex (Female/Male/Total)
#'@param v_init_age_grps Specifies the age bins to aggregate and return
#'@param age_grps (default is True)
#'
#'@return A dataset with the data selected
#'
#'@param export
#'
#'@examples
#'get_death_rate(v_state = "Aguascalientes", v_year = "2021", select_sex = "Total",
#'v_init_age_grps = c("0","5","15","25","45", "55","65","70"), age_grps= T)
#'get_death_rate(v_state = "Aguascalientes", v_year = c("2021", "2022"), select_sex = "Female",
#'v_init_age_grps = c("21", "22", "23"), age_grps= F)
get_death_rate <- function(v_state = "National",
                           v_year  = "2021",
                           select_sex    = unique(df_mortrate_state_age_sex$sex),
                           v_init_age_grps = c("0","5","15","25","45", "55","65","70"),
                           age_grps = T
                           ) {

  df_death_rate_aux <- get_death_population(v_state    = v_state,
                                            v_year     = v_year,
                                            select_sex = select_sex,
                                            v_init_age_grps = v_init_age_grps,
                                            age_grps = age_grps)

  df_death_rate <-  df_death_rate_aux %>% transform(death_rate = deaths / population)
  assign("df_death_rate", df_death_rate, envir = .GlobalEnv)

  return(df_death_rate)

}

