


#' v_b     <- vector of births                              +
#' m_d     <- matrix of aging rate by age and sex           -
#' m_mu    <- matrix of background mortality                +
#' m_eta   <- matrix emigration rate by age and sex         +
#' m_theta <- matrix of immigration rate, by age and sex    +
#'
#' init <- vector of population in age i and sex j
#'
#'
#' Functions of demog-mx
#'
### Non-homogeneous Demographic model
demog.mod.ns <- function(t, init, parameters, n.ages){
  derivs <- vector(length = n.ages)

  with(as.list(parameters),
       {
         # Initial age
         derivs[1] <- v_b[t] - (m_d[1, t] + m_mu[1, t])*init[1]

         # Rest of ages (does not include v_b)
         for(i in 2:n.ages) {
           derivs[i] <- (m_d[i - 1, t]) * init[i - 1] - (m_d[i, t] + m_mu[i, t])*init[i]
         }

         return(list(derivs))
       }
  )
}


## Non-homogeneous demographic model vectorized version
demog.mod.ns.vec <- function(t, init, parameters, n.ages){
  with(as.list(parameters),
       {
         Dg <- c(v_b[t], (m_d[, t]*init)[-n.ages])
         dD_dt <- Dg - (m_d[, t] + m_mu[, t])*init
         return(list(dD_dt))
       }
  )
}




#' In the original version `init` does not make distintion between males and
#' females, it is only a vector of population in age `i`
#'
#'
#' Sex can be added into a 3d array, being the 3rd dimmension the sex

library(demogmx)
library(reshape2)
library(tidyr)
library(tibble)
library(dplyr)

df_births <- get_births(v_state = "National", v_year = seq(1990, 2020),
                        year_groups = FALSE)
v_births <- df_births$births

df_pop <- get_population(v_state = "National", v_year = seq(1990, 2020),
                         v_sex = "Total", v_age = seq(0, 39), age_groups = F)

m_pop <- dcast(data = df_pop,
               formula = age ~ year,
               value.var = "population") %>%
  column_to_rownames(var = "age")


df_deaths <- get_deaths(v_state = "National", v_year = seq(1990, 2020),
                        v_sex = "Total", v_age = seq(0, 39), age_groups = F)

m_deaths <- dcast(data = df_deaths,
                  formula = age ~ year,
                  value.var = "death_rate") %>%
  column_to_rownames(var = "age")


df_mig <- get_migration(v_state = "National", v_year = seq(1990, 2020),
                        v_sex = "Total", v_age = seq(0, 39),
                        age_groups = F, v_type = "Total")

m_im <- dcast(data = df_mig,
              formula = age ~ year,
              value.var = "im_rate") %>%
  column_to_rownames(var = "age")


m_em <- dcast(data = df_mig,
              formula = age ~ year,
              value.var = "em_rate") %>%
  column_to_rownames(var = "age")


df_aging <- get_aging_rate(v_state = "National", v_year = seq(1990, 2020),
                           v_sex = "Total", v_age = seq(0, 39), age_groups = F)

m_aging <- dcast(data = df_aging,
                 formula = age ~ year,
                 value.var = "aging_rate") %>%
  column_to_rownames(var = "age")


#
# # Change age, given a single year
# derivs <- vector(length = 40)
# t <- 1
#
# # Initial age
# derivs[1] <- v_births[t] + (m_im[1, t] - m_em[1, t] - m_aging[1, t] - m_deaths[1, t])*m_pop[1, t]
#
# # Rest of ages (does not include v_b)
# for(i in 2:40) { #i <- 2
#   derivs[i] <- (m_aging[i - 1, t]*m_pop[i - 1, t]) + ((m_im[i, t] - m_em[i, t] - m_aging[i, t] + m_deaths[i, t])*m_pop[i, t])
# }
#
#
# plot(derivs, type = "l")



# Iterative version -------------------------------------------------------
# Change age and year
derivs <- matrix(nrow = 40, ncol = 31,
                 dimnames = list(c(0:39), seq(1990, 2020)))

for (t in seq(31)) {
  # Initial age
  derivs[1, t] <- v_births[t] + (m_im[1, t] - m_em[1, t] - m_aging[1, t] - m_deaths[1, t])*m_pop[1, t]

  # Rest of ages (does not include v_births)
  for(i in 2:40) { #i <- 2
    derivs[i, t] <- (m_aging[i - 1, t]*m_pop[i - 1, t]) + ((m_im[i, t] - m_em[i, t] - m_aging[i, t] + m_deaths[i, t])*m_pop[i, t])
  }
}
View(derivs)






t <- 1

# Vectorized version ------------------------------------------------------
# ## Increase
# Dg <- c(v_births[t], # First element of vector in time t
#         (m_aging[, t]*m_pop[, t])[-40]) # Remove the last element [-40]
#
# Dg <- c(v_births[t], # First element of vector in time t
#         ((m_aging[, t] + m_im[, t])*m_pop[, t])[-40]) # Remove the last element [-40]
#
# ## Decrease
# dD_dt <- Dg + (-m_em[, t] - m_aging[, t]  - m_deaths[, t])*m_pop[, t]



Dg <- c(v_births[t], # First element of vector in time t
        ((m_im[, t] - m_em[, t] - m_aging[, t] - m_deaths[, t])*m_pop[, t])[-40]) # Remove the last element [-40]
## Decrease
dD_dt <- Dg + (m_im[, t] - m_em[, t] - m_aging[, t] - m_deaths[, t])*m_pop[, t]
dD_dt


## Non-homogeneous demographic model vectorized version

demog.mod.ns.vec <- function(t, init, parameters, n.ages){
  with(as.list(parameters),
       {
         Dg <- c(v_b[t], (m_d[, t]*init)[-n.ages])
         dD_dt <- Dg - (m_d[, t] + m_mu[, t])*init
         return(list(dD_dt))
       }
  )
}


