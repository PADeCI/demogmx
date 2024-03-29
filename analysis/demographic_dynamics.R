


# Original functions ------------------------------------------------------
# Source: package demog-mx

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
#'

## Iterated version -------------------------------------------------------
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



## Vectorized version -----------------------------------------------------
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




# Data --------------------------------------------------------------------
#' In the original version `init` does not make distintion between males and
#' females, it is only a vector of population in age `i`
#'
#'

library(demogmx)
library(reshape2)
library(tidyr)
library(tibble)
library(dplyr)
library(deSolve)
library(ggplot2)

v_times <- 0:35
n_times <- length(v_times)
parm_ns <- list()
n_pop_init <- get_population(v_state = "National", v_year = 1985,
                             v_sex = "Total", v_age = c(0, 109),
                             age_groups = T)$population

## Population data --------------------------------------------------------
df_pop <- get_population(v_state = "National", v_year = seq(1985, 2020),
                         v_sex = "Total", v_age = seq(0, 109), age_groups = F)

v_pop_init <- get_population(v_state = "National", v_year = 1985,
                         v_sex = "Total", v_age = seq(0, 89),
                         age_groups = F)$population/n_pop_init

# m_pop <- reshape2::dcast(data = df_pop,
#                formula = age ~ year,
#                value.var = "population") %>%
#   tibble::column_to_rownames(var = "age")

## Birth data --------------------------------------------------------------
df_births <- get_births(v_state = "National", v_year = seq(1985, 2020),
                        year_groups = FALSE)

# Vector of birth rates (compared to initial time population)
v_r_births <- df_births$births/n_pop_init
# # Vector of birth rates (compared to each year)
# v_r_births <- df_births$birth_rate

# Store in parm list
parm_ns$v_b <- v_r_births
names(parm_ns$v_b) <- v_times

## Death data -------------------------------------------------------------
df_deaths <- get_deaths(v_state = "National", v_year = seq(1985, 2020),
                        v_sex = "Total", v_age = seq(0, 89), age_groups = FALSE)

m_deaths <- reshape2::dcast(data = df_deaths,
                  formula = age ~ year,
                  value.var = "death_rate") %>%
  tibble::column_to_rownames(var = "age") %>%
  as.matrix()

colnames(m_deaths) <- v_times

# Store in parm list
parm_ns$m_mu <- m_deaths


# df_deaths <- get_deaths(v_state = "National", v_year = seq(1985, 2020),
#                         v_sex = "Total", v_age = seq(0, 109), age_groups = FALSE)
#
# # Vector of death rates (compared to initial time population)
# v_r_deaths <- df_deaths$deaths/n_pop_init

## Migration data ----------------------------------------------------------
df_migration <- get_migration(v_state = "National", v_year = seq(1985, 2020),
                        v_sex = "Total", v_age = seq(0, 89),
                        age_groups = F, v_type = "Total")

m_immigration <- reshape2::dcast(data = df_migration,
              formula = age ~ year,
              value.var = "im_rate") %>%
  tibble::column_to_rownames(var = "age") %>%
  as.matrix()

m_emigration <- reshape2::dcast(data = df_migration,
              formula = age ~ year,
              value.var = "em_rate") %>%
  tibble::column_to_rownames(var = "age") %>%
  as.matrix()

colnames(m_immigration) <- v_times
colnames(m_emigration)  <- v_times

# Store in parm list
parm_ns$m_im <- m_immigration
parm_ns$m_em <- m_emigration

# df_migration <- get_migration(v_state = "National", v_year = seq(1985, 2020),
#                         v_sex = "Total", v_age = c(0, 89),
#                         age_groups = T, v_type = "Total")
#
# # Vectors of emigration immigration and rates  (compared to initial time
# # population)
# v_r_immigration <- df_migration$immigrants/n_pop_init
# v_r_emigration <- df_migration$emigrants/n_pop_init

## Aging data -------------------------------------------------------------
df_aging <- get_aging_rate(v_state = "National", v_year = seq(1985, 2020),
                           v_sex = "Total", v_age = seq(0, 89), age_groups = F)

m_r_aging <- reshape2::dcast(data = df_aging,
                 formula = age ~ year,
                 value.var = "aging_rate") %>%
  tibble::column_to_rownames(var = "age") %>%
  as.matrix()

colnames(m_r_aging) <- v_times

# Store in parm list
parm_ns$m_d <- m_r_aging

# df_aging <- get_aging_rate(v_state = "National", v_year = seq(1985, 2020),
#                            v_sex = "Total", v_age = c(0, 89), age_groups = T)
#
# # Vector of aging rates (compared to initial time population)
# v_r_aging <- df_aging$aging_pop/n_pop_init

# New functions -----------------------------------------------------------
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


## Iterative version ------------------------------------------------------

demog.mod.ns <- function(t, init, parameters, n.ages){
  derivs <- vector(length = n.ages)

  with(as.list(parameters),
       {
         derivs[1] <- v_b[t] - (m_em[1, t] - m_im[1, t] + m_d[1, t] + m_mu[1, t])*init[1]
         for(i in 2:n.ages) {
           derivs[i] <- (m_d[i - 1, t]) * init[i - 1] - (m_em[i, t] - m_im[i, t] + m_d[i, t] + m_mu[i, t])*init[i]
         }
         return(list(derivs))
       }
  )
}


# Dg Demographic growth
## Vectorized version -----------------------------------------------------
# ## Increase
# v_D_growth <- c(v_births[t], # First element of vector in time t
#         (m_aging[, t]*m_pop[, t])[-40]) # Remove the last element [-40]
#
# Dg <- c(v_births[t], # First element of vector in time t
#         ((m_aging[, t] + m_im[, t])*m_pop[, t])[-40]) # Remove the last element [-40]
#
# ## Decrease
# v_dD_dt <- Dg + (-m_em[, t] - m_aging[, t]  - m_deaths[, t])*m_pop[, t]
#
#
#
# Dg <- c(v_births[t], # First element of vector in time t
#         ((m_im[, t] - m_em[, t] - m_aging[, t] - m_deaths[, t])*m_pop[, t])[-40]) # Remove the last element [-40]
# ## Decrease
# dD_dt <- Dg + (m_im[, t] - m_em[, t] - m_aging[, t] - m_deaths[, t])*m_pop[, t]
# dD_dt
#
#
# ## Non-homogeneous demographic model vectorized version
# demog.mod.ns.vec <- function(t, v_pop, l_parameters){
#   with(as.list(l_parameters),
#        {
#          v_D_growth <- c(v_births[t], (m_r_aging[, t]*v_pop)[-n_ages])
#          v_dD_dt <- v_D_growth - (m_r_aging[, t] + m_r_deaths[, t])*init
#
#          return(list(v_dD_dt))
#        }
#   )
# }
#
# ## Non-homogeneous demographic model vectorized version
# demog.mod.ns.vec <- function(t, v_pop, l_parameters){
#   with(as.list(l_parameters),
#        {
#          v_D_growth <- c(v_births[t], (m_r_aging[, t]*v_pop)[-n_ages])
#
#          v_dD_dt <- v_D_growth - (m_r_aging[, t] + m_r_deaths[, t])*init
#
#          return(list(v_dD_dt))
#        }
#   )
# }

demog.mod.ns.vec <- function(t, init, parameters, n.ages){
  with(as.list(parameters),
       {
         # Vector of demographic growth
         v_D_growth <- c(v_b[t], ((m_im[, t] + m_d[, t])*init)[-n.ages])
         # Vector of demographic changes
         v_dD_dt <- v_D_growth - (m_em[, t] + m_d[, t] + m_mu[, t])*init

         return(list(v_dD_dt))
       }
  )
}


# Execute functions -------------------------------------------------------
n_ages <- length(v_pop_init)

# Original version
system.time(
  test_ns <- lsoda(y = v_pop_init, func = demog.mod.ns,
                   times = 1:35, parms = parm_ns, n.ages = n_ages)
)

# Vectorized version
system.time(
  test_vec_ns <- lsoda(y = v_pop_init, func = demog.mod.ns.vec,
                       times = 1:35, parms = parm_ns, n.ages = n_ages)
)

# Graphs ------------------------------------------------------------------
# Official population data
df_pop_state <- get_population(v_state = "National", v_year = seq(1985, 2020),
                               v_sex = "Total", v_age = c(0, 109), age_groups = T)

# General plot data
names(v_pop_init) <- 0:89
n_ages <- length(v_pop_init)
test_ns <- as.data.frame(test_ns)
test_vec_ns <- as.data.frame(test_vec_ns)

colnames(test_ns)[1] <- "year"
colnames(test_vec_ns)[1] <- "year"


# Plot original version
plot(rowSums(test_ns[, -1]), col = "red")
# Compare with vectorized version
points(rowSums(test_vec_ns[, -1]), pch = 20, col = "black")
# Create dataframe from test.vec_ns
df_test_vec_ns <- melt(test_vec_ns, id.vars = "year",
                       variable.name = "age", value.name = "population")


## Compare model with official data ---------------------------------------
# Original model
ggplot(subset(df_pop_state),
       aes(x = year, y = population/n_pop_init,
           linetype = "Data", col = "Data")) +
  geom_line() +
  geom_line(aes(x = 1985:2020, y = c(1, rowSums(test_ns[, -1])),
                linetype = "Model",
                col = "Model")) +
  scale_color_manual("Source", labels = c("Data", "Model"), values = c("black", "red")) +
  scale_linetype_manual("Source", labels = c("Data", "Model"), values = c(1, 2)) +
  ylab("Relative population over time") +
  theme(legend.position = "bottom")

# Vectorized model
ggplot(subset(df_pop_state),
       aes(x = year, y = population/n_pop_init,
           linetype = "Data", col = "Data")) +
  geom_line() +
  geom_line(aes(x = 1985:2020, y = c(1, rowSums(test_vec_ns[, -1])),
                linetype = "Model",
                col = "Model")) +
  scale_color_manual("Source", labels = c("Data", "Model"), values = c("black", "red")) +
  scale_linetype_manual("Source", labels = c("Data", "Model"), values = c(1, 2)) +
  ylab("Relative population over time") +
  theme(legend.position = "bottom")

