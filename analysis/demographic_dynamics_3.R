


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
#' In the original version `init` does not make distintion between males and
#' females, it is only a vector of population in age `i`

## Iterated version -------------------------------------------------------
### Non-homogeneous Demographic model
demog.mod.ns <- function(t, init, parameters, n.ages){
  derivs <- vector(length = n.ages)

  with(as.list(parameters),
       {
         # Initial age
         derivs[1] <- v_r_births[t] - (m_r_aging[1, t] + m_r_mort[1, t])*init[1]

         # Rest of ages (does not include v_r_births)
         for(i in 2:n.ages) {
           derivs[i] <- (m_r_aging[i - 1, t]) * init[i - 1] - (m_r_aging[i, t] + m_r_mort[i, t])*init[i]
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
         Dg <- c(v_r_births[t], (m_r_aging[, t]*init)[-n.ages])
         dD_dt <- Dg - (m_r_aging[, t] + m_r_mort[, t])*init
         return(list(dD_dt))
       }
  )
}

# New data ----------------------------------------------------------------
## Libraries --------------------------------------------------------------
library(ggplot2)
library(demogmx)
library(deSolve)
library(dplyr)
library(reshape2)
library(tidyr)
library(tibble)


## General parameters -----------------------------------------------------
v_times <- 0:35
n_times <- length(v_times)
parm_ns <- list()
# Total population of the country in 1985
n_pop_init <- get_population(v_state = "National", v_year = 1985,
                             v_sex = "Total", v_age = c(0, 109),
                             age_groups = T)$population

## Population data --------------------------------------------------------
df_pop <- get_population(v_state = "National", v_year = seq(1985, 2020),
                         v_sex = "Total", v_age = seq(0, 109), age_groups = F)

# Age proportion in 1985 (initial year)
v_pop_init <- get_population(v_state = "National", v_year = 1985,
                             v_sex = "Total", v_age = seq(0, 89),
                             age_groups = F)$population/n_pop_init

## Birth data --------------------------------------------------------------
df_births <- get_births(v_state = "National", v_year = seq(1985, 2020),
                        year_groups = FALSE)

# Vector of birth rates (compared to initial time population)
v_r_births <- df_births$births/n_pop_init

# # Vector of birth rates (compared to each year*********************** Question
# ********************************************************************* Question
# v_r_births_density <- df_births$birth_rate

# Store in parm list
parm_ns$v_r_births <- v_r_births
names(parm_ns$v_r_births) <- v_times

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
parm_ns$m_r_mort <- m_deaths

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
parm_ns$m_r_immigration <- m_immigration
parm_ns$m_r_emigration <- m_emigration

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
parm_ns$m_r_aging <- m_r_aging

# New functions -----------------------------------------------------------
## Iterated version ------------------------------------------------------
demog.mod.ns <- function(t, init, parameters, n.ages){
  derivs <- vector(length = n.ages)

  with(as.list(parameters),
       {
         derivs[1] <- v_r_births[t] - (m_r_emigration[1, t] - m_r_immigration[1, t] + m_r_aging[1, t] + m_r_mort[1, t])*init[1]
         for(i in 2:n.ages) {
           derivs[i] <- (m_r_aging[i-1, t]) * init[i-1] - (m_r_emigration[i, t] - m_r_immigration[i, t] + m_r_aging[i, t] + m_r_mort[i, t])*init[i]
         }
         return(list(derivs))
       }
  )
}

## Vectorized version -----------------------------------------------------
demog.mod.ns.vec <- function(t, init, parameters, n.ages){
  with(as.list(parameters),
       {
         # Vector of demographic growth
         v_D_growth <- c(v_r_births[t], ((m_r_immigration[, t] + m_r_aging[, t])*init)[-n.ages])
         # Vector of demographic variation
         v_dD_dt <- v_D_growth - (m_r_emigration[, t] + m_r_aging[, t] + m_r_mort[, t])*init

         return(list(v_dD_dt))
       }
  )
}


# Execute functions -------------------------------------------------------
n_ages <- length(v_pop_init)

# Original version
system.time(
  test_ns <- lsoda(y = v_pop_init, func = demog.mod.ns,
                   times = 1:36, parms = parm_ns, n.ages = n_ages)
)

# Vectorized version
# system.time(
#   test_vec_ns <- lsoda(y = v_pop_init, func = demog.mod.ns.vec,
#                        times = 1:35, parms = parm_ns, n.ages = n_ages)
# )

system.time(
  test_vec_ns <- lsoda(y = v_pop_init, func = demog.mod.ns.vec,
                       times = 1:36, parms = parm_ns, n.ages = n_ages)
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

## Model comparation ------------------------------------------------------
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
  geom_line(aes(x = 1985:2020,
                y = c(rowSums(test_ns[, -1])),
                linetype = "Model",
                col = "Model")) +
  scale_color_manual(name = "Source", labels = c("Data", "Model"), values = c("black", "red")) +
  scale_linetype_manual(name = "Source", labels = c("Data", "Model"), values = c(1, 2)) +
  scale_x_continuous(breaks = seq(from = 1985, to = 2020, by = 5),
                     minor_breaks = seq(from = 1985, to = 2020, by = 1)) +
  ylab("Relative population over time") +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_line(size = 1))

# Vectorized model
ggplot(subset(df_pop_state),
       aes(x = year, y = population/n_pop_init,
           linetype = "Data", col = "Data")) +
  geom_line() +
  geom_line(aes(x = 1985:2020,
                # y = c(1, rowSums(test_vec_ns[, -1])),
                y = c(rowSums(test_vec_ns[,-1])),
                linetype = "Model",
                col = "Model")) +
  scale_color_manual(name = "Source", labels = c("Data", "Model"), values = c("black", "red")) +
  scale_linetype_manual(name = "Source", labels = c("Data", "Model"), values = c(1, 2)) +
  scale_x_continuous(breaks = seq(from = 1985, to = 2020, by = 5),
                     minor_breaks = seq(from = 1985, to = 2020, by = 1)) +
  ylab("Relative population over time") +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_line(size = 1))
# # There is a kind of offset in the model data #*****************************
# coord_cartesian(xlim = c(1985, 1990),
#                 ylim = c(1, 1.25))

dim(subset(df_pop_state, year < 2020))
dim(test_vec_ns)
dim(test_vec_ns[,-1])
test_vec_ns$year = c(1984:2018)
