# library(deSolve)
# library(reshape2)
# source("R/demographic models.R")
#
# #### Non-stationary, age-structured demographic model ####
# v_times <- 1:200
# n_times <- length(v_times)
# parm_ns <- parm
# ### Non-homogeneous birth rate
# parm_ns$v_b <- rep(parm_ns$b, n_times)*seq(1, 2, length.out = 200)
# names(parm_ns$v_b) <- v_times
# ### Non-homogeneous death rate
# parm_ns$m_mu <- matrix(rep(parm_ns$mu, 200)*seq(1, 1/2, length.out = 200),
#                        ncol = n_times,
#                        dimnames = list(parm_ns$age, v_times))
# ### Non-homogeneous aging rate
# parm_ns$m_d <- matrix(rep(parm_ns$d, 200), ncol = n_times,
#                       dimnames = list(parm_ns$age, v_times))
#
# #### Run models ####
# system.time(
#   test_ns <- lsoda(y = init, func = demog.mod.ns,
#                    times = 1:200, parms = parm_ns, n_ages = n_ages)
# )
# system.time(
#   test.vec_ns <- lsoda(y = init, func = demog.mod.ns.vec,
#                        times = 1:200, parms = parm_ns, n_ages = n_ages)
# )
# points(rowSums(test_ns[, -1]), col = "red")
# points(rowSums(test.vec_ns[, -1]), pch = 2, col = "red")
#
# #### Non-stationary, age-structured demographic model ####
# load(file = "data/Estatal/df_pop_state_age.Rdata")
# load(file = "data/Estatal/df_pop_state.Rdata")
# load(file = "data/Estatal/df_birth_state.Rdata")
# load(file = "data/Estatal/df_mortrate_state_age.Rdata")
# v_times <- 0:80
# n_times <- length(v_times)
# parm_ns <- list()#parm
# n_pop0 <- subset(df_pop_state, state == "National" & year == 1970)$population
# ### Non-homogeneous birth rate
# parm_ns$v_b <- subset(df_birth_state, CVE_GEO == 0)$births/n_pop0
#   #subset(df_birthrate_state, state == "National")$birth_rate
# names(parm_ns$v_b) <- v_times
# ### Non-homogeneous death rate
# parm_ns$m_mu <- matrix(subset(df_mortrate_state_age,
#                               state == "National" & age <= 100)$mort_rate,
#                        ncol = n_times,
#                        dimnames = list(0:100, v_times))
# ### Non-homogeneous aging rate
# parm_ns$m_d <- matrix(rep(1, 81*101), ncol = n_times,
#                       dimnames = list(parm_ns$age, v_times))
# v_init <- subset(df_pop_state_age,
#                  state == "National" & age <= 100 & year == 1970)$population/n_pop0
# names(v_init) <- 0:100
# n_ages <- 101
# #### Run models ####
# system.time(
#   test_ns <- lsoda(y = v_init, func = demog.mod.ns,
#                    times = 1:101, parms = parm_ns, n_ages = n_ages)
# )
# system.time(
#   test.vec_ns <- as.data.frame(lsoda(y = v_init, func = demog.mod.ns.vec,
#                        times = 1:101, parms = parm_ns, n_ages = n_ages))
# )
# test.vec_ns$time <- 1950:2050
# colnames(test.vec_ns)[1] <- "year"
# plot(rowSums(test_ns[, -1]), col = "red")
# points(rowSums(test.vec_ns[, -1]), pch = 2, col = "red")
#
# df_test.vec_ns <- melt(test.vec_ns, id.vars = "year",
#                        variable.name = "age", value.name = "population")
# df_test.vec_ns$year <- factor(df_test.vec_ns$year)
# df_test.vec_ns$age <- as.numeric(df_test.vec_ns$age)-1
#
# ggplot(subset(df_pop_state, state == "National"),
#        aes(x = year, y = population/n_pop0,
#            linetype = "Data",
#            col = "Data")) +
#   geom_line() +
#   geom_line(aes(x = 1950:2051, y = c(1, rowSums(test_ns[, -1])),
#                 linetype = "Model",
#                 col = "Model")) +
#   scale_color_manual("Source", labels = c("Data", "Model"), values = c("black", "red")) +
#   scale_linetype_manual("Source", labels = c("Data", "Model"), values = c(1, 2)) +
#   ylab("Relative population over time")
#
# ggplot(subset(df_pop_state_age, state == "National" & year %in% c(1950, 2000, 2020)),
#        aes(x = age, y = population/n_pop0, color = as.factor(year))) +
#   geom_line() +
#   geom_line(data = subset(df_test.vec_ns, year %in% c(1950, 2000, 2020)),
#             aes(x = age, y = population), lty = 2)



library(deSolve)
library(reshape2)
library(ggplot2)
source("R/demographic models.R")

#### Non-stationary, age-structured demographic model ####
v_times <- 1:200
n_times <- length(v_times)
parm_ns <- parm # Check line 40
#
# ### Non-homogeneous birth rate
# parm_ns$v_b <- rep(parm_ns$b, n_times)*seq(1, 2, length.out = 200)
# names(parm_ns$v_b) <- v_times
# ### Non-homogeneous death rate
# parm_ns$m_mu <- matrix(rep(parm_ns$mu, 200)*seq(1, 1/2, length.out = 200),
#                        ncol = n_times,
#                        dimnames = list(parm_ns$age, v_times))
# ### Non-homogeneous aging rate
# parm_ns$m_d <- matrix(rep(parm_ns$d, 200), ncol = n_times,
#                       dimnames = list(parm_ns$age, v_times))
#
# #### Run models ####
# system.time(
#   test_ns <- lsoda(y = init, func = demog.mod.ns,
#                    times = 1:200, parms = parm_ns, n_ages = n_ages)
# )
# system.time(
#   test.vec_ns <- lsoda(y = init, func = demog.mod.ns.vec,
#                        times = 1:200, parms = parm_ns, n_ages = n_ages)
# )
# points(rowSums(test_ns[, -1]), col = "red")
# points(rowSums(test.vec_ns[, -1]), pch = 2, col = "red")

#### Non-stationary, age-structured demographic model ####
load(file = "data/Estatal/df_pop_state_age.Rdata")
load(file = "data/Estatal/df_pop_state.Rdata")
load(file = "data/Estatal/df_birth_state.Rdata")
load(file = "data/Estatal/df_mortrate_state_age.Rdata")
v_times <- 0:80
n_times <- length(v_times)
parm_ns <- list()#parm

n_pop0 <- subset(df_pop_state, state == "National" & year == 1970)$population

table(df_pop_state$year)

### Non-homogeneous birth rate
parm_ns$v_b <- subset(df_birth_state, CVE_GEO == 0)$births/n_pop0

#subset(df_birthrate_state, state == "National")$birth_rate
names(parm_ns$v_b) <- v_times

### Non-homogeneous death rate
parm_ns$m_mu <- matrix(subset(df_mortrate_state_age,
                              state == "National" & age <= 100)$mort_rate,
                       ncol = n_times,
                       dimnames = list(0:100, v_times)) # rows - ages, cols - years

### Non-homogeneous aging rate
parm_ns$m_d <- matrix(rep(1, 81*101), ncol = n_times,
                      dimnames = list(0:100, v_times))

v_init <- subset(df_pop_state_age,
                 state == "National" & age <= 100 & year == 1970)$population/n_pop0

names(v_init) <- 0:100

n_ages <- 101
#### Run models ####
# system.time(
#   test_ns <- lsoda(y = v_init, func = demog.mod.ns,
#                    times = 1:101, parms = parm_ns, n_ages = n_ages)
# )
# system.time(
#   test.vec_ns <- as.data.frame(lsoda(y = v_init, func = demog.mod.ns.vec,
#                                      times = 1:101, parms = parm_ns, n_ages = n_ages))
# )

# Original version
system.time(
  test_ns <- lsoda(y = v_init, func = demog.mod.ns,
                   times = 1:81, parms = parm_ns, n.ages = n_ages)
)
# Vectorized version
system.time(
  test.vec_ns <- as.data.frame(lsoda(y = v_init, func = demog.mod.ns.vec,
                                     times = 1:81, parms = parm_ns, n.ages = n_ages))
)


test.vec_ns$time <- 1970:2050
colnames(test.vec_ns)[1] <- "year"
# Plot original version
plot(rowSums(test_ns[, -1]), col = "red")
# Compare with vectorized version
points(rowSums(test.vec_ns[, -1]), pch = 2, col = "red")
# Create dataframe from test.vec_ns
df_test.vec_ns <- melt(test.vec_ns, id.vars = "year",
                       variable.name = "age", value.name = "population")

df_test.vec_ns$year <- factor(df_test.vec_ns$year)
df_test.vec_ns$age <- as.numeric(df_test.vec_ns$age)-1

# Graphs ------------------------------------------------------------------
## Compare model results vs official Data
ggplot(subset(df_pop_state, state == "National"),
       aes(x = year, y = population/n_pop0,
           linetype = "Data",
           col = "Data")) +
  geom_line() +
  geom_line(aes(x = 1970:2051, y = c(1, rowSums(test_ns[, -1])),
                linetype = "Model",
                col = "Model")) +
  scale_color_manual("Source", labels = c("Data", "Model"), values = c("black", "red")) +
  scale_linetype_manual("Source", labels = c("Data", "Model"), values = c(1, 2)) +
  ylab("Relative population over time") +
  theme(legend.position = "bottom")

ggplot(subset(df_pop_state_age, state == "National" & year %in% c(1970, 2000, 2020)),
       aes(x = age, y = population/n_pop0, color = as.factor(year), linetype = "Data")) +
  geom_line() +
  geom_line(data = subset(df_test.vec_ns, year %in% c(1970, 2000, 2020)),
            aes(x = age, y = population, linetype = "Model")) +
  theme(legend.position = "bottom") +
  scale_linetype_manual(name = "Source",
                        values = c(1, 2),
                        labels = c("Data", "Model"))

