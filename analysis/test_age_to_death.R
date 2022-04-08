library(data.table)
library(dplyr)
library(demogmx)

n_pop <- 100000
# df_example <- data.frame(year = sample(x = c(1990:2010), size = n_pop, replace = TRUE),
#                          sex  = sample(x = c("Female", "Male"), size = n_pop, replace = TRUE),
#                          age  = sample(x = c(0:109), size = n_pop, replace = TRUE))
df_example <- data.frame(year = rep(2010, n_pop),
                         sex  = rep("Male", n_pop),
                         age  = rep(0, n_pop))

df_mortrate <- df_mortrate_state_age_sex %>%
  mutate(death_rate = deaths/population) %>%
  group_by(year, state, sex) %>%
  mutate(id = cur_group_id()) %>%
  mutate(death_prob_cum = 1 - exp(-cumsum(death_rate))) %>%
  mutate(death_prob = c(death_prob_cum[1], diff(death_prob_cum))) %>%
  arrange(year, state, sex) # %>% ungroup()

# Convert to data.table
dt_mortrate <- as.data.table(x = df_mortrate)
# Set keys
setkey(x = dt_mortrate, year, state, sex, age)


get_col_dt <- function(Year, State, Sex, Age) {
  # IMPORTANT:
  # The name of the fun parameters do not have to match the colnames of
  # dt_mortrate, otherwise the subsetting will not work

  v_prob_1 <- rep(x = 0, times = Age)
  v_prob_2 <- dt_mortrate[.(Year, State, Sex, c(Age:109)), death_prob]

  v_prob <- c(v_prob_1, v_prob_2)

  return(v_prob)
}

# # Test fun
# get_col_dt(Year = 2006, State = "National", Sex = "Male", Age = 8)

# Use the fun in a vectorized way
aa <- Sys.time()
hh <- mapply(FUN = get_col_dt,
             Year = df_example$year,
             State = rep("National", times = n_pop),
             Sex = df_example$sex,
             Age = df_example$age)
thh <- t(hh)
bb <- Sys.time()
bb-aa


thh_norm <- thh / rowSums(thh)
rowSums(thh_norm)
test <- darthtools::samplev(thh_norm)

mean(test)


setkey(x = dt_mortrate, NULL)
# create subset table
sub <- data.table(start = Age, end = 109)
# use data.table non equijoin
dt1 <- dt_mortrate[sub, on = .(age >= start, age <= end)]
head(dt1)


setkey(x = dt_mortrate, year, state, sex, age)
dt_mortrate[.(Year, State, Sex, Age), death_prob, on = .(age >=Age, age <= 109)]

n_row <- 10000
n_col <- 100
v_hazard <- rep(0.2, n_col)
v_Ft <- 1- exp(-cumsum(v_hazard))
v_ft <- c(v_Ft[1], diff(v_Ft))

m_probs_const <- matrix(v_ft, nrow = n_row, ncol = n_col, byrow = TRUE)
m_probs_const_norm <- m_probs_const / rowSums(m_probs_const)
set.seed(20220408)
system.time(
  v_expected_t <- darthtools::samplev(m_probs_const_norm)
)
mean(v_expected_t)



#* https://stackoverflow.com/questions/45019256/how-to-extract-blocks-of-rows-without-looping-in-r
library(data.table)
dt <- data.table(value=runif(10000, 0, 10^3))
# add index column
dt[, idx := seq_len(.N)]
# create subset table
sub <- data.table(start=c(20,50,130,2000),end=c(25,60,150,2030))
# use data.table non equijoin
dt1 <- dt[sub, on = .(idx >= start, idx <= end)]
head(dt1)
#>        value idx idx.1
#> 1: 820.38637  20    25
#> 2: 262.51398  20    25
#> 3: 900.37408  20    25
#> 4:  74.91815  20    25
#> 5: 507.87825  20    25
#> 6: 547.45235  20    25
# use data.table non equi join but just keep column from dt
dt2 <- dt[sub, .(value, idx = x.idx), on = .(idx >= start, idx <= end)]
head(dt2)
