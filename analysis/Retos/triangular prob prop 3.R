


# Load libraries ----------------------------------------------------------
library(demogmx)
library(dplyr)
library(tidyr)


# Load and filter base dataset --------------------------------------------
df_mortrate <- df_mortrate_state_age_sex %>%
  # Sex has to be changed in each iteration
  filter(sex == "Total") %>%
  group_by(year, state) %>%
  mutate(death_rate = deaths / population,
         death_rate = replace_na(death_rate, 0), # REPLACE NA'S with 0's
         death_prob_cum = 1 - exp(-cumsum(death_rate)),
         death_prob = death_prob_cum - lag(death_prob_cum)
         # death_prob = c(death_prob_cum[1], diff(death_prob_cum))
         # death_prob = diff(c(0, death_prob_cum))
         ) %>%
  ungroup() %>%
  mutate(death_prob = if_else(condition = age == 0,
                              true = death_prob_cum,
                              false = death_prob))

# Create a dataframe with the transposed death probabilities
df_DP <- matrix(data = df_mortrate$death_prob,
               ncol = length(unique(df_mortrate$age)),
               byrow = TRUE) %>%
  as.data.frame() %>%
  # Repeat eachrow by the total number of ages (110)
  slice(rep(1:n(),
            each = length(unique(df_mortrate$age))))

# Rename columns
colnames(df_DP) <- paste0("DP", 0:109)

# Create a upper triangular matrix, then df, for the 110 ages
df_tri <- (upper.tri(x = matrix(data = 1,
                              nrow = length(unique(df_mortrate$age)),
                              ncol = length(unique(df_mortrate$age))
                              ),
                   diag = TRUE)*1) %>%
  as.data.frame()

colnames(df_tri) <- paste0("V", 0:109)

# stack arrays vertically
df_tri_exp <- do.call("rbind",
                      replicate(n = (nrow(df_DP)/length(unique(df_mortrate$age))),
                                expr = df_tri,
                                simplify = FALSE))

# Check that all dataframes have the correct dimensions
dim(df_mortrate)
dim(df_DP)
dim(df_tri_exp)

# Data Manipulation -------------------------------------------------------
df_mortrate_1 <- df_mortrate %>%
  bind_cols(df_DP, df_tri_exp) %>%
  # Make multiplication to obtain
  mutate(across(DP0:DP109) * across(V0:V109)) %>%
  # Remove the upper triangle matrix
  select(-c(V0:V109)) %>%
  # Obtain proportion of probability in relation to the rowsum
  mutate(across(.cols = DP0:DP109) / rowSums(across(.cols = DP0:DP109))) %>%
  # Replace the NA's by 0
  replace(is.na(.), 0)


