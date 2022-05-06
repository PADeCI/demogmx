


# Load libraries ----------------------------------------------------------
library(demogmx)
library(dplyr)
library(tidyr)
library(matrixStats)

# Load and filter base dataset --------------------------------------------
# Pick a test dataset filtering to have only one year, sex and state
df_mortrate_ags <- df_mortrate_state_age_sex %>%
  filter(sex == "Total",
         year == 1970,
         state == "Aguascalientes") %>%
  mutate(death_rate = deaths / population,
         death_rate = replace_na(death_rate, 0), # REPLACE NA'S with 0's
         death_prob_cum = 1 - exp(-cumsum(death_rate)),
         # death_prob = c(death_prob_cum[1], diff(death_prob_cum)),
         death_prob = diff(c(0, death_prob_cum)))

# Create diagonal matrix of dims(110*110)
df_diag <- diag(nrow = nrow(df_mortrate_ags),
     ncol = nrow(df_mortrate_ags)) %>%
    as.data.frame()


# Create a triangular matrix of dims(110 * 110)
df_tri <- (upper.tri(x = matrix(data = 1,
                              nrow = nrow(df_mortrate_ags),
                              ncol = nrow(df_mortrate_ags)),
                   diag = TRUE)*1) %>%
  as.data.frame()

df_DP <- matrix(data = rep(df_mortrate_ags$death_prob, 110),
                nrow = 110,
                ncol = 110,
                byrow = TRUE,
                dimnames = list(c(1:110),
                                paste0("DP", 1:110))) %>%
  as.data.frame()


# Data Manipulation -------------------------------------------------------
df_mortrate_ags_1 <- df_mortrate_ags %>%
  bind_cols(df_DP, df_tri) %>%
  mutate(across(DP1:DP110) * across(V1:V110)) %>%
  select(-c(V1:V110)) %>%
  mutate(across(.cols = DP1:DP110) / rowSums(across(.cols = DP1:DP110)))


# Check results -----------------------------------------------------------
# Manually obtain the probability proportion by row

# First row
v_prop_probs_1 <- df_tri_probs[1, ]/sum(df_tri_probs[1, ])
v_prop_probs_1[49]

# 46th row
v_prop_probs_46 <- df_tri_probs[46, ]/sum(df_tri_probs[46, ])
v_prop_probs_46[49]


# Compare the values with their corresponding place in df_tri_probs_prop
v_prop_probs_1[49]  == df_mortrate_ags_1[1, "DP49"]
v_prop_probs_46[49] == df_mortrate_ags_1[46, "DP49"]


# # Vertically stack df_tri 110 times
# # USE THIS WHEN ARE MORE THAN ONE STATES AND YEARS.
# df_xx <- do.call("rbind", replicate(n = 2, expr = df_tri, simplify = FALSE))
