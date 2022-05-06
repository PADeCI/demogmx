


# Load libraries ----------------------------------------------------------
library(demogmx)
library(dplyr)
library(tidyr)


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


# Create a triangular matrix of dims(110 * 110)
m_tri <- upper.tri(x = matrix(data = 1,
                              nrow = nrow(df_mortrate_ags),
                              ncol = nrow(df_mortrate_ags)),
                   diag = TRUE)*1

# Change the format of m_tri towards a data.frame
df_tri <- as.data.frame(m_tri)

# Data Manipulation -------------------------------------------------------
# Create squared dataframe of death probabilities by age
df_probs <- do.call("rbind", replicate(n = nrow(df_mortrate_ags),
                                       expr = df_mortrate_ags$death_prob,
                                       simplify = FALSE)) %>%
  as.data.frame()

# Triangular dataframe of probabilities
df_tri_probs <- df_probs * df_tri


# Obtain Triangular df with proportions of probabilities by row
df_tri_probs_prop <- df_tri_probs %>%
  mutate(across(.cols = 1:110) / rowSums(across(1:110))) %>%
  # Replace NA's obtained when dividing by zero in last rows
  replace(is.na(.), 0)


# Check results -----------------------------------------------------------
# Manually obtain the probability proportion by row

# First row
v_prop_probs_1 <- df_tri_probs[1, ]/sum(df_tri_probs[1, ])
v_prop_probs_1[49]

# 46th row
v_prop_probs_46 <- df_tri_probs[46, ]/sum(df_tri_probs[46, ])
v_prop_probs_46[49]


# Compare the values with their corresponding place in df_tri_probs_prop
v_prop_probs_1[49]  == df_tri_probs_prop[1, 49]
v_prop_probs_46[49] == df_tri_probs_prop[46, 49]


# # Vertically stack df_tri 110 times
# # USE THIS WHEN ARE MORE THAN ONE STATES AND YEARS.
# df_xx <- do.call("rbind", replicate(n = 2, expr = df_tri, simplify = FALSE))
