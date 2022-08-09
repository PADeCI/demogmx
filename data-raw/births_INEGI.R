

# Activate libraries ------------------------------------------------------
library(dplyr)
library(ggplot2)
library(broom)
library(segmented)
library(readr)
library(reshape2)
library(tidyr)



# Import base data --------------------------------------------------------
# Registered births in Mexico by state and by year of registration
# To obtain the information:
# - "Entidad y Municipio de registro” was selected in Características del nacimiento
# - “Sexo” was selected in Características del registrado.
# Source: https://www.inegi.org.mx/sistemas/olap/proyectos/bd/continuas/natalidad/nacimientos.asp?s=est&c=23699&proy=nat_nac
#
# In the original dataset, there is the category "Not specified" in sex variable
# this category is available but was not included

df_muj_base <- read_csv(file = "data-raw/births/Mujeres_reg.csv",
                       locale = locale(encoding = "latin1"))

df_hom_base <- read_csv(file = "data-raw/births/Hombres_reg.csv",
                            locale = locale(encoding = "latin1"))

# # Not specified sex
# df_ne_base <- read_csv(file = "data-raw/births/No_especificado_reg.csv",
#                         locale = locale(encoding = "latin1"))


# Modify Data -------------------------------------------------------------


# Convert from wide to long format
df_muj_long <- df_muj_base %>%
  melt(id.vars = c("State_id", "State"),
       variable.name = "Year",
       value.name = "Births") %>%
  mutate(Sex = "Female") %>%
  as_tibble()

df_hom_long <- df_hom_base %>%
  melt(id.vars = c("State_id", "State"),
       variable.name = "Year",
       value.name = "Births") %>%
  mutate(Sex = "Male") %>%
  as_tibble()

# Check if columns have NA
# apply(X = df_muj_long, MARGIN = 2, FUN = function(x) sum(is.na(x)))
# apply(X = df_hom_long, MARGIN = 2, FUN = function(x) sum(is.na(x)))
colSums(x = is.na(df_hom_long))
colSums(x = is.na(df_muj_long))

# Join both dataframes
df_tot_long_0 <- df_hom_long %>%
  bind_rows(df_muj_long) %>%
  mutate(year = as.numeric(levels(Year)[Year])) %>%
  arrange(Year, State, Sex)

# Create Total category in Sex Variable
df_births_INEGI_0 <- df_tot_long_0 %>%
  group_by(State_id, State, Year, year) %>%
  mutate(prop = Births/sum(Births)) %>%
  ungroup() %>%
  bind_rows(df_tot_long_0 %>%
              group_by(State_id, State, Year, year) %>%
              summarise(Births = sum(Births)) %>%
              mutate(Sex = "Total")) %>%
  arrange(year, State, Sex) %>%
  mutate(prop = replace_na(prop, replace = 1))

# Format rename columns
df_births_INEGI_1 <- df_births_INEGI_0 %>%
  rename(CVE_GEO    = State_id,
         state      = State,
         sex        = Sex,
         births     = Births,
         birth_prop = prop) %>%
  dplyr::select(year, state, sex, CVE_GEO, births, birth_prop)

## CVE_GEO codes
df_births_INEGI_2 <- df_births_INEGI_1 %>%
  mutate(state = replace(state, CVE_GEO ==  1, "Aguascalientes"     ),
         state = replace(state, CVE_GEO ==  2, "Baja California"    ),
         state = replace(state, CVE_GEO ==  3, "Baja California Sur"),
         state = replace(state, CVE_GEO ==  4, "Campeche"           ),
         state = replace(state, CVE_GEO ==  5, "Coahuila"           ),
         state = replace(state, CVE_GEO ==  6, "Colima"             ),
         state = replace(state, CVE_GEO ==  7, "Chiapas"            ),
         state = replace(state, CVE_GEO ==  8, "Chihuahua"          ),
         state = replace(state, CVE_GEO ==  9, "Mexico City"        ),
         state = replace(state, CVE_GEO == 10, "Durango"            ),
         state = replace(state, CVE_GEO == 11, "Guanajuato"         ),
         state = replace(state, CVE_GEO == 12, "Guerrero"           ),
         state = replace(state, CVE_GEO == 13, "Hidalgo"            ),
         state = replace(state, CVE_GEO == 14, "Jalisco"            ),
         state = replace(state, CVE_GEO == 15, "State of Mexico"    ),
         state = replace(state, CVE_GEO == 16, "Michoacan"          ),
         state = replace(state, CVE_GEO == 17, "Morelos"            ),
         state = replace(state, CVE_GEO == 18, "Nayarit"            ),
         state = replace(state, CVE_GEO == 19, "Nuevo Leon"         ),
         state = replace(state, CVE_GEO == 20, "Oaxaca"             ),
         state = replace(state, CVE_GEO == 21, "Puebla"             ),
         state = replace(state, CVE_GEO == 22, "Queretaro"          ),
         state = replace(state, CVE_GEO == 23, "Quintana Roo"       ),
         state = replace(state, CVE_GEO == 24, "San Luis Potosi"    ),
         state = replace(state, CVE_GEO == 25, "Sinaloa"            ),
         state = replace(state, CVE_GEO == 26, "Sonora"             ),
         state = replace(state, CVE_GEO == 27, "Tabasco"            ),
         state = replace(state, CVE_GEO == 28, "Tamaulipas"         ),
         state = replace(state, CVE_GEO == 29, "Tlaxcala"           ),
         state = replace(state, CVE_GEO == 30, "Veracruz"           ),
         state = replace(state, CVE_GEO == 31, "Yucatan"            ),
         state = replace(state, CVE_GEO == 32, "Zacatecas"          ),
         state = replace(state, CVE_GEO ==  0, "National")) %>%
  mutate(state = as.factor(state))

# # Age == 200 is the population of all ages
# df_pop_filt <- df_mortrate_state_age_sex %>%
#   bind_rows(df_mortrate_state_age_sex %>%
#               group_by(year, state, sex, CVE_GEO) %>%
#               mutate(population = sum(population),
#                      age = 200))

# Obtain the total population of each state by sex
df_pop_states <- df_mortrate_state_age_sex %>%
  filter(state != "MCMA") %>%
  group_by(year, state, sex, CVE_GEO) %>%
  summarise(population = sum(population)) %>%
  ungroup()

# Include population information
df_births_INEGI <- df_births_INEGI_2 %>%
  left_join(y = df_pop_states,
            by = c("year", "state", "CVE_GEO", "sex")) %>%
  mutate(birth_rate = births/population)


# # Save data
# usethis::use_data(df_births_INEGI, overwrite = TRUE)



# Visualizations ----------------------------------------------------------

df_births_INEGI_0

df_tot_long %>%
  filter(State == "Total",
         Sex != "Total") %>%
  ggplot(aes(x = year, y = Births, fill = Sex)) +
  geom_bar(position = "fill", stat = "identity")


df_tot_long %>%
  filter(State == "Total") %>%
  ggplot(aes(x = year, y = Births,  color = Sex)) +
  geom_line()


plt_male_birts_prop <- df_births_INEGI_0 %>%
  filter(Sex == "Male",
         State == "Total") %>%
  ggplot(aes(x = year, y = prop)) +
  geom_line() +
  geom_abline(slope = 0, intercept = 0.5, linetype = "dashed", color = "blue") +
  scale_x_continuous(breaks = seq(from = 1985, to = 2020, by = 5),
                     minor_breaks = seq(from = 1985, to = 2020, by = 1)) +
  scale_y_continuous(limits = c(0.49, 0.51),
                     breaks = seq(from = 0.49, to = 0.51, by = 0.002)) +
  theme_bw() +
  theme(panel.grid.major.x = element_line(size = 1.5)) +
  labs(title = "Proportion of male births",
       subtitle = "Mexico, 1985 - 2020",
       caption = "INEGI, 2020")

# ggsave(plt_male_birts_prop,
#        filename = "figs/plt_male_births_prop.png",
#        width = 9, height = 5)

# df_tot_long %>%
#   filter(Sex == "Male",
#          State == "Total") %>%
#   ggplot(aes(x = year, y = prop)) +
#   geom_line() +
#   geom_abline(slope = 0, intercept = 0.5, linetype = "dashed", color = "blue") +
#   scale_x_continuous(breaks = seq(from = 1985, to = 2020, by = 5),
#                      minor_breaks = seq(from = 1985, to = 2020, by = 1)) +
#   scale_y_continuous(limits = c(0.49, 0.51),
#                      breaks = seq(from = 0.49, to = 0.51, by = 0.002)) +
#   theme_bw() +
#   theme(panel.grid.major.x = element_line(size = 1.5)) +
#   labs(title = "Proportion of male births",
#        subtitle = "Mexico, 1985 - 2020",
#        caption = "INEGI, 2020")


# df_tot_long %>%
#   filter(Sex == "Male",
#          State == "Total") %>%
#   ggplot(aes(x = year, y = prop)) +
#   geom_line() +
#   geom_abline(slope = 0, intercept = 0.5, linetype = "dashed", color = "blue") +
#   scale_x_continuous(breaks = seq(from = 1985, to = 2020, by = 5),
#                      minor_breaks = seq(from = 1985, to = 2020, by = 1)) +
#   scale_y_continuous(limits = c(0.49, 0.51),
#                      breaks = seq(from = 0.49, to = 0.51, by = 0.002)) +
#   theme_bw() +
#   theme(panel.grid.major.x = element_line(size = 1.5)) +
#   labs(title = "Proportion of male births",
#        subtitle = "Mexico, 1985 - 2020",
#        caption = "INEGI, 2020")

plt_male_births_prop_states <- df_births_INEGI_0 %>%
  filter(Sex == "Male",
         State != "Total") %>%
  ggplot(aes(x = year, y = prop, color = State)) +
  geom_line(size = 0.7) +
  geom_abline(slope = 0, intercept = 0.5, linetype = "dashed", color = "blue") +
  scale_x_continuous(breaks = seq(from = 1985, to = 2020, by = 5),
                     minor_breaks = seq(from = 1985, to = 2020, by = 1)) +
  theme_bw() +
  theme(panel.grid.major.x = element_line(size = 1.5),
        legend.position = "bottom") +
  labs(title = "Proportion of males",
       subtitle = "Mexico states, 1985 - 2020")

# df_births_states <- df_births_INEGI_0 %>%
#   filter(Sex == "Male",
#          State != "Total")
#
# plty_prop_males_states <- plot_ly(data = df_births_states, x = ~year, y = ~prop, color = ~State,
#         type = "scatter", mode = "lines+markers",
#         line = list(width = 1.3),
#         marker = list(size = 4)) %>%
#   layout(showlegend = T, autosize = TRUE)
#
# class(plty_prop_males_states)
#
# htmlwidgets::saveWidget(widget = plty_prop_males_states,
#                         file = "figs/prop_males_states.html",
#                         selfcontained = TRUE)



df_tot_sum <- df_tot_long %>%
  filter(State != "Total",
         Sex == "Male") %>%
  group_by(State) %>%
  summarise(max = max(prop),
            min = min(prop),
            difmm = max - min,
            mean = mean(prop),
            mode = quantile(x = prop, prob = 0.5))

df_tot_long %>%
  filter(Sex == "Total",
         State == "Total") %>%
  ggplot(aes(x = year, y = Births)) +
  theme_bw() +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1985, to = 2020, by = 5),
                     minor_breaks = seq(from = 1985, to = 2020, by = 1)) +
  scale_y_continuous(breaks = seq(from = 1500000,
                                  to = 3000000,
                                  by = 300000),
                     minor_breaks = seq(from = 1500000,
                                        to = 3000000,
                                        by = 100000),
                     labels = scales::comma) +
  theme(panel.grid.major.x = element_line(size = 1.5),
        panel.grid.major.y = element_line(size = 1.5))


# Piecewise Models --------------------------------------------------------
aa <- df_tot_long %>%
  filter(Sex == "Male",
         State == "Total")

plot(x = aa$year, y = aa$prop)
points(x = 1995, y = y, col = "red")
points(x = 2010, y = y, col = "green")

## with data modifications ------------------------------------------------

# Create needed new rows for pw regression
bb <- aa %>%
  mutate(knot = 2004,
         dummy_knot = ifelse(year > knot, 1, 0),
         pos_year = year - knot,
         pos_inter = dummy_knot*pos_year)

# Fit model
fit_pw <- lm(formula = prop ~ year + pos_inter,
             data = bb)

# Check Model results
summary(fit_pw)

# Extract estimates
ss <- tidy(fit_pw)
intercept <- ss$estimate[1]
c_year <- ss$estimate[2]
c_year_2 <- ss$estimate[3]


# y
## if year <= 2003
y <-  intercept +c_year*1995

## if year > 2003, suppose 2010
y <-  intercept + c_year*2003 + (c_year+c_year_2)*(2010 - 2003)

bb2 <- bb %>%
  mutate(y_value = ifelse(test = dummy_knot == 0,
                          yes = intercept + c_year*year,
                          no = intercept + c_year*year + (c_year_2)*(year-knot)))

bb2 %>%
  ggplot() +
  geom_point(aes(x = 2003, y = 0.4985273), size = 3, color = "green") +
  geom_line(aes(x = year, y = prop)) +
  geom_line(aes(x = year, y = y_value), linetype = "dotted", size = 0.8, color = "blue")  +
  geom_abline(slope = 0, intercept = 0.5, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(from = 1985, to = 2020, by = 5),
                     minor_breaks = seq(from = 1985, to = 2020, by = 1)) +
  scale_y_continuous(limits = c(0.49, 0.51),
                     breaks = seq(from = 0.49, to = 0.51, by = 0.002)) +
  theme_bw() +
  theme(panel.grid.major.x = element_line(size = 1.5))

## With package Segment ---------------------------------------------------

# Fit Model
fit <- lm(formula = prop ~ year,
          data = aa)

segmented_fit <- segmented(fit, seg.Z = ~year, psi = 2003)

summary(segmented_fit)

plot(x = aa$year, y = aa$prop)
plot(segmented_fit, add = T)



