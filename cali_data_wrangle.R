library(tidyverse)
library(here)



## start with the CDC data: a text csv file
cali_data <- read_csv(here::here("raw_data", "cali_data.txt"))


## We could easily chain the MUTATE statements together
## This often preferred
## Here, separated to show the process


## Percentages
# total population -- all cohorts
tot_pop <- sum(cali_data$pop_size)

# cohort population percentage
cali_data  <- cali_data %>%
  mutate(pop_per = pop_size / tot_pop)

# prob of cohort member being hospitalized
cali_data  <- cali_data %>%
  mutate(host_prob = C19_hospital /  pop_size)

# scale to per 1000 people -- should match CDC estimates
cali_data  <- cali_data %>%
  mutate(per_thousand = host_prob * 1000)

# scale to per  100K people -- more understandable results
cali_data  <- cali_data %>%
  mutate(per_100K = host_prob * 100000)

## Probabilities

# probabilty of hospitalization for all cohorts
prob_sum <- sum(cali_data$host_prob)

#  share breakdown by cohort 
cali_data  <- cali_data %>%
  mutate(host_per = host_prob / prob_sum)



## For Visualization

# set levels for better visualization
cali_data$status <- factor(cali_data$status,
                           levels = c("Vax_Prior_C19",
                                      "Vax_No_Prior",
                                      "UnVax_Prior_C19",
                                      "UnVax_No_Prior"))

## SAVE 
save(here::here("tidy_data", "cali_data.RData"))


## Pretty version of data

cali_data_show <- cali_data %>%
  rename(Cohort = status, Size = pop_size,
         "Hospital Cases" = C19_hospital,
         "Size (%)" = pop_per,
         "Hosptial Prob." = host_prob,
         "Per 1K" = per_thousand,
         "Per 100K" = per_100K,
         "Hosptial (%)" = host_per)


cali_data_show2 <- cali_data_show %>%
  select(Cohort, Size, `Size (%)`, `Hospital Cases`,
         `Hosptial (%)`, `Hosptial Prob.`, `Per 1K`,
         `Per 100K`)

cali_data_show2 