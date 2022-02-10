library(tidyverse)
library(glue)
library(visdat)
library(here)



# Load Data ---------------------------------------------------------------

load(here::here("tidy_data", "cali_data.RData"))

## Check, always

cali_data %>% skimr::skim()

cali_data %>% vis_dat()

View(cali_data)



# Visualizations ----------------------------------------------------------


## Custom colors from RColorBrewer
my_four <- c("#1F78B4", "#A6CEE3",  "#33A02C", "#B2DF8A")


###  PER 100K people
g_one <-
cali_data %>%
  ggplot( aes(x = status , 
              y = per_100K, 
              fill = status)) +
  geom_col() +
  guides(fill = "none") +
  scale_fill_manual(values = my_four)

g_one

g_one + labs(x = "Population Cohort", y = "Per 100,000 people", 
             title = "Covid Hospitalizations in CA",
             subtitle = "CDC Data: 30-May-2021 to 20-Nov-2021",
             caption = "Data Humanist, CC-By 4.0") 

# Test out variable labeling

g_one +
  scale_x_discrete(
    labels=c( "Vaccinated\nPrior C19" ,
              "Vaccinated\nNo C19",
              "Unvaccinated\nPrior C19",
              "Unvaccinated\nNo C19"
            ) ) + 
  labs(x = "Population Cohort", y = "Per 100,000 people", 
                       title = "Covid Hospitalizations in CA",
                       subtitle = "CDC Data: 30-May-2021 to 20-Nov-2021",
                       caption = "Data Humanist, CC-By 4.0") 


# These labels better suited
g_one  +
  scale_x_discrete(
    labels = c( "YES Natural Immunity:\n YES Vax" , 
                "NO Natural Immunity:\n YES Vax" ,
              "YES Natural Immunity:\n NO Vax" , 
              "NO Natural Immunity:\n NO Vax") 
              ) +
  labs(x = "Population Cohort", y = "Per 100,000 people", 
         title = "Covid Hospitalizations in CA",
         subtitle = "CDC Data: 30-May-2021 to 20-Nov-2021",
         caption = "Data Humanist, CC-By 4.0") 



# Compare Natural immunity with Vaccinated, including NI
focus_cali <- cali_data %>%
  filter(status != "UnVax_No_Prior") %>%
  ggplot( aes(x =status , 
              y = per_100K, 
              fill = status)) +
  geom_col() + coord_flip() +
  guides(fill = "none") +
  scale_fill_manual(values = my_four) +
  scale_x_discrete(
    labels = c( "YES Natural Immunity:\n YES Vax" , 
                "NO Natural Immunity:\n YES Vax" ,
                "YES Natural Immunity:\n NO Vax" ) 
  ) 

focus_cali



focus_cali +   labs(x = "Population Cohort", y = "Per 100,000 people", 
                    title = "Covid Hospitalizations in CA",
                    subtitle = "CDC Data: 30-May-2021 to 20-Nov-2021",
                    caption = "Data Humanist, CC-By 4.0") 
  


# Compare Natural immunity with Vaccinated -- NO NI
hosp_contrast <- cali_data %>%
  filter(status != "UnVax_No_Prior" , status != "Vax_Prior_C19" ) %>%
  mutate(year = "CA") %>%
  ggplot( aes(x =status , 
              y = per_100K, 
              fill = status)) +
  geom_col() + coord_flip() +
  guides(fill = "none") +
  scale_fill_manual(values = c("#A6CEE3", "#33A02C"),
                    labels=c("NO Natural Immunity: YES Vax" ,
                             "YES Natural Immunity: NO Vax") ) +
  scale_x_discrete(
    labels = c(  "NO Natural Immunity:\n YES Vax" ,
                "YES Natural Immunity:\n NO Vax" ) 
  ) 


hosp_contrast + labs(x = "Population Cohort", y = "Per 100,000 people", 
                    title = "Covid Hospitalizations in CA",
                    subtitle = "CDC Data: 30-May-2021 to 20-Nov-2021",
                    caption = "Data Humanist, CC-By 4.0") 


## PERCENTAGES

per_graph <- cali_data %>%
  mutate(year = "CA") %>%
  ggplot( aes(x = year, y = host_per, fill = status)) +
  geom_bar(position="stack", stat="identity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = " CDC\nData", y = "Covid Hospitalizations (%)",
       fill = "Cohort Status") +
coord_flip() +
  theme(legend.position = c(0.5, 0.5) ) +
  scale_fill_manual(values = my_four,
                    labels=c("YES Natural Immunity: YES Vax" , 
                             "NO Natural Immunity: YES Vax" ,
                             "YES Natural Immunity: NO Vax" , 
                             "NO Natural Immunity: NO Vax" ) )

per_graph 

per_graph +  labs(x = "CDC Data", 
                  y = "Covid Hospitalizations (%)", 
                  title = "Covid Hospitalizations in CA",
                  subtitle = "30-May-2021 to 20-Nov-2021",
                  caption = "Data Humanist, CC-By 4.0") 


# Compare Natural immunity with Vaccinated, including NI

focus_per <- cali_data %>%
  filter(status != "UnVax_No_Prior") %>%
  mutate(year = "CA") %>%
  ggplot( aes(x = year, y = host_per, fill = status)) +
  geom_bar(position="stack", stat="identity") +
  coord_flip() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = " CDC\nData", y = "Covid Hospitalizations (%)",
       fill = "Cohort Status") +
  theme(legend.position = c(0.5, 0.5) ) +
  scale_fill_manual(values = my_four,
                    labels=c("YES Natural Immunity: YES Vax" , 
                             "NO Natural Immunity: YES Vax" ,
                             "YES Natural Immunity: NO Vax") )


focus_per +   labs(x = "CDC Data",
                   y = "Covid Hospitalizations (%)", 
                  title = "Covid Hospitalizations in CA",
                  subtitle = "30-May-2021 to 20-Nov-2021",
                  caption = "Data Humanist, CC-By 4.0") 



# Compare Natural immunity with Vaccinated -- NO NI

deep_focus_one <-  cali_data %>%
  filter(status != "UnVax_No_Prior" , status != "Vax_Prior_C19" ) %>%
  mutate(year = "CA") %>%
  ggplot( aes(x = year, y = host_per, fill = status)) +
  geom_bar(position="stack", stat="identity") +
  coord_flip() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = " CDC\nData", y = "Covid Hospitalizations (%)",
       fill = "Cohort Status") +
  theme(legend.position = c(0.5, 0.5) ) +
  scale_fill_manual(values = c("#A6CEE3", "#33A02C"),
                    labels=c("NO Natural Immunity: YES Vax" ,
                             "YES Natural Immunity: NO Vax") )



deep_focus_one +  labs(x = "CDC Data", y = "Covid Hospitalizations (%)", 
                       title = "Covid Hospitalizations in CA",
                       subtitle = "30-May-2021 to 20-Nov-2021",
                       caption = "Data Humanist, CC-By 4.0") 

save.image("~/R_STUDIO/Covid/tidy_data/dashboard2_data.RData")

