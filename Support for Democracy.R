library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)  
library(rnaturalearthdata)
library(haven)
rm(list = ls())

nigeria_states <- ne_states(country = "Nigeria", returnclass = "sf") 
survey_data_2012 <- read_sav("~/Desktop/Dissertation /Nigeria Round 5 data (2013)")

select_data_2012 <- survey_data_2012 %>% 
  select(name = REGION, Age = Q1, Support_for_democracy = Q32) %>% 
  mutate(Year = 2012)

select_data_2012$name <- factor(select_data_2012$name, levels = 620:656, labels = c(
  "Abia", "Adamawa",  %>% , "Anambra", "Bauchi", "Bayelsa",
  "Benue", "Borno", "Cross-River", "Delta", "Ebonyi", "Edo",
  "Ekiti", "Enugu", "FCT", "Gombe", "Imo", "Jigawa", "Kaduna",
  "Kano", "Katsina", "Kebbi", "Kogi", "Kwara", "Lagos", "Nasarawa",
  "Niger", "Ogun", "Ondo", "Osun", "Oyo", "Plateau", "Rivers",
  "Sokoto", "Taraba", "Yobe", "Zamfara"
))

select_data_2012 <- select_data_2012 %>% 
  filter(Support_for_democracy %in% c(1,2,3))

# Compute percentage of respondents who support democracy (Q32 == 3)
percent_support_2012 <- select_data_2012 %>% 
  group_by(name) %>% 
  summarise(
    total_responses = n(),
    democracy_supporters = sum(Support_for_democracy == 3),
    support_percentage = democracy_supporters / total_responses * 100
  )

selected_nigeria_2012 <-   nigeria_states %>% 
  select(name, longitude, latitude) %>% 
  mutate(Year = 2012)

combined_data_2012 <- selected_nigeria_2012 %>% 
  left_join(percent_support_2012, by = "name")

ggplot(combined_data_2012) +
  geom_sf(aes(fill = support_percentage), color = "black", size = 0.3) +  # Map with crime data
  scale_fill_distiller(palette = "Reds", direction = 1) + # Use Viridis color scale
  theme_minimal() +
  labs(
    title = "Support for Democracy by Region (2012)",
    fill = "Support (%)"
  )


########## 2015 now
rm(list = ls())
nigeria_states <- ne_states(country = "Nigeria", returnclass = "sf") 
survey_data_2015 <- read_sav("~/Desktop/Dissertation /Nigeria Round 6 data (2015)")

select_data_2015 <- survey_data_2015 %>% 
  select(name = REGION, Age = Q1, Support_for_democracy = Q32) %>% 
  mutate(Year = 2015)

select_data_2015$name <- factor(select_data_2015$name, levels = 620:656, labels = c(
  "Abia", "Adamawa", "Akwa-Ibom", "Anambra", "Bauchi", "Bayelsa",
  "Benue", "Borno", "Cross-River", "Delta", "Ebonyi", "Edo",
  "Ekiti", "Enugu", "FCT", "Gombe", "Imo", "Jigawa", "Kaduna",
  "Kano", "Katsina", "Kebbi", "Kogi", "Kwara", "Lagos", "Nasarawa",
  "Niger", "Ogun", "Ondo", "Osun", "Oyo", "Plateau", "Rivers",
  "Sokoto", "Taraba", "Yobe", "Zamfara"
))

select_data_2015 <- select_data_2015 %>% 
  filter(Support_for_democracy %in% c(1,2,3))

# Compute percentage of respondents who support democracy (Q32 == 3)
percent_support_2015 <- select_data_2015 %>% 
  group_by(name) %>% 
  summarise(
    total_responses = n(),
    democracy_supporters = sum(Support_for_democracy == 3),
    support_percentage = democracy_supporters / total_responses * 100
  )

selected_nigeria_2015 <-   nigeria_states %>% 
  select(name, longitude, latitude) %>% 
  mutate(Year = 2015)

combined_data_2015 <- selected_nigeria_2015 %>% 
  left_join(percent_support_2015, by = "name")


ggplot(combined_data_2015) +
  geom_sf(aes(fill = support_percentage), color = "black", size = 0.3) +  # Map with crime data
  scale_fill_distiller(palette = "Reds", direction = 1) + # Use Viridis color scale
  theme_minimal() +
  labs(
    title = "Support for Democracy by Region (2015)",
    fill = "Support (%)"
  )


####### combing 2012 and 2015 years

select_2015 <- combined_data_2015 %>% 
  select(name, Year, total_responses, democracy_supporters, support_percentage)

full_year <- bind_rows(combined_data_2012, combined_data_2015)

ggplot(full_year) +
  geom_sf(aes(fill = support_percentage), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  facet_wrap(~Year) +  # Separate maps for each year
  labs(title = "Support for Democracy Over Time",
       fill = "Support (%)")


library(tmap)

tm_shape(full_year) +
  tm_polygons("support_percentage", palette = "Blues", title = "Support for Democracy") +
  tm_facets(by = "Year")  # Creates different maps for each year

