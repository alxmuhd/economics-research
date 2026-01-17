library(tidyverse)

crime_agg <- read.csv("~/Desktop/Dissertation /crime_agg.csv")
survey <- read.csv("~/Desktop/Dissertation /survey.csv")

trend_assumption <- survey %>% 
  group_by(year) %>% 
  summarise(
    total_responses = n(),
    democracy_supporters = sum(support.for.democracy == 3),
    support_percentage = democracy_supporters / total_responses * 100
  )


merged_data <- merge(survey, crime_agg, by = c("state", "year"), all.x = TRUE)

merged_data <- merged_data %>%
  mutate(
      region = case_when(
       state %in% c("Borno","Yobe", "Adamawa", "Bauchi", "Gombe", "Taraba") ~ "North East (Treated)",
       TRUE ~ "Other (Control)"
    )
  )

# #plotting regional pre-trends
# region_parallel_trend <- merged_data %>% 
#   mutate(
#     region = case_when(
#       state %in% c("Borno","Yobe", "Adamawa", "Bauchi", "Gombe", "Taraba") ~ "North East (Treated)",
#       TRUE ~ "Other (Control)"
#     )
#   ) %>%
#   group_by(region) %>%
#   summarise(
#     states = toString(unique(state)),
#     avg_support_pre2014 = mean(support.for.democracy[year < 2014], na.rm = TRUE),
#     avg_urban = mean(Urban.Rural == 1, na.rm = TRUE),  # % urban
#     avg_age = mean(age, na.rm = TRUE)
#   )
# 
# ggplot(merged_data %>% filter(year < 2014), 
#        aes(x = year, y = support.for.democracy, color = region)) +
#   stat_summary(fun = mean, geom = "line") +
#   labs(title = "Region-Based Parallel Trends (Pre-2014)",
#        y = "Support for Democracy (1-3)", x = "Year") +
#   geom_vline(xintercept = 2014, linetype = "dashed")  # Conflict onset

region_plot <- merged_data %>% 
  filter(year < 2014) %>%  # Focus on pre-conflict period
  mutate(
    region = ifelse(state %in% c("Borno", "Yobe", "Adamawa", "Bauchi", "Gombe", "Taraba"), 
                    "North East (Treated)", "Other (Control)")
  ) %>%
  group_by(region, year) %>% 
  summarise(avg_support = mean(support.for.democracy, na.rm = TRUE))

ggplot(region_plot, aes(x = year, y = avg_support, color = region)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Region-Based Parallel Trends (Pre-2014)",
    y = "Average Support for Democracy (1-3 scale)", 
    x = "Year",
    color = "Region"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("North East (Treated)" = "red", "Other (Control)" = "blue"))



#########full timeline regional parallel trend
# Calculate average support by region and year
full_trends <- merged_data %>%
  group_by(region, year) %>%
  summarise(
    avg_support = mean(support.for.democracy, na.rm = TRUE),
    se = sd(support.for.democracy, na.rm = TRUE) / sqrt(n())  # Standard error
  )

# Plot with confidence intervals
filter(
  full_trends, year >=2005
) %>% 
ggplot( aes(x = year, y = avg_support, color = region)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_support - 1.96*se, ymax = avg_support + 1.96*se), 
                width = 0.2, alpha = 0.5) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 2015, y = max(full_trends$avg_support), 
           label = "Conflict Escalation (2014)", hjust = 0, color = "red") +
  labs(
    title = "Support for Democracy: North East vs. Other Regions (2001–2022)",
    y = "Average Support (1-3 Scale)", 
    x = "Year",
    color = "Region"
  ) +
  scale_color_manual(values = c("North East (Treated)" = "#E41A1C", "Other (Control)" = "#377EB8")) +
  theme_minimal() +
  theme(legend.position = "bottom")
\################

###crime line graph
crime <- read.csv("~/Desktop/Dissertation /Africa_1997-2025_Jan31.csv")
crime <- crime %>% 
  filter(country=="Nigeria", year >=2003)

# First, aggregate your data by year and event type
crime_agg2 <- crime %>%
  group_by(year, event_type) %>%
  summarise(total_fatalities = sum(fatalities)) %>%
  ungroup()

# Create the plot
ggplot(data = crime_agg2, aes(x = year, y = total_fatalities, colour = event_type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Fatalities by Event Type in Nigeria (2005-2025)",
       x = "Year",
       y = "Number of Fatalities",
       colour = "Event Type") +
  scale_x_continuous(breaks = seq(2005, 2025, by = 5)) +  # Set x-axis breaks
  scale_y_continuous(labels = scales::comma) +  # Format y-axis numbers
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))


ggplot(data =crime, aes(x = year, y = fatalities, colour = event_type)) + 
  geom_line() + theme_minimal()

### Econometrics diff and diff
library(fixest)

merged_data <- merged_data %>%
  mutate(
    treated = ifelse(state %in% c("Borno", "Yobe", "Adamawa","Bauchi", "Gombe", "Taraba"), 1, 0),
    post_2014 = ifelse(year >= 2014, 1, 0)
  )



# Two-way fixed effects model (state + year)
did_model <- feols(
  support.for.democracy ~ treated * post_2014 | state + year,
  data = merged_data,
  cluster = ~state  # Cluster SEs at state level
)
summary(did_model)

filtered_merged_data <- merged_data %>% 
  filter(year >= 2005)

####summary tables
library(dplyr)
library(tibble)
library(knitr)
library(kable)

#full sample
overall <- filtered_merged_data %>% 
  summarise(
    Mean_Age = mean(age, na.rm = TRUE),
    SD_Age = sd(age, na.rm = TRUE),
    Percent_Female = mean(gender == 1, na.rm = TRUE) * 100,
    Percent_Unemployed = mean(employment == 0, na.rm = TRUE) * 100,
    Percent_Urban = mean(Urban.Rural == 1, na.rm = TRUE) * 100,
    Percent_Pro_Democracy = mean(support.for.democracy == 3, na.rm = TRUE) * 100,
    Percent_Educated = mean(education >= 5, na.rm = TRUE) * 100,
    Mean_Fatalities = mean(yearly_total_fatalities, na.rm = TRUE),
    SD_Fatalities = sd(yearly_total_fatalities, na.rm = TRUE)
  )

# ====BY TREATED (Conflict vs. Non-Conflict) ====
conflict_stats <- filtered_merged_data %>%
  group_by(treated) %>%
  summarise(
    Mean_Age = mean(age, na.rm = TRUE),
    SD_Age = sd(age, na.rm = TRUE),
    Percent_Female = mean(gender == 1, na.rm = TRUE) * 100,
    Percent_Unemployed = mean(employment == 0, na.rm = TRUE) * 100,
    Percent_Urban = mean(Urban.Rural == 1, na.rm = TRUE) * 100,
    Percent_Pro_Democracy = mean(support.for.democracy == 3, na.rm = TRUE) * 100,
    Percent_Educated = mean(education >= 5, na.rm = TRUE) * 100,
    Mean_Fatalities = mean(yearly_total_fatalities, na.rm = TRUE),
    SD_Fatalities = sd(yearly_total_fatalities, na.rm = TRUE)
  )

pvals_conflict <- tibble(
  Age = t.test(age ~ treated, data = filtered_merged_data)$p.value,
  Female = t.test(gender == 1 ~ treated, data = filtered_merged_data)$p.value,
  Unemp = t.test(employment == 0 ~ treated, data = filtered_merged_data)$p.value,
  Urban = t.test(Urban.Rural == 1 ~ treated, data = filtered_merged_data)$p.value,
  Democracy = t.test(support.for.democracy == 3 ~ treated, data = filtered_merged_data)$p.value,
  Educated = t.test(education >= 5 ~ treated, data = filtered_merged_data)$p.value,
  Fatalities = t.test(yearly_total_fatalities ~ treated, data = filtered_merged_data)$p.value
)

# ==== BY POST_2014 (Pre vs. Post Escalation) ====
post_stats <- filtered_merged_data %>%
  group_by(post_2014) %>%
  summarise(
    Mean_Age = mean(age, na.rm = TRUE),
    SD_Age = sd(age, na.rm = TRUE),
    Percent_Female = mean(gender == 1, na.rm = TRUE) * 100,
    Percent_Unemployed = mean(employment == 0, na.rm = TRUE) * 100,
    Percent_Urban = mean(Urban.Rural == 1, na.rm = TRUE) * 100,
    Percent_Pro_Democracy = mean(support.for.democracy == 3, na.rm = TRUE) * 100,
    Percent_Educated = mean(education >= 5, na.rm = TRUE) * 100,
    Mean_Fatalities = mean(yearly_total_fatalities, na.rm = TRUE),
    SD_Fatalities = sd(yearly_total_fatalities, na.rm = TRUE)
  )

pvals_post <- tibble(
  Age = t.test(age ~ post_2014, data = filtered_merged_data)$p.value,
  Female = t.test(gender == 1 ~ post_2014, data = filtered_merged_data)$p.value,
  Unemp = t.test(employment == 0 ~ post_2014, data = filtered_merged_data)$p.value,
  Urban = t.test(Urban.Rural == 1 ~ post_2014, data = filtered_merged_data)$p.value,
  Democracy = t.test(support.for.democracy == 3 ~ post_2014, data = filtered_merged_data)$p.value,
  Educated = t.test(education >= 5 ~ post_2014, data = filtered_merged_data)$p.value,
  Fatalities = t.test(yearly_total_fatalities ~ post_2014, data = filtered_merged_data)$p.value
)

# ==== COMBINE EVERYTHING INTO TABLE 1 ====
summary_table <- tibble(
  Characteristic = c(
    "Mean Age (SD)", "% Female", "% Unemployed", "% Urban", 
    "% Support Democracy", "% Educated", "Fatalities (Mean, SD)"
  ),
  `Full Sample` = c(
    sprintf("%.1f (%.1f)", overall$Mean_Age, overall$SD_Age),
    sprintf("%.1f", overall$Percent_Female),
    sprintf("%.1f", overall$Percent_Unemployed),
    sprintf("%.1f", overall$Percent_Urban),
    sprintf("%.1f", overall$Percent_Pro_Democracy),
    sprintf("%.1f", overall$Percent_Educated),
    sprintf("%.1f (%.1f)", overall$Mean_Fatalities, overall$SD_Fatalities)
  ),
  `Treated Group` = c(
    sprintf("%.1f (%.1f)", conflict_stats$Mean_Age[2], conflict_stats$SD_Age[2]),
    sprintf("%.1f", conflict_stats$Percent_Female[2]),
    sprintf("%.1f", conflict_stats$Percent_Unemployed[2]),
    sprintf("%.1f", conflict_stats$Percent_Urban[2]),
    sprintf("%.1f", conflict_stats$Percent_Pro_Democracy[2]),
    sprintf("%.1f", conflict_stats$Percent_Educated[2]),
    sprintf("%.1f (%.1f)", conflict_stats$Mean_Fatalities[2], conflict_stats$SD_Fatalities[2])
  ),
  `Control Group` = c(
    sprintf("%.1f (%.1f)", conflict_stats$Mean_Age[1], conflict_stats$SD_Age[1]),
    sprintf("%.1f", conflict_stats$Percent_Female[1]),
    sprintf("%.1f", conflict_stats$Percent_Unemployed[1]),
    sprintf("%.1f", conflict_stats$Percent_Urban[1]),
    sprintf("%.1f", conflict_stats$Percent_Pro_Democracy[1]),
    sprintf("%.1f", conflict_stats$Percent_Educated[1]),
    sprintf("%.1f (%.1f)", conflict_stats$Mean_Fatalities[1], conflict_stats$SD_Fatalities[1])
  ),
  `p (Conflict)` = c(
    sprintf("%.3f", pvals_conflict$Age),
    sprintf("%.3f", pvals_conflict$Female),
    sprintf("%.3f", pvals_conflict$Unemp),
    sprintf("%.3f", pvals_conflict$Urban),
    sprintf("%.3f", pvals_conflict$Democracy),
    sprintf("%.3f", pvals_conflict$Educated),
    sprintf("%.3f", pvals_conflict$Fatalities)
  ),
  `Post-2014` = c(
    sprintf("%.1f (%.1f)", post_stats$Mean_Age[2], post_stats$SD_Age[2]),
    sprintf("%.1f", post_stats$Percent_Female[2]),
    sprintf("%.1f", post_stats$Percent_Unemployed[2]),
    sprintf("%.1f", post_stats$Percent_Urban[2]),
    sprintf("%.1f", post_stats$Percent_Pro_Democracy[2]),
    sprintf("%.1f", post_stats$Percent_Educated[2]),
    sprintf("%.1f (%.1f)", post_stats$Mean_Fatalities[2], post_stats$SD_Fatalities[2])
  ),
  `Pre-2014` = c(
    sprintf("%.1f (%.1f)", post_stats$Mean_Age[1], post_stats$SD_Age[1]),
    sprintf("%.1f", post_stats$Percent_Female[1]),
    sprintf("%.1f", post_stats$Percent_Unemployed[1]),
    sprintf("%.1f", post_stats$Percent_Urban[1]),
    sprintf("%.1f", post_stats$Percent_Pro_Democracy[1]),
    sprintf("%.1f", post_stats$Percent_Educated[1]),
    sprintf("%.1f (%.1f)", post_stats$Mean_Fatalities[1], post_stats$SD_Fatalities[1])
  ),
  `p (Post2014)` = c(
    sprintf("%.3f", pvals_post$Age),
    sprintf("%.3f", pvals_post$Female),
    sprintf("%.3f", pvals_post$Unemp),
    sprintf("%.3f", pvals_post$Urban),
    sprintf("%.3f", pvals_post$Democracy),
    sprintf("%.3f", pvals_post$Educated),
    sprintf("%.3f", pvals_post$Fatalities)
  )
)

# | Treated Group (Pre-2014) | Treated Group (Post-2014) | Control Group (Pre-2014) | Control Group (Post-2014) | with 4 group version 
# 1. Create interaction group variable
filtered_merged_data <- filtered_merged_data %>%
  mutate(group = case_when(
    treated == 1 & post_2014 == 0 ~ "Treated_Pre2014",
    treated == 1 & post_2014 == 1 ~ "Treated_Post2014",
    treated == 0 & post_2014 == 0 ~ "Control_Pre2014",
    treated == 0 & post_2014 == 1 ~ "Control_Post2014"
  ))

# 2. Summary stats by group
group_stats <- filtered_merged_data %>%
  group_by(group) %>%
  summarise(
    Mean_Age = mean(age, na.rm = TRUE),
    SD_Age = sd(age, na.rm = TRUE),
    Percent_Female = mean(gender == 1, na.rm = TRUE) * 100,
    Percent_Unemployed = mean(employment == 0, na.rm = TRUE) * 100,
    Percent_Urban = mean(Urban.Rural == 1, na.rm = TRUE) * 100,
    Percent_Pro_Democracy = mean(support.for.democracy == 3, na.rm = TRUE) * 100,
    Percent_Educated = mean(education >= 5, na.rm = TRUE) * 100,
    Mean_Fatalities = mean(yearly_total_fatalities, na.rm = TRUE),
    SD_Fatalities = sd(yearly_total_fatalities, na.rm = TRUE),
    Mean_events = mean(yearly_total_events, na.rm = TRUE),
    SD_events = sd(yearly_total_events, na.rm = TRUE)
  ) %>%
  arrange(factor(group, levels = c("Treated_Pre2014", "Treated_Post2014", "Control_Pre2014", "Control_Post2014")))

# === 1. Calculate p-values for conflict (treated vs control) and post-2014 ===
pvals_conflict <- tibble(
  Age = t.test(age ~ treated, data = filtered_merged_data)$p.value,
  Female = t.test(gender == 1 ~ treated, data = filtered_merged_data)$p.value,
  Unemp = t.test(employment == 0 ~ treated, data = filtered_merged_data)$p.value,
  Urban = t.test(Urban.Rural == 1 ~ treated, data = filtered_merged_data)$p.value,
  Democracy = t.test(support.for.democracy == 3 ~ treated, data = filtered_merged_data)$p.value,
  Educated = t.test(education >= 5 ~ treated, data = filtered_merged_data)$p.value,
  Fatalities = t.test(yearly_total_fatalities ~ treated, data = filtered_merged_data)$p.value,
  Events = t.test(yearly_total_events ~ treated, data = filtered_merged_data)$p.value
)

pvals_post <- tibble(
  Age = t.test(age ~ post_2014, data = filtered_merged_data)$p.value,
  Female = t.test(gender == 1 ~ post_2014, data = filtered_merged_data)$p.value,
  Unemp = t.test(employment == 0 ~ post_2014, data = filtered_merged_data)$p.value,
  Urban = t.test(Urban.Rural == 1 ~ post_2014, data = filtered_merged_data)$p.value,
  Democracy = t.test(support.for.democracy == 3 ~ post_2014, data = filtered_merged_data)$p.value,
  Educated = t.test(education >= 5 ~ post_2014, data = filtered_merged_data)$p.value,
  Fatalities = t.test(yearly_total_fatalities ~ post_2014, data = filtered_merged_data)$p.value,
  Events = t.test(yearly_total_events ~ post_2014, data = filtered_merged_data)$p.value
)

# === 2. Construct final table with p-value columns added ===
summary_table <- tibble(
  Characteristic = c(
    "Mean Age (SD)", "% Female", "% Unemployed", "% Urban",
    "% Support Democracy", "% Educated", "Fatalities (Mean, SD)", "Total events (Mean, SD)"
  ),
  `Treated Group (Pre-2014)` = c(
    sprintf("%.1f (%.1f)", group_stats$Mean_Age[1], group_stats$SD_Age[1]),
    sprintf("%.1f", group_stats$Percent_Female[1]),
    sprintf("%.1f", group_stats$Percent_Unemployed[1]),
    sprintf("%.1f", group_stats$Percent_Urban[1]),
    sprintf("%.1f", group_stats$Percent_Pro_Democracy[1]),
    sprintf("%.1f", group_stats$Percent_Educated[1]),
    sprintf("%.1f (%.1f)", group_stats$Mean_Fatalities[1], group_stats$SD_Fatalities[1]),
    sprintf("%.1f (%.1f)", group_stats$Mean_events[1], group_stats$SD_events[1])
  ),
  `Treated Group (Post-2014)` = c(
    sprintf("%.1f (%.1f)", group_stats$Mean_Age[2], group_stats$SD_Age[2]),
    sprintf("%.1f", group_stats$Percent_Female[2]),
    sprintf("%.1f", group_stats$Percent_Unemployed[2]),
    sprintf("%.1f", group_stats$Percent_Urban[2]),
    sprintf("%.1f", group_stats$Percent_Pro_Democracy[2]),
    sprintf("%.1f", group_stats$Percent_Educated[2]),
    sprintf("%.1f (%.1f)", group_stats$Mean_Fatalities[2], group_stats$SD_Fatalities[2]),
    sprintf("%.1f (%.1f)", group_stats$Mean_events[2], group_stats$SD_events[2])
  ),
  `Control Group (Pre-2014)` = c(
    sprintf("%.1f (%.1f)", group_stats$Mean_Age[3], group_stats$SD_Age[3]),
    sprintf("%.1f", group_stats$Percent_Female[3]),
    sprintf("%.1f", group_stats$Percent_Unemployed[3]),
    sprintf("%.1f", group_stats$Percent_Urban[3]),
    sprintf("%.1f", group_stats$Percent_Pro_Democracy[3]),
    sprintf("%.1f", group_stats$Percent_Educated[3]),
    sprintf("%.1f (%.1f)", group_stats$Mean_Fatalities[3], group_stats$SD_Fatalities[3]),
    sprintf("%.1f (%.1f)", group_stats$Mean_events[3], group_stats$SD_events[3])
  ),
  `Control Group (Post-2014)` = c(
    sprintf("%.1f (%.1f)", group_stats$Mean_Age[4], group_stats$SD_Age[4]),
    sprintf("%.1f", group_stats$Percent_Female[4]),
    sprintf("%.1f", group_stats$Percent_Unemployed[4]),
    sprintf("%.1f", group_stats$Percent_Urban[4]),
    sprintf("%.1f", group_stats$Percent_Pro_Democracy[4]),
    sprintf("%.1f", group_stats$Percent_Educated[4]),
    sprintf("%.1f (%.1f)", group_stats$Mean_Fatalities[4], group_stats$SD_Fatalities[4]),
    sprintf("%.1f (%.1f)", group_stats$Mean_events[4], group_stats$SD_events[4])
  ),
  
  # NEW: P-values for conflict and post-2014
  `p (Conflict)` = c(
    sprintf("%.3f", pvals_conflict$Age),
    sprintf("%.3f", pvals_conflict$Female),
    sprintf("%.3f", pvals_conflict$Unemp),
    sprintf("%.3f", pvals_conflict$Urban),
    sprintf("%.3f", pvals_conflict$Democracy),
    sprintf("%.3f", pvals_conflict$Educated),
    sprintf("%.3f", pvals_conflict$Fatalities),
    sprintf("%.3f", pvals_conflict$Events)
  ),
  `p (Post-2014)` = c(
    sprintf("%.3f", pvals_post$Age),
    sprintf("%.3f", pvals_post$Female),
    sprintf("%.3f", pvals_post$Unemp),
    sprintf("%.3f", pvals_post$Urban),
    sprintf("%.3f", pvals_post$Democracy),
    sprintf("%.3f", pvals_post$Educated),
    sprintf("%.3f", pvals_post$Fatalities),
    sprintf("%.3f", pvals_post$Events)
  )
)

# === 3. Display table neatly ===
kable(summary_table, caption = "Table 1: Sample Characteristics by Conflict Exposure and Time Period (with p-values)")




# Event study model
merged_data <- merged_data %>%
  mutate(year = as.factor(year))

event_model <- feols(
  support.for.democracy ~ i(year, treated, ref = "2012") | state + year,
  data = merged_data,
  cluster = ~state
)

# Plot coefficients
iplot(event_model, main = "Event Study: Conflict Exposure and Support for Democracy",
      xlab = "Year", ylab = "Effect on Support for Democracy (relative to 2012)",
      col = "darkred")
abline(h = 0, lty = 2)





#Placebo Tests
library(fixest)
# Ensure year is numeric before you do anything else
merged_data <- merged_data %>%
  mutate(year = as.numeric(as.character(year)))

# --- Placebo 2008 ---
merged_data <- merged_data %>%
  mutate(post_2008 = ifelse(year >= 2008, 1, 0))

placebo_2008 <- feols(
  support.for.democracy ~ treated * post_2008 | state + year,
  data = merged_data,
  cluster = ~state
)
summary(placebo_2008)

# --- Placebo 2012 ---
merged_data <- merged_data %>%
  mutate(post_2012 = ifelse(year >= 2012, 1, 0))

placebo_2012 <- feols(
  support.for.democracy ~ treated * post_2012 | state + year,
  data = merged_data,
  cluster = ~state
)
summary(placebo_2012)

#alternative clustering levels 
# Create geopolitical zone variable (if not already in data)
merged_data <- merged_data %>%
  mutate(zone = case_when(
    state %in% c("Borno", "Yobe", "Adamawa", "Bauchi", "Gombe", "Taraba") ~ "North East",
    state %in% c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara") ~ "North West",
    state %in% c("Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau", "FCT") ~ "North Central",
    state %in% c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo") ~ "South East",
    state %in% c("Akwa Ibom", "Bayelsa", "Cross River", "Delta", "Edo", "Rivers") ~ "South South",
    state %in% c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo") ~ "South West"
  ))

# Run main DiD model but cluster at zone level
did_cluster_zone <- feols(
  support.for.democracy ~ treated * post_2014 | state + year,
  data = merged_data,
  cluster = ~zone
)
summary(did_cluster_zone)

#sample restriction 
# Exclude borderline states
restricted_data <- merged_data %>%
  filter(!state %in% c("Bauchi", "Taraba"))

did_restricted <- feols(
  support.for.democracy ~ treated * post_2014 | state + year,
  data = restricted_data,
  cluster = ~state
)
summary(did_restricted)

# alternative treatment definition definition (intensity-based)

# --- INTENSITY CLASSIFICATION BASED ON YEARLY FATALITIES ---
merged_data <- merged_data %>%
  mutate(
    conflict_intensity = case_when(
      yearly_total_fatalities >= 10000 ~ "High",
      yearly_total_fatalities >= 1000 & yearly_total_fatalities < 10000 ~ "Medium",
      yearly_total_fatalities < 1000 ~ "Low"
    ),
    high_conflict = ifelse(conflict_intensity == "High", 1, 0)  # binary for DiD
  )


# --- CHECK DISTRIBUTION ---
table(merged_data$conflict_intensity, useNA = "ifany")

# --- DID MODEL USING HIGH CONFLICT INTENSITY ---
merged_data <- merged_data %>%
  mutate(fatalities_1000 = yearly_total_fatalities / 1000)

did_intensity <- feols(
  support.for.democracy ~ fatalities_1000 * post_2014,
  data = merged_data
)

summary(did_intensity)

# --- CREATE INTENSITY TABLE (TO DISPLAY IN DISSERTATION) ---
intensity_table <- merged_data %>%
  group_by(conflict_intensity) %>%
  summarise(
    avg_fatalities = mean(yearly_total_fatalities, na.rm = TRUE),
    count = n()
  )

intensity_table



#heterogeneity analysis

library(fixest)
library(dplyr)
library(broom)

# --- FUNCTION TO RUN HETEROGENEITY MODEL AND RETURN RESULTS ---
library(fixest)
library(broom)

# === EDUCATION (secondary or more vs less than secondary) ===
edu_model <- feols(
  support.for.democracy ~ treated * post_2014 * (education >= 5) | state + year,
  cluster = ~state,
  data = merged_data
)
summary(edu_model)

# === GENDER (female vs male) ===
gender_model <- feols(
  support.for.democracy ~ treated * post_2014 * gender | state + year,
  cluster = ~state,
  data = merged_data
)
summary(gender_model)

# === URBAN (urban vs rural) ===
urban_model <- feols(
  support.for.democracy ~ treated * post_2014 * Urban.Rural | state + year,
  cluster = ~state,
  data = merged_data
)
summary(urban_model)

# === EMPLOYMENT (employed vs unemployed) ===
employment_model <- feols(
  support.for.democracy ~ treated * post_2014 * employment | state + year,
  cluster = ~state,
  data = merged_data
)
summary(employment_model)

# === AGE GROUPS (youth vs. older adults) ===
# First, create an age group variable
merged_data <- merged_data %>%
  mutate(age_group = case_when(
    age < 30 ~ "Youth",
    age >= 30 & age < 50 ~ "Middle",
    TRUE ~ "Older"
  ))

age_model <- feols(
  support.for.democracy ~ treated * post_2014 * age_group | state + year,
  cluster = ~state,
  data = merged_data
)
summary(age_model)

##combining for heteorgeneity analysis

library(modelsummary)
modelsummary(
  list("Education" = edu_model,
       "Gender" = gender_model,
       "Urban/Rural" = urban_model,
       "Employment" = employment_model,
       "Age Groups" = age_model),
  stars = TRUE,
  gof_omit = 'AIC|BIC|Log.Lik',
  title = "Table X: Heterogeneity of Conflict Exposure Effects on Democratic Support"
)
