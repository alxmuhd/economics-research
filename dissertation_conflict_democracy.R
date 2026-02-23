# ============================================================
# PROJECT: Conflict and Support for Democracy in Nigeria
# DATA:    Afrobarometer Rounds 1–9 (1999–2022) + ACLED
# METHOD:  Difference-in-Differences (Two-Way Fixed Effects)
# ============================================================


# ============================================================
# SECTION 0: SETUP
# ============================================================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(haven)
library(tidyr)
library(dplyr)
library(fixest)
library(modelsummary)
library(knitr)

rm(list = ls())


# ============================================================
# SECTION 1: DATA LOADING
# ============================================================

# --- Afrobarometer survey rounds ---
df_1999 <- read_sav("~/Desktop/Dissertation /Nigeria Round 1 data (1999)")
df_2001 <- read_sav("~/Desktop/Dissertation /Nigeria Round 1.5 data (2001)")
df_2003 <- read_sav("~/Desktop/Dissertation /Nigeria Round 2 data (2003)")
df_2005 <- read_sav("~/Desktop/Dissertation /Nigeria Round 3 data (2005)")
df_2008 <- read_sav("~/Desktop/Dissertation /Nigeria Round 4 data (2008)")
df_2012 <- read_sav("~/Desktop/Dissertation /Nigeria Round 5 data (2013)")
df_2015 <- read_sav("~/Desktop/Dissertation /Nigeria Round 6 data (2015)")
df_2018 <- read_sav("~/Desktop/Dissertation /Nigeria Round 7 data (2017)")
df_2021 <- read_sav("~/Desktop/Dissertation /Nigeria Round 8 data (2021)")
df_2022 <- read_sav("~/Desktop/Dissertation /Nigeria Round 9 data (2022)")

# --- ACLED conflict data ---
crime <- read.csv("~/Desktop/Dissertation /Africa_1997-2025_Jan31.csv")


# ============================================================
# SECTION 2: VARIABLE MAPPING DICTIONARY
# ============================================================

column_mappings <- list(
  "1999" = c("ID" = "RESPNO",  "gender" = "GENDER",  "age" = "Q1",   "education" = "Q4",
             "employment" = "Q9",   "state" = "Q101",  "Urban/Rural" = "Q102",
             "support for democracy" = "Q36"),
  "2001" = c("ID" = "respno",  "gender" = "q96",    "age" = "q80",  "education" = "q84",
             "employment" = "q89",  "state" = "state", "Urban/Rural" = "urbrur",
             "support for democracy" = "supdemx1"),
  "2003" = c("ID" = "respno",  "gender" = "q96",    "age" = "q80",  "education" = "q84",
             "employment" = "q89",  "state" = "regon2","Urban/Rural" = "urbrur",
             "support for democracy" = "q38"),
  "2005" = c("ID" = "RESPNO",  "gender" = "Q101",   "age" = "Q1",   "education" = "Q90",
             "employment" = "Q94",  "state" = "REGION","Urban/Rural" = "URBRUR",
             "support for democracy" = "Q37"),
  "2008" = c("ID" = "RESPNO",  "gender" = "Q101",   "age" = "Q1",   "education" = "Q89",
             "employment" = "Q94",  "state" = "REGION","Urban/Rural" = "URBRUR",
             "support for democracy" = "Q30"),
  "2012" = c("ID" = "RESPNO",  "gender" = "Q101",   "age" = "Q113", "education" = "Q117",
             "employment" = "Q96",  "state" = "REGION","Urban/Rural" = "URBRUR",
             "support for democracy" = "Q32"),
  "2015" = c("ID" = "RESPNO",  "gender" = "Q101",   "age" = "Q1",   "education" = "Q97",
             "employment" = "Q95A", "state" = "REGION","Urban/Rural" = "URBRUR",
             "support for democracy" = "Q21"),
  "2018" = c("ID" = "RESPNO",  "gender" = "Q101",   "age" = "Q1",   "education" = "Q97",
             "employment" = "Q94",  "state" = "REGION","Urban/Rural" = "URBRUR",
             "support for democracy" = "Q28"),
  "2021" = c("ID" = "RESPNO",  "gender" = "Q101",   "age" = "Q1",   "education" = "Q97",
             "employment" = "Q95A", "state" = "REGION","Urban/Rural" = "URBRUR",
             "support for democracy" = "Q21"),
  "2022" = c("ID" = "RESPNO",  "gender" = "Q101",   "age" = "Q1",   "education" = "Q94",
             "employment" = "Q93A", "state" = "REGION","Urban/Rural" = "URBRUR",
             "support for democracy" = "Q23")
)


# ============================================================
# SECTION 3: SURVEY DATA CLEANING & STANDARDISATION
# ============================================================

# --- Function: rename columns to standard names & tag year ---
rename_correctly <- function(df, year) {
  mapping <- column_mappings[[as.character(year)]]
  if (!is.null(mapping)) df <- df %>% rename(all_of(mapping))
  df %>% mutate(year = year)
}

# --- Function: keep only the 9 harmonised variables ---
select_relevant <- function(df) {
  df %>% select("ID", "support for democracy", "Urban/Rural",
                "age", "gender", "education", "employment", "state", "year")
}

# Apply both functions to each wave
df_list_raw <- list(
  list(df_1999, 1999), list(df_2001, 2001), list(df_2003, 2003),
  list(df_2005, 2005), list(df_2008, 2008), list(df_2012, 2012),
  list(df_2015, 2015), list(df_2018, 2018), list(df_2021, 2021),
  list(df_2022, 2022)
)

processed <- lapply(df_list_raw, function(x) select_relevant(rename_correctly(x[[1]], x[[2]])))
names(processed) <- c("df_1999","df_2001","df_2003","df_2005","df_2008",
                       "df_2012","df_2015","df_2018","df_2021","df_2022")
list2env(processed, envir = .GlobalEnv)


# --- Standardise state labels ---

state_labels_1_38 <- c(
  "Abia", "Adamawa", "Akwa-Ibom", "Anambra", "Bauchi", "Bayelsa", "Benue", "Borno",
  "Cross River", "Delta", "Ebonyi", "Edo", "Ekiti", "Enugu", "FCT", "Gombe", "Imo",
  "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Kogi", "Kwara", "Lagos",
  "Nasarawa", "Niger", "Ogun", "Ondo", "Osun", "Oyo", "Plateau", "Rivers",
  "Sokoto", "Taraba", "Yobe", "Zamfara", "No Answer"
)

state_labels_620_656 <- c(
  "Abia", "Adamawa", "Akwa-Ibom", "Anambra", "Bauchi", "Bayelsa", "Benue", "Borno",
  "Cross-River", "Delta", "Ebonyi", "Edo", "Ekiti", "Enugu", "FCT", "Gombe", "Imo",
  "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Kogi", "Kwara", "Lagos", "Nasarawa",
  "Niger", "Ogun", "Ondo", "Osun", "Oyo", "Plateau", "Rivers", "Sokoto", "Taraba",
  "Yobe", "Zamfara"
)

df_1999$state <- factor(df_1999$state, levels = 1:38, labels = state_labels_1_38)
df_2001$state <- factor(df_2001$state, levels = 1:38, labels = state_labels_1_38)

df_2005$state <- factor(df_2005$state,
  levels = c(340:359, 550:560),
  labels = c("Lagos","Ogun","Oyo","Osun","Ondo","Ekiti","Enugu","Anambra","Imo",
             "Abia","Akwa-Ibom","Bayelsa","Cross-River","Delta","Edo","Rivers","Kano",
             "Sokoto","Kaduna","Katsina","Zamfara","Bauchi","Borno","Adamawa",
             "Taraba","Plateau","Benue","Kogi","Kwara","Niger","FCT"))

df_2008$state <- factor(df_2008$state,
  levels = 620:645,
  labels = c("Abia","Adamawa","Akwa Ibom","Anambra","Bauchi","Bayelsa","Benue","Borno",
             "Cross River","Delta","Ebonyi","Edo","Ekiti","Enugu","FCT","Gombe","Imo",
             "Jigawa","Kaduna","Kano","Katsina","Kebbi","Kogi","Kwara","Lagos","Nasarawa"))

for (df_name in c("df_2012","df_2015","df_2018","df_2021","df_2022")) {
  df <- get(df_name)
  df$state <- factor(df$state, levels = 620:656, labels = state_labels_620_656)
  assign(df_name, df)
}

# --- Recode 1999 democracy variable (scale was reversed) ---
df_1999 <- df_1999 %>%
  mutate(`support for democracy` = case_when(
    `support for democracy` == 1 ~ 3,
    `support for democracy` == 3 ~ 1,
    `support for democracy` == 2 ~ 2,
    TRUE ~ NA_real_
  ))

# --- Combine all waves ---
survey <- bind_rows(lapply(
  list(df_1999,df_2001,df_2003,df_2005,df_2008,df_2012,df_2015,df_2018,df_2021,df_2022),
  function(df) df %>% mutate(across(everything(), as.character))
))

# --- Filter missing / refused / don't know ---
survey <- survey %>%
  filter(
    !`support for democracy` %in% c(NA, "8", "9"),
    !age       %in% c("999", "998"),
    !employment %in% c("9", "8", "-1", "998"),
    !education  %in% c("10", "99", "-1", "98"),
    !state      %in% c("NA")
  )

# --- Fix Urban/Rural coding in 2008 (3-point → 2-point scale) ---
survey <- survey %>%
  mutate(`Urban/Rural` = case_when(
    year == "2008" & `Urban/Rural` == "2" ~ "1",
    year == "2008" & `Urban/Rural` == "3" ~ "2",
    TRUE ~ `Urban/Rural`
  ))

# --- Harmonise education: "1" (informal) → "0" (no education) ---
survey <- survey %>%
  mutate(education = ifelse(education == "1", "0", education))

# --- Fix 1999 employment: codes were flipped, and category 2 is dropped ---
survey <- survey %>%
  filter(!(year == "1999" & employment == "2")) %>%
  mutate(employment = ifelse(year == "1999" & employment == "0", "1",
                      ifelse(year == "1999" & employment == "1", "0", employment)))

# --- Collapse employment into binary: 0 = not working, 1 = working ---
survey <- survey %>%
  mutate(employment = case_when(
    year %in% c("2001","2003","2005","2008","2012","2015","2018","2021","2022") &
      employment %in% c("2","3","4","5") ~ "1",
    year %in% c("2001","2003","2005","2008","2012","2015","2018","2021","2022") &
      employment %in% c("0","1") ~ "0",
    TRUE ~ employment
  ))

# --- Standardise state name variants ---
survey <- survey %>%
  mutate(state = case_when(
    state == "Abuja"       ~ "FCT",
    state == "Akwa Ibom"  ~ "Akwa-Ibom",
    state == "Cross River" ~ "Cross-River",
    TRUE ~ state
  )) %>%
  filter(state != "NA")

# --- Convert types ---
survey <- survey %>%
  mutate(
    ID                      = as.numeric(gsub("[^0-9]", "", ID)),
    `support for democracy` = as.integer(`support for democracy`),
    `Urban/Rural`           = as.factor(`Urban/Rural`),
    age                     = as.integer(age),
    gender                  = as.factor(gender),
    education               = as.factor(education),
    employment              = as.factor(employment),
    state                   = as.factor(state),
    year                    = as.integer(year)
  )

write.csv(survey, file = "~/Desktop/Dissertation /survey.csv", row.names = FALSE)


# ============================================================
# SECTION 4: CRIME DATA PREPARATION
# ============================================================

crime <- crime %>%
  filter(country == "Nigeria", year >= 1999, year <= 2022, state != "") %>%
  select(state = admin1, year, event_type, disorder_type, fatalities)

crime_agg <- crime %>%
  group_by(state, year) %>%
  summarise(
    total_fatalities                      = sum(fatalities, na.rm = TRUE),
    total_events                          = n(),
    Battles_count                         = sum(event_type == "Battles"),
    Riots_count                           = sum(event_type == "Riots"),
    Protests_count                        = sum(event_type == "Protests"),
    Violence_against_civilians_count      = sum(event_type == "Violence against civilians"),
    Explosions_Remote_violence_count      = sum(event_type == "Explosions/Remote violence"),
    Strategic_developments_count         = sum(event_type == "Strategic developments"),
    Fatalities_Battles                    = sum(fatalities * (event_type == "Battles"),   na.rm = TRUE),
    Fatalities_Riots                      = sum(fatalities * (event_type == "Riots"),     na.rm = TRUE),
    Fatalities_Protests                   = sum(fatalities * (event_type == "Protests"),  na.rm = TRUE),
    Fatalities_Violence_against_civilians = sum(fatalities * (event_type == "Violence against civilians"), na.rm = TRUE),
    Fatalities_Explosions_Remote_violence = sum(fatalities * (event_type == "Explosions/Remote violence"), na.rm = TRUE),
    Fatalities_Strategic_developments    = sum(fatalities * (event_type == "Strategic developments"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    crime %>%
      group_by(year) %>%
      summarise(yearly_total_fatalities = sum(fatalities, na.rm = TRUE),
                yearly_total_events     = n(), .groups = "drop"),
    by = "year"
  )

write.csv(crime_agg, file = "crime_agg.csv", row.names = FALSE)


# ============================================================
# SECTION 5: GEOGRAPHIC DATA & FINAL DATASET ASSEMBLY
# ============================================================

nigeria_states <- ne_states(country = "Nigeria", returnclass = "sf") %>%
  mutate(name = recode(name,
    "Akwa Ibom"                 = "Akwa-Ibom",
    "Cross River"               = "Cross-River",
    "Federal Capital Territory" = "FCT")) %>%
  rename(state = name) %>%
  select(state, longitude, latitude)

survey <- survey %>%
  group_by(state, year) %>%
  mutate(
    total_response       = n(),
    democracy_supporters = sum(support.for.democracy == 3),
    support_percentage   = democracy_supporters / total_response * 100
  ) %>%
  ungroup() %>%
  left_join(nigeria_states, by = "state")

final_dataset <- survey %>%
  left_join(crime_agg, by = c("state", "year"))

write.csv(final_dataset, file = "final_dataset.csv", row.names = FALSE)


# ============================================================
# SECTION 6: EXPLORATORY VISUALISATIONS
# ============================================================

# --- Fatalities over time by event type ---
crime %>%
  group_by(year, event_type) %>%
  summarise(total_fatalities = sum(fatalities), .groups = "drop") %>%
  ggplot(aes(x = year, y = total_fatalities, colour = event_type)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1999, 2022, by = 3)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Fatalities by Event Type in Nigeria (1999–2022)",
       x = "Year", y = "Number of Fatalities", colour = "Event Type") +
  theme_minimal() + theme(legend.position = "bottom")

# --- Define treatment for all plots & models ---
merged_data <- final_dataset %>%
  mutate(
    treated   = ifelse(state %in% c("Borno","Yobe","Adamawa","Bauchi","Gombe","Taraba"), 1, 0),
    post_2014 = ifelse(year >= 2014, 1, 0),
    region    = ifelse(treated == 1, "North East (Treated)", "Other (Control)")
  )

# --- Pre-2014 parallel trends ---
merged_data %>%
  filter(year < 2014) %>%
  group_by(region, year) %>%
  summarise(avg_support = mean(support.for.democracy, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = avg_support, color = region)) +
  geom_line(linewidth = 1) + geom_point() +
  scale_color_manual(values = c("North East (Treated)" = "red", "Other (Control)" = "blue")) +
  labs(title = "Pre-Trend Check: Support for Democracy (Pre-2014)",
       y = "Average Support (1–3)", x = "Year", color = "Region") +
  theme_minimal()

# --- Full timeline parallel trends (2005 onwards) ---
merged_data %>%
  group_by(region, year) %>%
  summarise(avg_support = mean(support.for.democracy, na.rm = TRUE),
            se = sd(support.for.democracy, na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
  filter(year >= 2005) %>%
  ggplot(aes(x = year, y = avg_support, color = region)) +
  geom_line(linewidth = 1) + geom_point() +
  geom_errorbar(aes(ymin = avg_support - 1.96*se, ymax = avg_support + 1.96*se),
                width = 0.2, alpha = 0.5) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 2015, y = 2.8, label = "Conflict Escalation (2014)",
           hjust = 0, color = "red") +
  scale_color_manual(values = c("North East (Treated)" = "#E41A1C", "Other (Control)" = "#377EB8")) +
  labs(title = "Support for Democracy: North East vs. Other Regions (2005–2022)",
       y = "Average Support (1–3 Scale)", x = "Year", color = "Region") +
  theme_minimal() + theme(legend.position = "bottom")


# ============================================================
# SECTION 7: MAIN ECONOMETRIC ANALYSIS
# ============================================================

filtered_data <- merged_data %>%
  filter(year >= 2005) %>%
  mutate(year = as.factor(year))

# --- Main DiD model: two-way FE, clustered SEs ---
did_model <- feols(
  support.for.democracy ~ treated * post_2014 | state + year,
  data = filtered_data, cluster = ~state
)
summary(did_model)

# --- Event study: treatment effects by year relative to 2012 ---
event_model <- feols(
  support.for.democracy ~ i(year, treated, ref = "2012") | state + year,
  data = filtered_data, cluster = ~state
)
iplot(event_model,
      main = "Event Study: Conflict Exposure and Support for Democracy",
      xlab = "Year", ylab = "Effect (relative to 2012)", col = "darkred")
abline(h = 0, lty = 2)


# ============================================================
# SECTION 8: ROBUSTNESS CHECKS
# ============================================================

# Restore numeric year for robustness manipulations
merged_data <- merged_data %>% mutate(year = as.numeric(as.character(year)))

# --- Placebo 1: false treatment onset at 2008 ---
placebo_2008 <- feols(
  support.for.democracy ~ treated * I(year >= 2008) | state + year,
  data = merged_data, cluster = ~state
)
summary(placebo_2008)

# --- Placebo 2: false treatment onset at 2012 ---
placebo_2012 <- feols(
  support.for.democracy ~ treated * I(year >= 2012) | state + year,
  data = merged_data, cluster = ~state
)
summary(placebo_2012)

# --- Alternative clustering: geopolitical zone ---
merged_data <- merged_data %>%
  mutate(zone = case_when(
    state %in% c("Borno","Yobe","Adamawa","Bauchi","Gombe","Taraba")           ~ "North East",
    state %in% c("Jigawa","Kaduna","Kano","Katsina","Kebbi","Sokoto","Zamfara") ~ "North West",
    state %in% c("Benue","Kogi","Kwara","Nasarawa","Niger","Plateau","FCT")    ~ "North Central",
    state %in% c("Abia","Anambra","Ebonyi","Enugu","Imo")                      ~ "South East",
    state %in% c("Akwa-Ibom","Bayelsa","Cross-River","Delta","Edo","Rivers")   ~ "South South",
    state %in% c("Ekiti","Lagos","Ogun","Ondo","Osun","Oyo")                   ~ "South West"
  ))

did_cluster_zone <- feols(
  support.for.democracy ~ treated * post_2014 | state + year,
  data = merged_data, cluster = ~zone
)
summary(did_cluster_zone)

# --- Restricted sample: drop borderline states ---
did_restricted <- feols(
  support.for.democracy ~ treated * post_2014 | state + year,
  data    = merged_data %>% filter(!state %in% c("Bauchi","Taraba")),
  cluster = ~state
)
summary(did_restricted)

# --- Intensity-based treatment: fatalities per 1,000 ---
merged_data <- merged_data %>% mutate(fatalities_1000 = yearly_total_fatalities / 1000)
did_intensity <- feols(
  support.for.democracy ~ fatalities_1000 * post_2014,
  data = merged_data
)
summary(did_intensity)


# ============================================================
# SECTION 9: HETEROGENEITY ANALYSIS
# ============================================================

edu_model <- feols(
  support.for.democracy ~ treated * post_2014 * I(as.numeric(as.character(education)) >= 5) | state + year,
  cluster = ~state, data = merged_data
)

gender_model <- feols(
  support.for.democracy ~ treated * post_2014 * gender | state + year,
  cluster = ~state, data = merged_data
)

urban_model <- feols(
  support.for.democracy ~ treated * post_2014 * Urban.Rural | state + year,
  cluster = ~state, data = merged_data
)

employment_model <- feols(
  support.for.democracy ~ treated * post_2014 * employment | state + year,
  cluster = ~state, data = merged_data
)

merged_data <- merged_data %>%
  mutate(age_group = case_when(
    age < 30             ~ "Youth",
    age >= 30 & age < 50 ~ "Middle",
    TRUE                 ~ "Older"
  ))

age_model <- feols(
  support.for.democracy ~ treated * post_2014 * age_group | state + year,
  cluster = ~state, data = merged_data
)

modelsummary(
  list("Education" = edu_model, "Gender" = gender_model,
       "Urban/Rural" = urban_model, "Employment" = employment_model,
       "Age Groups" = age_model),
  stars    = TRUE,
  gof_omit = "AIC|BIC|Log.Lik",
  title    = "Table: Heterogeneity of Conflict Exposure Effects on Democratic Support"
)
