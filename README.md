# economics-research
Replication packages for causal inference and applied microeconomics projects in development, labour, and health economics.

Projects
1. The Cognitive Cost of Furlough (UKHLS, IV/2SLS)

Description: Estimates the causal impact of the UK furlough scheme on cognitive mental health outcomes using UK Household Longitudinal Study COVID panel data. Identification is based on an instrumental-variables strategy exploiting pre-pandemic work-from-home feasibility.
Methods: Instrumental Variables (2SLS), panel data methods, robustness and heterogeneity analysis.
Data: UKHLS COVID-19 Study (restricted access).
Status: Replication files and synthetic data only (due to data licensing restrictions).

2. Conflict Exposure and Democratic Attitudes in Nigeria

Description: Examines the causal effect of sustained exposure to violent conflict on democratic attitudes using Afrobarometer survey data merged with geocoded conflict events from ACLED.
Methods: Difference-in-Differences, Event Study, fixed effects.
Data: Afrobarometer (public), ACLED (public).
Status: Replication code and documentation available.

3. GIS Portfolio — Spatial Analysis in R
Description: Demonstrates applied spatial data analysis across seven GIS workflows using R. Covers vector and raster data handling, coordinate reference systems, spatial intersection, digital elevation modelling, satellite imagery processing, and population density mapping. Data sources include GADM, OpenStreetMap, Natural Earth, AWS Terrain Tiles, Sentinel-2 (ESA), and WorldPop. Output: reproducible R Markdown document rendered to PDF.

## Data Sources
### 1. Afrobarometer (Nigeria, Rounds 1–9)
Public opinion survey data used to measure democratic attitudes and political trust.
Codebooks and documentation available at:
https://www.afrobarometer.org/data/

### 2. ACLED (Armed Conflict Location & Event Data Project)
Conflict event data used to measure exposure to Boko Haram violence.
Documentation and access information:
https://acleddata.com/

### 3. UK Household Longitudinal Study (UKHLS) – COVID-19 Waves
Panel data used to analyse the effects of furlough on cognitive mental health.
Study documentation available at:
https://www.understandingsociety.ac.uk/

## Replication Instructions
1. Download Afrobarometer data (Rounds 1–9) from the official website.
2. Register and download ACLED conflict event data.
3. Access UKHLS COVID waves via UK Data Service.
4. Update file paths in the scripts before running.
