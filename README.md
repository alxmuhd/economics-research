# economics-research
Replication packages for applied microeconomics and macroeconomic time-series analysis, covering causal inference, VAR modelling, and policy-relevant research in development, labour, health, and inflation dynamics.

Projects

1. UK Phillips Curve – Inflation Dynamics and VAR Analysis (2008–2025) - Macroeconomic analysis
   
Description: Investigates whether the Phillips curve relationship between unemployment and inflation remains active in the UK using quarterly ONS data from 2008–2025. Examines baseline, expectations-augmented, structural break, and dynamic specifications to assess time variation in the inflation–labour market trade-off.

Methods: OLS regression, expectations-augmented Phillips curve (lagged inflation), structural break analysis (pre/post COVID), rolling regression (20-quarter window), Vector Autoregression (VAR), Impulse Response Functions (IRF), Forecast Error Variance Decomposition (FEVD), Newey-West HAC corrections.

Data: ONS CPIH (inflation), ONS quarterly unemployment rate, ONS Output per Hour Worked (productivity).

Status: Full replication code and reproducible R Markdown document available.

2. The Cognitive Cost of Furlough (UKHLS, IV/2SLS)

Description: Estimates the causal impact of the UK furlough scheme on cognitive mental health outcomes using UK Household Longitudinal Study COVID panel data. Identification is based on an instrumental-variables strategy exploiting pre-pandemic work-from-home feasibility.
Methods: Instrumental Variables (2SLS), panel data methods, robustness and heterogeneity analysis.
Data: UKHLS COVID-19 Study (restricted access).
Status: Replication files and synthetic data only (due to data licensing restrictions).

3. Conflict Exposure and Democratic Attitudes in Nigeria

Description: Examines the causal effect of sustained exposure to violent conflict on democratic attitudes using Afrobarometer survey data merged with geocoded conflict events from ACLED.
Methods: Difference-in-Differences, Event Study, fixed effects.
Data: Afrobarometer (public), ACLED (public).
Status: Replication code and documentation available.

4. GIS Portfolio — Spatial Analysis in R
   
Description: Demonstrates applied spatial data analysis across seven GIS workflows using R. Covers vector and raster data handling, coordinate reference systems, spatial intersection, digital elevation modelling, satellite imagery processing, and population density mapping. Data sources include GADM, OpenStreetMap, Natural Earth, AWS Terrain Tiles, Sentinel-2 (ESA), and WorldPop. Output: reproducible R Markdown document rendered to PDF.

## Data Sources
### 1. UK Office for National Statistics (ONS) – Macroeconomic Data
Quarterly and monthly macroeconomic data used to estimate the UK Phillips curve and VAR model, including:
• CPIH (Consumer Prices Index including owner occupiers’ housing costs) – used to construct year-on-year inflation  
• Quarterly unemployment rate – used as the labour market slack measure  
• Output per Hour Worked (Table 18) – used for productivity context  

Documentation and access available at:  
https://www.ons.gov.uk/
### 2. Afrobarometer (Nigeria, Rounds 1–9)
Public opinion survey data used to measure democratic attitudes and political trust.
Codebooks and documentation available at:
https://www.afrobarometer.org/data/

### 3. ACLED (Armed Conflict Location & Event Data Project)
Conflict event data used to measure exposure to Boko Haram violence.
Documentation and access information:
https://acleddata.com/

### 4. UK Household Longitudinal Study (UKHLS) – COVID-19 Waves
Panel data used to analyse the effects of furlough on cognitive mental health.
Study documentation available at:
https://www.understandingsociety.ac.uk/ ```

## Replication Instructions
1. All data are publicly available from the UK Office for National Statistics (ONS). No restricted-access datasets are used in this project.
2. Download Afrobarometer data (Rounds 1–9) from the official website.
3. Register and download ACLED conflict event data.
4. Access UKHLS COVID waves via UK Data Service.
5. Update file paths in the scripts before running.
