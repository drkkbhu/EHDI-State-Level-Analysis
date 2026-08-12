# EHDI-State-Level-Analysis
SAS and R code for the analysis of state-level predictors of early hearing detection and intervention outcomes.

## Overview

This repository contains the SAS and R code used for data cleaning, statistical analysis, and figure generation for a research study titled: State-level factors associated with infant hearing loss detection and intervention indicators in the United States.

## Authors

Dr. Keshav Kumar

## Study Description

This study examined state-level predictors of early hearing detection and intervention outcomes using longitudinal state-year data.

## Repository Contents

- `code/` - SAS and R analysis scripts
- `figures/` - Figures generated for the manuscript
- `results/` - Supporting analysis outputs
- `documentation/` - Variable definitions and analysis documentation

## Data Availability
The data used in this study are publicly available from:

The primary data source was the CDC EHDI Hearing Screening and Follow-up Survey (HSFS), 
which provides annual state-level counts of infants screened, diagnosed, and enrolled in early intervention, 
compiled and made publicly available on the EHDI Data Hub Website.
Socioeconomic covariates were drawn from the American Community Survey (ACS) demographic data.
Kaiser Family Foundation (KFF) maternal health indicators.
State Health Access Data Assistance Center (SHADAC) public health funding datasets.
CDC Breastfeeding Report Card.

Access date & Data source:
Accessed December 28, 2025. https://ehdidata.wustl.edu/faq/
Accessed December 28, 2025. https://www.socialexplorer.com/reports/socialexplorer/en/report/9b98abea-dc7a-11f0-bd73-5bc6cdf36552 
Accessed January 4, 2026. https://shadac.org/
Accessed December 28, 2025. https://www.kff.org/state-health-policy-data/state-indicator/breastfeeding-rates/
Accessed April 20, 2026. https://www.cdc.gov/breastfeeding-data/about/index.html

Complete Reference from paper:
FAQ | Early Hearing Detection and Intervention Data Hub | Washington University in St. Louis. Accessed December 28, 2025. https://ehdidata.wustl.edu/faq/
SocialExplorer Reports. Accessed December 28, 2025. https://www.socialexplorer.com/reports/socialexplorer/en/report/9b98abea-dc7a-11f0-bd73-5bc6cdf36552
Percentage of Infants who were Breastfed | KFF State Health Facts. KFF. Accessed December 28, 2025. https://www.kff.org/state-health-policy-data/state-indicator/breastfeeding-rates/
SHADAC analysis of Trust for America's Health (TFAH). Shortchanging America's Health; Investing in America's Health; The Impact of Chronic Underfunding on America's Public Health System. State Health Compare, State Health Access Data Assistance Center (SHADAC), University of Minnesota. statehealthcompare.shadac.org. Accessed January 4, 2026. https://shadac.org/
CDC. About Breastfeeding Data. Breastfeeding Data. July 11, 2025. Accessed April 20, 2026. https://www.cdc.gov/breastfeeding-data/about/index.html

The dataset was downloaded and prepared for analysis as described in
the R scripts contained in this repository.

The data used in this study are publicly available and contain no
personally identifiable information (PII) or protected health
information (PHI).

## Software

Analyses were conducted using SAS 8.6 and R 4.2.3

## Reproducibility

The analysis scripts are organized according to the analytical workflow:

1. Data cleaning
2. Variable construction
3. Descriptive analyses
4. Statistical modeling
5. Figure generation
6. Sensitivity analyses

## Citation

Please cite this repository as:


## Contact

Corresponding author: Keshav Kumar, k.keshav@wustl.edu
