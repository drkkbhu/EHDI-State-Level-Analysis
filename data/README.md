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

# Analysis Data

This folder contains the two analytic datasets used in the study. Both
datasets were constructed from publicly available data sources using
the data preparation and processing procedures documented in the R
analysis scripts in this repository.

The datasets contain state-year aggregated observations and do not
contain personally identifiable information (PII) or protected health
information (PHI).

## 1. Primary Analysis Dataset

**File:** `analysis_data_v3_EIrd_2009to2022_4282026.csv`

This dataset was used for the primary analyses and includes state-year
observations from 2009 through 2022.

States were excluded if they had more than one missing year for any of
the following variables:

- Hearing loss prevalence (HLp)
- Early intervention rate (EIr)
- Early intervention rate based on diagnosed infants (EIrd)
- Screening rate (PerTscr)
- Dr

The primary analytic dataset therefore includes states with no more
than one missing year for each of these variables during the
2009-2022 study period.

The variable `EIrd` was calculated as:

EIrd = (EI / TDia) × 1,000

where EI represents the number of infants receiving early intervention
and TDia represents the number of infants diagnosed.

## 2. Complete-Case Sensitivity Analysis Dataset

**File:** `analysis_data_complete_case_sensitivity_with_EIrd.csv`

This dataset was created for the complete-case sensitivity analysis.

The year 2020 was excluded from the sensitivity analysis. The analysis
therefore included the following 13 years:

- 2009-2019
- 2021-2022

Only states with complete data for all study variables across all 13
years were included.

The variables required for complete-case inclusion were:

- Hearing loss prevalence (HLp)
- Early intervention rate (EIr)
- Early intervention rate based on diagnosed infants (EIrd)
- Screening rate (PerTscr)
- Dr

## Data Construction

The analytic datasets were derived from publicly available data sources.
The data preparation, variable construction, state exclusion criteria,
and analytic dataset creation are documented in the R scripts contained
in the `R/` directory.

## Reproducibility

The datasets provided here represent the analytic datasets used in the
study and are intended to facilitate reproduction of the reported
analyses.

For details regarding the statistical analyses, see the R scripts in
the `R/` directory.
