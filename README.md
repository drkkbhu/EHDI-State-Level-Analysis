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

## Data

The analytic datasets used in this study were constructed from publicly
available data sources and contain state-year aggregated observations.
No personally identifiable information (PII) or protected health
information (PHI) is included.

Two analytic datasets are provided:

### Primary analysis

`data/analysis_data_v3_EIrd_2009to2022_4282026.csv`

The primary analysis dataset covers 2009-2022 and includes states with
no more than one missing year for each required analysis variable.

### Sensitivity analysis

`data/analysis_data_complete_case_sensitivity_with_EIrd.csv`

The complete-case sensitivity dataset excludes 2020 and includes only
states with complete data for all required analysis variables across
the 13 study years from 2009-2019 and 2021-2022.

See `data/README.md` for detailed information about dataset
construction and inclusion criteria.

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
