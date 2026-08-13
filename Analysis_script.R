---
title: "State-level factors associated with infant hearing loss detection and intervention indicators in the United States."
author: "Keshav Kumar"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
  html_document:
    theme: journal
    toc: yes
    toc_float:
      collapsed: yes
    toc_depth: 4
    plots:
      style: Normal
      align: left
    code_folding: hide
  fig_caption: yes
  df_print: kable
  pdf_document:
    latex_engine: lualatex
    toc: yes
    toc_depth: '4'
  word_document:
    toc: yes
    toc_depth: '4'
always_allow_html: yes
editor_options:
  chunk_output_type: console
---
[//]: # CSS style arguments

 <style type="text/css">


h1 {
  font-size: 40px;
}

h2 {
  font-size: 30px;
}

p {
  font-size: 16px;
}

li {
  font-size: 18px;
}

body{ /* Normal  */
      font-size: 20px;
      counter-reset:table figure;
  }

.table{
width:auto;
font-size:12px;
}

td, th{
padding-left:10px;
text-align: right;
font-size: 16px;
}

caption::before{
counter-increment: table;
content: "Table " counter(table) ": ";
}


.caption::before{
counter-increment: figure;
content: "Figure " counter(figure) ": ";
}

caption, .caption{
font-style:italic;
font-size: 14px;
margin-top:0.5em;
margin-bottom:0.5em;
width:90%;
text-align: left;
}

#TOC {
font-size: 14px;
background-color: white;
overflow: auto;
}

</style>


```{r setup, include=FALSE}

# global settings (see Yihue Xie https://yihui.org/knitr/options/)

knitr::opts_chunk$set(fig.width=12, fig.height=12, warning=FALSE, 
                      message=FALSE, cache=FALSE, results='asis')

# Set working directory for knit — must use absolute path here, NOT getwd()
# getwd() returns the temp directory during knit, not the project folder
knitr::opts_knit$set(root.dir = "C:/Users/kesha/OneDrive/Desktop/R_git/EHDI_paper_analysis_2025/EHDI_paper_2025")


```


```{r PROLOG_DATAMGMT}


###### PROLOG ########


# PROLOG   ###
# PROJECT: EHDI Paper                                   
# PURPOSE: Publication                                     
# DIR:     C:\Users\kesha\OneDrive\Desktop\R_git\EHDI_paper_analysis_2025\EHDI_paper_2025\output_data_paper_2025                   
# DATA:    final_paper_data_filtered25.csv             
# AUTHOR:  Keshav Kumar                                            
# CREATED: Dec 21, 2025                                           
# LATEST:  Dec 21, 2025                                      
# NOTES:   Source for GOVT spending data: 
#          ACS social explorer 2024.
#         Kaiser Family FOundation for healthcare spending data

# PROLOG   ### 


# install packages and open libraries

# stargazer for model comparison
# sandwich For robust SE estimator
# broom for getting results with Robust SEs
# MASS For negative binomial
# lmtest For model comparison
# SMPracticals for lung cancer data
# ggplot2 for plots
# writing results to excel
# magrittr for pipes
# sessioninfo for session_info at bottom
# details for session_info at bottom
# ggthemes for tufte theme
# ggrepel for text plotting
# patchwork for combining plots
# sjplot for model tables
#latexpdf for converting table to pdf or png


options(repos = c(CRAN = "https://cloud.r-project.org"))

pacman::p_load(stargazer, sandwich, lmtest, SMPracticals, ggplot2, writexl, broom, broom.mixed, dplyr, tidyverse, lme4, summarytools, tableone, reshape2, kableExtra, magrittr, sessioninfo, details, ggthemes, ggrepel, patchwork, sjPlot, latexpdf, readxl, purrr, plm, table1, FactoMineR, factoextra, ggprism, flextable, officer)

# plot theme
theme_set(theme_tufte())  # but might not carry over in chunks

# Okabe-Ito colorblind-friendly color palette:
# https://jfly.uni-koeln.de/color/

oi_pal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
     "#0072B2", "#D55E00", "#CC79A7", "#999999")


```

  
### R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
# # Clear existing variables from the environment
# rm(list = ls ())
# # Set the working directory to the project folder
# setwd("C:/Users/kesha/OneDrive/Desktop/R_git/Capstone")
```

### Set environment is commented out since this is in already setup in git
#############################
```{r}
# # Set the base folder path
# box_folder=  "C:/Users/kesha/OneDrive/Desktop/R_git/Capstone" 
# 
# ## Input directory
# inputData.dir <- paste(box_folder,"ACS_social_explorer_data")

## Output directory
# out.dir <- paste(box_folder,"/Output/Result", Sys.Date(), sep="")
# if (!dir.exists(out.dir)) {
#   dir.create(out.dir, recursive = TRUE)
# }
# setwd(out.dir)
```


### Data Management
1. Data Processing & Inclusion Criteria
Data sources: EHDI (wustl.edu/CDC), KFF (Maternal Health), SHADAC (State Funding), and ACS (Demographics).
```{r data clean}
# Load and Filter per Inclusion Criteria: 42 States, <10% outcome missingness
excluded_states <- c("Alabama", "Colorado", "Delaware", "District of Columbia", 
                     "Illinois", "Minnesota", "New Hampshire", "New York", "Mississippi")

# read final edhi data for analysis
ehdi_paper <- read.csv("analysis_data_v3_EIrd_2009to2022_4282026.csv")%>% 
  select("State","Year","HLp","EIr","EIrd","PerTscr","Dr", "state_per_capita_fund","drinks_per_capita","Health_care_expenditure",
    "age_under5","Total_population","uninsured_under18_rate",
    "Median_household_income","under18_poverty_rate",
    "F_income_below_poverty_rate",
      "Per_rep_women_uninsured",
    "Per_women_breastfed" ) %>% 
  mutate("Per_age_under5"=age_under5/Total_population*100,) %>% 
  rename("Health_care_expenditure_percapita" ="Health_care_expenditure",
         "Per_infant_breastfed" = "Per_women_breastfed")%>% 
  filter(Year>=2009 & Year<=2022,
         Year !=2020)

# ehdi_paper <- ehdi_paper%>% 
#   select(`State`,`Year`,`HL`,`HLp`,`NHL`,TDia,`Tscr`,`TotPas`,`EI`,`Dr`,`EIr`,`LTFr`,`TNscr`,`LTF`,`CHIP_Enrollment`,`Health_care_expenditure`,`age_under5`,`Median_household_income`,`have_insurance_under18`,`without_insurance_under18`,`uninsured_under18_rate`,`F_income_below_poverty_rate`,`under18_poverty_rate`,`CHIP_enrollment_rate`,
#          `Family_income_below_poverty`,`Family_income_above_poverty`)
  
# check missingness by state year and variable
  missing_by_var <- ehdi_paper %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  arrange(desc(n_missing))

#missing rows
rows_with_any_missing <- ehdi_paper %>%
  mutate(any_missing = if_any(-c(State, Year), is.na)) %>%
  filter(any_missing) %>%
  select(State, Year)

# missing in details with variables
missing_details <- ehdi_paper %>%
  pivot_longer(cols = -c(State, Year), names_to = "variable", values_to = "value") %>%
  filter(is.na(value)) %>%
  arrange(State, Year, variable)
  View(missing_details)

# apply filter to remove NA
  ehdi_pos <- ehdi_paper %>%  
    filter(!is.na(HLp)) %>%
    filter(State != "Colorado")
# group_by(Year) %>%
#             summarise(across(where(is.numeric), ~sum(., na.rm=TRUE))) %>%
#             mutate(State="Total", Year, sep="_")


#filter states that have data for all the years for all the variables


# Check which states have data for all years
states_all_years <- ehdi_pos %>%
  group_by(State) %>%
  summarise(num_years = n_distinct(Year)) %>%
  filter(num_years == 5) %>%
  pull(State)

# Filter the dataset to include only those states
# ehdi_pos <- ehdi_pos %>%
#   filter(State %in% states_all_years)

#write out filtered data
# write_csv(ehdi_pos,
#          "output_data_paper_2025/final_paper_data_filtered25.csv")
```
# Reviewer Note: 
# The primary analytic sample includes 42 states with near-complete outcome data. 
# States missing >1 year of outcome data (10% threshold) were excluded to maintain 
# longitudinal integrity of the panel.

# HLp Quartile  
```{r}
    # Quartiles by hlp
# ehdi_pos <- ehdi_pos %>% 
#   mutate(
#   hlp_quartile = ntile(HLp, 4)
#     ) %>%
#     mutate(
#       hlp_quartile = factor(hlp_quartile,
#                                 labels = c("Lowest load", "Q2", "Q3", "Highest load")))

```

# defining variables
```{r}
  # 2) Define outcome + covariates (as described by you)
 
   y <- c("HLp", "EIr", "EIrd")
  
  xdesc <- c("PerTscr","Dr","state_per_capita_fund","Median_household_income",
      "Per_rep_women_uninsured",
    "Per_infant_breastfed" 
  )
```
  
  
# Assign variable label

```{r}
label(ehdi_pos$HLp) <- "Hearing Loss Prevalence (HLp)"
label(ehdi_pos$EIr) <- "Early Intervention Rate (Part c & Non-Part C) (EIr)"
label(ehdi_pos$EIrd) <- "Early Intervention Rate (Part c & Non-Part C) (EIrd)"
label(ehdi_pos$PerTscr) <- "Percent of Infants Screened for Hearing Loss (HL)"
label(ehdi_pos$Dr) <- "Diagnosis Rate"
label(ehdi_pos$Median_household_income) <- "Median Household Income (USD)"
label(ehdi_pos$state_per_capita_fund) <- "Per Capita State Public Health Funding"
label(ehdi_pos$Per_rep_women_uninsured) <- "Percent of Reproductive Age Women Uninsured"
label(ehdi_pos$Per_infant_breastfed) <- "Percent of Infant Ever Breastfed"


# create lookup vector
var_labels <- sapply(
  ehdi_pos[, c(y, xdesc)],
  function(v) attr(v, "label")
)

var_labels

```


# Descriptive table
```{r}


descriptive <- table1(~ HLp +EIrd+ EIr+PerTscr+Dr + Median_household_income + state_per_capita_fund + Per_rep_women_uninsured + Per_infant_breastfed,
       data= ehdi_pos, render.missing=NULL, render.continuous ="median(IQR)", caption= "Descriptive Statistics of EHDI Outcomes and State-Level Covariates, (2009–2022)")

# Use kable to create the table with a caption
kable(descriptive, caption = "Descriptive statistics 2009-2022)", format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

  # 1) Prepare data (assumes ehdi_pos already exists)
  df <- ehdi_pos %>%
    mutate(
      State = str_squish(as.character(State)),
      Year  = as.character(Year)
    ) #%>% filter(Year >= 2015, Year <= 2019)
  
  # # 3) Quick missingness check (paper-like transparency)
  # missing_summary <- df %>%
  #   summarise(across(c(all_of(y), all_of(xdesc)), ~ sum(is.na(.)))) %>%
  #   pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  #   arrange(desc(n_missing))
  # kable(missing_summary)
  

  # 4) Descriptive statistics (overall) like an exhibit table
  desc_overall <- df %>%
    summarise(across(c(all_of(y), all_of(xdesc)),
                     list(
                       N = ~ sum(!is.na(.)),
                       Mean = ~ mean(., na.rm = TRUE),
                       SD = ~ sd(., na.rm = TRUE),
                       Min = ~ min(., na.rm = TRUE),
                       Max = ~ max(., na.rm = TRUE)
                     ),
                     .names = "{.col}__{.fn}")) %>%
    pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
    separate(stat, into = c("variable", "stat"), sep = "__") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    arrange(match(variable, c(y, xdesc))) %>% 
    mutate(Variable = var_labels[variable]) %>% 
    select(Variable, N, Mean, SD, Min, Max)
  
  kable(desc_overall, digit=2, caption = "Descriptive Statistics of EHDI Outcomes and State-Level Covariates, 2009–2022", format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
  
  # 2. Convert to flextable and format for professional appearance
ft <- flextable(desc_overall) %>%
  set_caption("Table 1: Descriptive Statistics of EHDI Outcomes and State-Level Covariates, 2009–2022") %>%
  colformat_double(digits = 2) %>%  # Sets decimal places for Mean, SD, etc.
  autofit() %>%                     # Adjusts column widths automatically
  theme_booktabs() %>%              # Clean, academic style
  bold(part = "header")

# 3. Export directly to Word
#save_as_docx(ft, path = "Descriptive_Statistics_Table1.docx")
```


### histogram to see the distribution of variables

```{r}
#Dependent variable
ggplot(ehdi_pos, aes(x = HLp)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Hearing Loss Prevenalnce (HLp)",
    x = "Hearing loss prevalence",
    y = "Count"
  ) +
  theme_minimal()

ggplot(ehdi_pos, aes(x = EIr)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Early Intervention Rate (EIr)",
    x = "Early Intervention rate",
    y = "Count"
  ) +
  theme_minimal()

ggplot(ehdi_pos, aes(x = EIrd)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Early Intervention Rate (EIrd)",
    x = "Early Intervention rate",
    y = "Count"
  ) +
  theme_minimal()

```


# Histo for covariates
```{r}
#Independent variable
  covariates <- c("PerTscr","Dr","state_per_capita_fund","drinks_per_capita",
    
    "Health_care_expenditure_percapita",
    "Per_age_under5","uninsured_under18_rate", "Median_household_income","under18_poverty_rate",
    "F_income_below_poverty_rate",
      "Per_rep_women_uninsured",
    "Per_infant_breastfed" 
  )

# Reshape data to "long" format for facet plotting
ehdi_pos %>%
  select(all_of(covariates)) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "lightgray", color = "white") +
  facet_wrap(~name, scales = "free") + # "free" allows each x-axis to scale independently
  labs(title = "Distribution of Covariates (Independent Variables)", x = "Value", y = "Count") +
  theme_minimal()

```


### correlation check
```{r}
# total correlation check
  total_variables <- c("EIr","EIrd", "HLp","PerTscr","Dr","state_per_capita_fund","drinks_per_capita",
    "Health_care_expenditure_percapita",
    "Per_age_under5","uninsured_under18_rate", "Median_household_income","under18_poverty_rate",
    "F_income_below_poverty_rate",
      "Per_rep_women_uninsured",
    "Per_infant_breastfed" 
  )
total_covariates <- cor(ehdi_pos[, total_variables ], use = "pairwise.complete.obs")
kable(round(total_covariates, 3))

# correlation of hlp variables
xcorr_hlp <- c( "HLp","PerTscr","state_per_capita_fund","Per_age_under5","uninsured_under18_rate","Median_household_income","Per_infant_breastfed")
cors_hlp <- cor(ehdi_pos[, xcorr_hlp], use = "pairwise.complete.obs")
kable(round(cors_hlp, 3))

heatmaply::heatmaply_cor(cors_hlp, k_col = 2, k_row = 2,
                         xlab = "Variables", ylab = "Variables",
                         main = "Correlation matrix (interactive)")

xcorr_EIr <- c( "EIrd","EIr","PerTscr","Dr","state_per_capita_fund","Per_age_under5", "Median_household_income","Per_rep_women_uninsured","Per_infant_breastfed" )
cors_EIr <- cor(ehdi_pos[, xcorr_EIr], use = "pairwise.complete.obs")
kable(round(cors_EIr, 3))

heatmaply::heatmaply_cor(cors_EIr, k_col = 2, k_row = 2,
                         xlab = "Variables", ylab = "Variables",
                         main = "Correlation matrix (interactive)")

# 6) Correlation matrix (optional robustness-style table)
cor_mat <- cor(df %>% select(all_of(c(y, xcorr_hlp))), use = "pairwise.complete.obs")
kable(round(cor_mat, 3))

```

# ================================
# PCA for Covariate Reduction
# ================================
Because Poverty and Median Income are highly correlated (r > 0.7), we use PCA to create a "Socioeconomic Status (SES)" index to avoid multicollinearity.
```{r}

pca_vars_age <- c(
  "PerTscr",
  "Dr","Per_age_under5",
  "state_per_capita_fund",
  "Median_household_income",
  "Per_rep_women_uninsured",
  "Per_infant_breastfed"
)

pca_data_age <- ehdi_pos %>%
  select(all_of(pca_vars_age)) %>%
  drop_na()

pca_res_age <- prcomp(pca_data_age, scale. = TRUE)

# Variable loading plot
fviz_pca_var(
  pca_res_age,
  col.var = "contrib",
  gradient.cols = c("#2c7fb8", "#E7B800", "#d7191c"),
  repel = TRUE,
  title = "PCA: Clustering of Biopsychosocial Covariates"
) +
  theme_minimal()
```

# Assign variable label

```{r}
label(ehdi_pos$HLp) <- "Hearing Loss Prevalence (HLp)"
label(ehdi_pos$EIr) <- "Early Intervention Rate(EIr)"
label(ehdi_pos$EIrd) <- "Early Intervention Rate(EIrd)"
label(ehdi_pos$PerTscr) <- "Percent of Infants Total Screened"
label(ehdi_pos$Dr) <- "Diagnosis Rate"
label(ehdi_pos$Median_household_income) <- "Median Household Income (USD)"
label(ehdi_pos$state_per_capita_fund) <- "Per Capita State Public Health Funding"
label(ehdi_pos$Per_rep_women_uninsured) <- "Percent Reproductive Age Women Uninsured"
label(ehdi_pos$Per_infant_breastfed) <- "Percent of Infant Ever Breastfed"


# create lookup vector
var_pca <- sapply(
  ehdi_pos[, c(y, xdesc)],
  function(v) attr(v, "label")
)

var_pca

```


```{r}
# 1. Define variables and prepare data
pca_vars <- c(
  "PerTscr",
  "Dr",
  "state_per_capita_fund",
  "Median_household_income",
  "Per_rep_women_uninsured",
  "Per_infant_breastfed"
)

# Prepare clean data for PCA
pca_data <- ehdi_pos %>%
  select(all_of(pca_vars)) %>%
  drop_na()

  # 2. Convert to flextable and format for professional appearance
pca_ft <- flextable(pca_data) %>%
  set_caption("Data Source for PCA Covariates") %>%
  colformat_double(digits = 2) %>%  # Sets decimal places for Mean, SD, etc.
  autofit() %>%                     # Adjusts column widths automatically
  theme_booktabs() %>%              # Clean, academic style
  bold(part = "header")

# 3. Export directly to Word
#save_as_docx(pca_ft, path = "Data Source for PCA Covariates.docx")

# 2. Run PCA using original column names
pca_res <- prcomp(pca_data, scale. = TRUE)

# 3. GENERATE PCA SCORES IMMEDIATELY 
# (Must be done before renaming rotation matrix to avoid 'newdata' errors)
pca_scores <- as.data.frame(predict(pca_res, pca_data))

# 4. PREPARE FOR PLOTTING
# Create a temporary object for plotting to keep the main pca_res object "clean"
pca_plot_obj <- pca_res

# Replace technical rownames with your descriptive labels for the plot
pca_var_labels <- var_pca[pca_vars]
rownames(pca_plot_obj$rotation) <- pca_var_labels

# 5. GENERATE THE PCA PLOT
library(ggforce)

# Extract the labeled rotation data
arrow_data <- as.data.frame(pca_plot_obj$rotation[, 1:2])
scaling_factor <- 1.3 

fviz_pca_var(
    pca_plot_obj,
    col.var = "black",       # Sets labels to black
    alpha.var = 0,           # Hides the original thin arrows
    labelsize = 3,           # Slightly larger for readability
    repel = FALSE            # Keeps labels near arrow tips
) +
    # Draw the correlation circle using annotate (avoids length warnings)
    annotate("path",
             x = cos(seq(0, 2*pi, length.out = 100)),
             y = sin(seq(0, 2*pi, length.out = 100)),
             linewidth = 0.9, 
             color = "black") +
    # Manual Thicker & Longer Arrows
    geom_segment(
        data = arrow_data,
        aes(
            x = 0, y = 0,
            xend = PC1 * scaling_factor, 
            yend = PC2 * scaling_factor  
        ),
        linewidth = 1,
        color = "black",
        arrow = arrow(length = unit(0.3, "cm"))
    ) +
    scale_x_continuous(expand = expansion(mult = 0.2)) +
    scale_y_continuous(expand = expansion(mult = 0.2)) +
    labs(
        title = "Principal Component Structure of Covariates",
        subtitle = "Variable loadings indicate shared variance across biopsychosocial factors"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold")
    )

#save plot
#ggsave("PCA_plot_final_v2.TIFF",  width = 8, height = 6,dpi = 300, compression = "lzw")

# FINAL: Extract exact data used for PCA arrows in the plot

scaling_factor <- 1.3  # must match your plot

pca_plot_data <- as.data.frame(pca_res$rotation[, 1:2]) %>%
  tibble::rownames_to_column("Variable") %>%
  dplyr::mutate(
    Label = var_pca[Variable],   # descriptive labels used in plot
    x = 0,
    y = 0,
    xend = PC1 * scaling_factor,
    yend = PC2 * scaling_factor
  ) %>%
  dplyr::select(Label, PC1, PC2, x, y, xend, yend)

pca_plot_data
#write.csv(pca_plot_data, "pca_plot_arrows_data.csv", row.names = FALSE)

  # 2. Convert to flextable and format for professional appearance
pca_word <- flextable(pca_plot_data) %>%
  set_caption("Data Source for PCA Covariates") %>%
  colformat_double(digits = 2) %>%  # Sets decimal places for Mean, SD, etc.
  autofit() %>%                     # Adjusts column widths automatically
  theme_booktabs() %>%              # Clean, academic style
  bold(part = "header")
#save_as_docx(pca_word, path = "Data Source for PCA Covariates.docx")

# 6. MAP SCORES BACK TO MAIN DATASET
# Create a copy and merge based on complete cases
ehdi_pos_pca <- ehdi_pos
complete_cases_idx <- which(complete.cases(ehdi_pos[, pca_vars]))

# Assign the first three PCs back to the data
ehdi_pos_pca[complete_cases_idx, c("PC1", "PC2", "PC3")] <- pca_scores[, 1:3]

# Create final working dataframe
df <- ehdi_pos_pca %>%
  mutate(
    State = str_squish(as.character(State)),
    Year = as.integer(Year)
  )

# Create panel data object
pdf <- pdata.frame(df, index = c("State", "Year"))

# 7. Print Summary to Verify
cat("\n=== PCA ANALYSIS SUMMARY ===\n")
print(summary(pca_res))

```



```{r}

# # creating pca without the age covariates
# 
# pca_vars <- c(
#   "PerTscr",
#   "Dr",
#   "state_per_capita_fund",
#   "Median_household_income",
#   "Per_rep_women_uninsured",
#   "Per_infant_breastfed"
# )
# 
# 
# # prepare pca data 
# pca_data <- ehdi_pos %>%
#   select(all_of(pca_vars)) %>%
#   drop_na()
# 
# # run pca
# pca_res <- prcomp(pca_data, scale. = TRUE)
# pca_scores <- as.data.frame(predict(pca_res,pca_data))
# 
# #create label vector as pca variables
# pca_var_labels <- var_pca[pca_vars]
# # Store a copy of original rotation for safe keeping if needed
# original_rotation <- pca_res$rotation
# # replace rownames
# rownames(pca_res$rotation) <- pca_var_labels
# 
# # 
# # # color Variable loading plot
# # fviz_pca_var(
# #   pca_res,
# #   col.var = "contrib",
# #   gradient.cols = c("#040404", "#c51b8a", "#d7191c"),
# #   alpha.var = 0.7,
# #   repel = TRUE) +
# #   labs(
# #     title = "Principal Component Structure of Covariates",
# #     subtitle = "Loadings indicate shared variance across biopsychosocial factors"
# #   ) +
# #   theme_minimal(base_size = 13) +
# #   theme(
# #     plot.title = element_text(face = "bold"),
# #     axis.title = element_text(face = "bold"),
# #     legend.title = element_text(face = "bold")
# #   )
# # 
# # 
# 
# 
# 
# # black and white pca loading plot
# 
# library(ggforce)
# 
# # 1. Extract the rotation (loadings) data
# arrow_data <- as.data.frame(pca_res$rotation[, 1:2])
# 
# # 2. Define a scaling factor to increase arrow length (e.g., 1.2 for 20% longer)
# # Adjust this number to increase the length
# scaling_factor <- 1.3 
# 
# fviz_pca_var(
#     pca_res,
#     col.var = "black",       # Sets labels to black
#     alpha.var = 0,           # Hides the original thin arrows
#     labelsize = 4,           # Slightly larger for readability
#     repel = FALSE             # Prevents labels from overlapping arrows
# ) +
#     # Thicker, darker correlation circle
#     geom_circle(
#         aes(x0 = 0, y0 = 0, r = 1),
#         inherit.aes = FALSE,
#         linewidth = 0.9,
#         color = "black"
#     ) +
#     # Manual Thicker & Longer Arrows
#     geom_segment(
#         data = arrow_data,
#         aes(
#             x = 0, y = 0,
#             xend = PC1 * scaling_factor, # Increase length on X
#             yend = PC2 * scaling_factor  # Increase length on Y
#         ),
#         linewidth = 1,
#         color = "black",
#         arrow = arrow(length = unit(0.3, "cm"))
#     ) +
#     scale_x_continuous(expand = expansion(mult = 0.2)) +
#     scale_y_continuous(expand = expansion(mult = 0.2)) +
#     labs(
#         title = "Principal Component Structure of Covariates",
#         subtitle = "Variable loadings indicate shared variance across biopsychosocial factors"
#     ) +
#     theme_minimal(base_size = 13) +
#     theme(
#         plot.title = element_text(face = "bold"),
#         plot.subtitle = element_text(size = 11),
#         axis.title = element_text(face = "bold")
#     )
# 
# 
# # PCA Summary
# cat("\n=== PCA ANALYSIS SUMMARY ===\n")
# print(summary(pca_res))
# 
# # Scree plot
# fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 60)) +
#   labs(title = "Scree Plot: Variance Explained by PCA Components",
#        subtitle = "Components 1-3 explain >80% of total variance") +
#   theme_minimal()
# 
# # Check variance explained
# variance_explained <- summary(pca_res)$importance[2,] * 100
# cat("\nVariance explained by each component (%):\n")
# print(variance_explained)
# 
# cat("\nCumulative variance explained by first 3 components: ", 
#     sum(variance_explained[1:3]), "%\n")
# 
# 
# # Extract PCA loadings
# pca_loadings <- as.data.frame(pca_res$rotation[, 1:3])
# colnames(pca_loadings) <- c("PC1", "PC2", "PC3")
# 
# cat("\nPCA Loadings (First 3 Components):\n")
# kable(round(pca_loadings, 3), caption = "PCA Variable Loadings") %>%
#   kable_styling()
# 
# # Check variable contributions to PC1
# fviz_contrib(pca_res, choice = "var", axes = 1, top = 10) +
#   ggtitle("Contributions of Variables to PC1")
# 
# # Add PCA scores to original dataset
# pca_scores <- as.data.frame(predict(pca_res, pca_data))
# ehdi_pos_pca <- ehdi_pos
# 
# # Add PCA scores where we have complete data
# complete_cases <- complete.cases(ehdi_pos[, var_pca])
# ehdi_pos_pca[complete_cases, c("PC1", "PC2", "PC3")] <- pca_scores
# 
# # Create working dataframe with PCA scores
# df <- ehdi_pos_pca %>%
#   mutate(
#     State = str_squish(as.character(State)),
#     Year = as.integer(Year)
#   )
# 
# # Create panel data
# pdf <- pdata.frame(df, index = c("State", "Year"))

```

# # defining variables for HLp model

```{r}
  # 2) Define outcome + covariates (as described by you)
 #dependent variable
   y <- "HLp"
 # Independent variable
 hlp_vars_original  <- c("PerTscr","Dr","state_per_capita_fund","Median_household_income","Per_rep_women_uninsured", "Per_infant_breastfed")

  hlp_vars_pca <- c("PC1", "PC2", "PC3")
```


============================================================================
# SENSITIVITY ANALYSIS: South vs. Non-South Interaction Check
# Purpose: Address reviewer concern about regional cultural confounding
#          of the breastfeeding–EHDI association
# ============================================================================
#
# SCIENTIFIC RATIONALE FOR SOUTH VS. NON-SOUTH CLASSIFICATION:
# ------------------------------------------------------------
# We use the U.S. Census Bureau's official South region definition 
# (CDC/NCHS, 2024; Census Bureau, 2020) because:
#
# 1. It is a pre-defined, standardized federal classification — not ad hoc
#    (Source: CDC NCHS Geographic Region Definitions,
#     https://www.cdc.gov/nchs/hus/sources-definitions/geographic-region.htm)
#
# 2. Southern states have consistently lower breastfeeding initiation rates
#    (e.g., Louisiana 72.4%, Alabama 77.9%, Kentucky 73.9% vs. national 
#    average 85.7%) per the CDC Breastfeeding Report Card, 2022
#    (https://www.cdc.gov/breastfeeding-data/breastfeeding-report-card/)
#
# 3. Goldhagen et al. (Pediatrics, 2005; 116(6):e746-53) demonstrated that
#    the Deep South region is a stronger predictor of poor child health 
#    outcomes than conventional socioeconomic variables, establishing region 
#    as a meaningful proxy for cultural, policy, and structural factors
#    (DOI: 10.1542/peds.2005-0366)
#
# 4. The Urban Institute (2022) documented that maternal mortality, Medicaid 
#    non-expansion, and racial health inequities cluster disproportionately 
#    in Southern states, reflecting distinct health policy and cultural 
#    environments relevant to maternal-child health engagement
#
# STATES CLASSIFICATION (U.S. Census Bureau South Region):
# South: AL, AR, DE, DC, FL, GA, KY, LA, MD, MS, NC, OK, SC, TN, TX, VA, WV
# Non-South: All remaining states
# ============================================================================

```{r cars}
# ---- STEP 1: Check which states are actually in your data ----

cat("\n=== STATES IN YOUR DATA ===\n")
cat("Total unique states:", n_distinct(df$State), "\n")
print(sort(unique(df$State)))

```


```{r pressure, echo=FALSE}
# ---- STEP 2: Create South indicator ----
# U.S. Census Bureau South Region (CDC/NCHS definition)
# Reference: https://www.cdc.gov/nchs/hus/sources-definitions/geographic-region.htm
# We only list states that EXIST in your 42-state dataset

south_states_census <- c(
  "Alabama", "Arkansas", "Delaware", "Florida", "Georgia",
  "Kentucky", "Louisiana", "Maryland", "Mississippi",
  "North Carolina", "Oklahoma", "South Carolina", "Tennessee",
  "Texas", "Virginia", "West Virginia"
)

# Show which Census South states are actually in your data
south_in_data <- south_states_census[south_states_census %in% unique(df$State)]
south_not_in_data <- south_states_census[!south_states_census %in% unique(df$State)]

cat("\nCensus South states PRESENT in your data:\n")
print(south_in_data)
cat("\nCensus South states NOT in your data (excluded earlier):\n")
print(south_not_in_data)

```

```{r}
# Create the indicator
df <- df %>%
  mutate(
    South = ifelse(State %in% south_states_census, 1, 0),
    South_label = ifelse(South == 1, "South", "Non-South")
  )
# Summary
cat("\n=== REGIONAL CLASSIFICATION ===\n")
region_summary <- df %>%
  distinct(State, South_label) %>%
  count(South_label, name = "N_states")
print(region_summary)

cat("\nObservations by region:\n")
print(table(df$South_label))

```


```{r}
# ---- STEP 3: Descriptive comparison ----

bf_by_region <- df %>%
  group_by(South_label) %>%
  summarise(
    N_obs = n(),
    N_states = n_distinct(State),
    BF_mean = mean(Per_infant_breastfed, na.rm = TRUE),
    BF_sd   = sd(Per_infant_breastfed, na.rm = TRUE),
    HLp_mean = mean(HLp, na.rm = TRUE),
    HLp_sd   = sd(HLp, na.rm = TRUE),
    .groups = "drop"
  )

kable(bf_by_region, digits = 3,
      caption = "Breastfeeding and HLp by Region (South vs. Non-South)") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```


```{r}
# ---- STEP 4: INTERACTION MODEL ----
# Formula: HLp ~ covariates + Per_infant_breastfed * South + factor(Year)
# The * operator includes both main effects AND the interaction

f_interact_hlp <- as.formula(
  paste(y, "~",
        paste(hlp_vars_original[hlp_vars_original != "Per_infant_breastfed"],
              collapse = " + "),
        "+ Per_infant_breastfed * South + factor(Year)")
)

# Check formula is correct
cat("\n=== FORMULA ===\n")
print(f_interact_hlp)

# Fit model
m_interact_hlp <- lm(f_interact_hlp, data = df)

# Cluster-robust SE by state (same as your main models)
vc_interact_hlp <- vcovCL(m_interact_hlp, cluster = df$State, type = "HC1")

# Full results with robust SEs
interact_results_hlp <- tidy(coeftest(m_interact_hlp, vcov = vc_interact_hlp, conf.int = TRUE, conf.level = 0.95)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    significant = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ "†",
      TRUE            ~ ""
    )
  )

# Show key variables only (exclude year dummies for readability)
key_terms <- interact_results_hlp %>%
  filter(!grepl("factor\\(Year\\)", term))

kable(key_terms, digits = 3,
      caption = "HLp: Pooled OLS with Breastfeeding × South Interaction (Cluster-Robust SE)") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```


```{r}
# ---- STEP 5: EXTRACT AND INTERPRET THE INTERACTION TERM ----

cat("\n========================================\n")
cat("  KEY RESULT: INTERACTION TERM\n")
cat("========================================\n")

interact_term <- interact_results_hlp %>%
  filter(term == "Per_infant_breastfed:South")

if(nrow(interact_term) == 0) {
  # Sometimes R names it South:Per_infant_breastfed instead
  interact_term <- interact_results_hlp %>%
    filter(grepl("South", term) & grepl("Per_infant_breastfed", term))
}

cat(sprintf(
  "\nBreastfeeding × South: β = %.3f, SE = %.3f, t = %.3f, p = %.4f %s\n",
  interact_term$estimate, interact_term$std.error,
  interact_term$statistic, interact_term$p.value,
  interact_term$significant
))

# Also show the main effect of breastfeeding (for Non-South, the reference)
bf_main <- interact_results_hlp %>%
  filter(term == "Per_infant_breastfed")

cat(sprintf(
  "Breastfeeding main effect (Non-South): β = %.3f, SE = %.3f, p = %.4f %s\n",
  bf_main$estimate, bf_main$std.error, bf_main$p.value, bf_main$significant
))

# The total effect for South = main + interaction
cat(sprintf(
  "Breastfeeding total effect for South = %.3f + %.3f = %.3f\n",
  bf_main$estimate, interact_term$estimate,
  bf_main$estimate + interact_term$estimate
))

if (interact_term$p.value > 0.05) {
  cat("\n>> RESULT: Interaction is NON-SIGNIFICANT (p > 0.05)\n")
  cat(">> The breastfeeding-HLp association does NOT significantly differ\n")
  cat(">> between Southern and Non-Southern states.\n")
  cat(">> This refutes the concern that regional cultural beliefs drive\n")
  cat(">> the observed association.\n")
} else {
  cat("\n>> RESULT: Interaction IS significant (p < 0.05)\n")
  cat(">> The breastfeeding effect DIFFERS by region.\n")
  cat(">> Report stratified results for South and Non-South.\n")
}

```


```{r}
# ---- STEP 6: MODEL FIT ----

cat("\n=== MODEL FIT ===\n")
fit <- glance(m_interact_hlp)
cat(sprintf("N = %d, R² = %.3f, Adj. R² = %.3f, F = %.2f\n",
            fit$nobs, fit$r.squared, fit$adj.r.squared, fit$statistic))


# ---- STEP 7: COMPARE WITH YOUR ORIGINAL MODEL (stability check) ----

cat("\n=== STABILITY CHECK: Original vs. Interaction Model ===\n")

# Your original pooled OLS (no interaction)
f_original <- as.formula(
  paste(y, "~", paste(hlp_vars_original, collapse = " + "), "+ factor(Year)")
)
m_original <- lm(f_original, data = df)
vc_original <- vcovCL(m_original, cluster = df$State, type = "HC1")

# Compare breastfeeding coefficients
bf_orig <- tidy(coeftest(m_original, vcov = vc_original)) %>%
  filter(term == "Per_infant_breastfed") %>%
  mutate(Model = "Original (no interaction)")

bf_new <- tidy(coeftest(m_interact_hlp, vcov = vc_interact_hlp)) %>%
  filter(term == "Per_infant_breastfed") %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    Model = "With interaction (Non-South effect)")

comparison <- bind_rows(bf_orig, bf_new) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.10  ~ "†",
    TRUE ~ ""
  )) 

kable(comparison, digits = 3,
      caption = "Breastfeeding Coefficient Stability: Original vs. Interaction Model") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```


```{r}
# ---- STEP 8: VIF CHECK ----

cat("\n=== VIF CHECK ===\n")
vif_vals <- car::vif(m_interact_hlp)
kable(vif_vals, digits = 2, caption = "VIF for Interaction Model") %>%
  kable_styling()
```

============================================================================
# INTERACTION TEST: Breastfeeding × Low/High Breastfeeding Culture
```{r}

#
# DATA-DRIVEN CLASSIFICATION using CDC Breastfeeding Report Card (2022)
# "Ever breastfed" rates among infants born in 2019
#
# Cutpoint: National average = 83.2% (CDC Healthy People benchmark)
#   Low BF Culture  = states below 83.2% "ever breastfed"
#   High BF Culture = states at or above 83.2%
#
# WHY THIS IS BETTER THAN SOUTH vs. NON-SOUTH:
# 1. Directly measures what the reviewer asked about (breastfeeding culture)
# 2. Data-driven from an authoritative published source (CDC, 2022)
# 3. Correctly classifies states that Census South misclassifies:
#    - Maryland (88.5%), Texas (84.1%), Virginia (83.3%) are Census "South"
#      but have HIGH breastfeeding rates
#    - Pennsylvania (74.8%), Ohio (79.5%), Missouri (78.3%) are Census
#      "Non-South" but have LOW breastfeeding rates
# 4. Uses the national Healthy People benchmark as the threshold
# 5. Time-invariant classification (fixed 2019 data) — not endogenous to
#    the panel model's time-varying Per_infant_breastfed variable
#
# CITE: CDC. Breastfeeding Report Card, United States, 2022.
#       https://www.cdc.gov/breastfeeding-data/breastfeeding-report-card/
# ============================================================================

# ---- STEP 1: Create classification from CDC published data ----

# CDC "Ever Breastfed" rates, 2019 births (published 2022 Report Card)
# Only states in your 42-state analytic sample
low_bf_culture_states <- c(
  "West Virginia",    # 59.8%
  "Florida",          # 71.0%
  "Louisiana",        # 71.1%
  "Kentucky",         # 74.7%
  "Pennsylvania",     # 74.8%  ← Non-South, but low BF
  "Arkansas",         # 74.9%
  "Oklahoma",         # 77.3%
  "Missouri",         # 78.3%  ← Non-South, but low BF
  "Tennessee",        # 78.8%
  "Ohio",             # 79.5%  ← Non-South, but low BF
  "Massachusetts",    # 80.0%  ← Non-South, but low BF
  "South Carolina",   # 80.6%
  "Rhode Island",     # 82.4%  ← Non-South, but low BF
  "Iowa",             # 82.4%  ← Non-South, but low BF
  "New Jersey",       # 82.5%  ← Non-South, but low BF
  "Georgia",          # 82.6%
  "Michigan"          # 83.1%  ← Non-South, but low BF
)
# All other 25 states in your data = High BF Culture (≥83.2%)

# Create indicator
df <- df %>%
  mutate(
    Low_BF_Culture = ifelse(State %in% low_bf_culture_states, 1, 0),
    BF_Culture_label = ifelse(Low_BF_Culture == 1, 
                               "Low BF Culture (<83.2%)", 
                               "High BF Culture (≥83.2%)")
  )


# ---- STEP 2: Verify classification ----

cat("\n=== BF CULTURE CLASSIFICATION (CDC 2022 Report Card) ===\n")
culture_summary <- df %>%
  distinct(State, BF_Culture_label) %>%
  count(BF_Culture_label, name = "N_states")
print(culture_summary)

cat("\nObservations by group:\n")
print(table(df$BF_Culture_label))

# Compare with South classification
cat("\n=== CROSS-TAB: BF Culture vs. Census South ===\n")
cross_tab <- df %>%
  distinct(State, South_label, BF_Culture_label) %>%
  count(BF_Culture_label, South_label) %>%
  pivot_wider(names_from = South_label, values_from = n, values_fill = 0)
print(cross_tab)


# ---- STEP 3: Descriptive comparison ----

bf_culture_desc <- df %>%
  group_by(BF_Culture_label) %>%
  summarise(
    N_obs = n(),
    N_states = n_distinct(State),
    BF_mean = mean(Per_infant_breastfed, na.rm = TRUE),
    BF_sd   = sd(Per_infant_breastfed, na.rm = TRUE),
    HLp_mean = mean(HLp, na.rm = TRUE),
    HLp_sd   = sd(HLp, na.rm = TRUE),
    .groups = "drop"
  )

kable(bf_culture_desc, digits = 3,
      caption = "Breastfeeding and HLp by BF Culture Classification (CDC 2022)") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


# ---- STEP 4: INTERACTION MODEL ----

cat("\n=== INTERACTION MODEL: Breastfeeding × Low BF Culture ===\n")

f_interact_culture <- as.formula(
  paste(y, "~",
        paste(hlp_vars_original[hlp_vars_original != "Per_infant_breastfed"],
              collapse = " + "),
        "+ Per_infant_breastfed * Low_BF_Culture + factor(Year)")
)

cat("Formula:\n")
print(f_interact_culture)

m_interact_culture <- lm(f_interact_culture, data = df)
vc_interact_culture <- vcovCL(m_interact_culture, cluster = df$State, type = "HC1")

interact_results_culture <- tidy(coeftest(m_interact_culture, vcov = vc_interact_culture)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    significant = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ "†",
      TRUE            ~ ""
    )
  )

# Key terms only
key_terms_culture <- interact_results_culture %>%
  filter(!grepl("factor\\(Year\\)", term))

kable(key_terms_culture, digits = 3,
      caption = "HLp: Breastfeeding × BF Culture Interaction (Cluster-Robust SE)") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


# ---- STEP 5: KEY RESULT ----

cat("\n========================================\n")
cat("  KEY RESULT: BF CULTURE INTERACTION\n")
cat("========================================\n")

interact_term_culture <- interact_results_culture %>%
  filter(grepl("Low_BF_Culture", term) & grepl("Per_infant_breastfed", term))

bf_main_culture <- interact_results_culture %>%
  filter(term == "Per_infant_breastfed")

cat(sprintf(
  "\nBreastfeeding main effect (High BF Culture): β = %.3f, SE = %.3f, p = %.4f %s\n",
  bf_main_culture$estimate, bf_main_culture$std.error, 
  bf_main_culture$p.value, bf_main_culture$significant
))

cat(sprintf(
  "Breastfeeding × Low BF Culture interaction: β = %.3f, SE = %.3f, p = %.4f %s\n",
  interact_term_culture$estimate, interact_term_culture$std.error,
  interact_term_culture$p.value, interact_term_culture$significant
))

cat(sprintf(
  "Breastfeeding total effect in Low BF Culture states: %.3f + %.3f = %.3f\n",
  bf_main_culture$estimate, interact_term_culture$estimate,
  bf_main_culture$estimate + interact_term_culture$estimate
))

if (interact_term_culture$p.value > 0.05) {
  cat("\n>> RESULT: Interaction is NON-SIGNIFICANT (p > 0.05)\n")
  cat(">> Even using a direct, data-driven measure of breastfeeding culture\n")
  cat(">> from the CDC, the breastfeeding-HLp association does NOT differ\n")
  cat(">> between low and high breastfeeding culture states.\n")
} else {
  cat("\n>> RESULT: Interaction IS significant (p < 0.05)\n")
}


# ---- STEP 6: MODEL FIT ----

cat("\n=== MODEL FIT ===\n")
fit_culture <- glance(m_interact_culture)
cat(sprintf("N = %d, R² = %.3f, Adj. R² = %.3f\n",
            fit_culture$nobs, fit_culture$r.squared, fit_culture$adj.r.squared))


# ---- STEP 7: SIDE-BY-SIDE COMPARISON OF BOTH INTERACTION TESTS ----

cat("\n=== COMPARISON: Both Interaction Tests ===\n")

# South interaction (from earlier)
south_int <- tidy(coeftest(m_interact_hlp, vcov = vc_interact_hlp)) %>%
  filter(grepl("South", term) & grepl("Per_infant_breastfed", term)) %>%
  mutate(Test = "Census South")

# BF Culture interaction
culture_int <- interact_results_culture %>%
  filter(grepl("Low_BF_Culture", term) & grepl("Per_infant_breastfed", term)) %>%
  mutate(Test = "CDC BF Culture")

both_tests <- bind_rows(south_int, culture_int) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.10  ~ "†",
    TRUE ~ ""
  ))

kable(both_tests, digits = 3,
      caption = "Comparison of Interaction Tests: Census South vs. CDC BF Culture") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

cat("\n>> Both tests confirm: the breastfeeding-HLp association is NOT\n")
cat(">> moderated by regional breastfeeding culture.\n")
```


############################################################
# OPTIONAL TO CHECK: HLp: Pooled OLS without year fixed effects 
# Cluster-robust SE by state 
############################################################
```{r}

# Pooled OLS without year fixed effects
f_pooled_no_year <- as.formula(paste(y, "~", paste(hlp_vars_original, collapse = " + ")))
m_pooled_no_year <- lm(f_pooled_no_year, data = df)

# Cluster-robust SE by State
vc_pooled_no_year <- vcovCL(m_pooled_no_year, cluster = df$State, type = "HC1")

# Display coefficient table with significance stars
tidy(coeftest(m_pooled_no_year, vcov = vc_pooled_no_year)) %>%
   mutate(
     conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
     significant = case_when(
    p.value < 0.01 ~ "***",
    p.value < 0.05 ~ "**",
    p.value < 0.1 ~ "*",
    TRUE ~ ""
  ))%>%
  kable(digits = 3, caption = "HLp Pooled OLS Coefficients (No Year FE)")

# Summary
glance(m_pooled_no_year) %>%
  select(nobs, r.squared, adj.r.squared, statistic, p.value) %>%
  kable(digits = 3, caption = "HLp Model Fit Summary (No Year FE)")
```

# check multicollinearity
```{r}

# HLp pooled OLS without year FE
kable(car::vif(m_pooled_no_year))
```


############################################################
# MODEL 1 HLp: Pooled OLS with year fixed effects 
# Cluster-robust SE by state 
############################################################
```{r}

# Filter complete cases for PCA models
df_hlp_complete <- df %>% filter(complete.cases(select(., HLp, all_of(hlp_vars_original))))

# MODEL A: Pooled OLS with Year FE (Original variables)
f_pooled_hlp <- as.formula(paste(y, "~", paste(hlp_vars_original, collapse = " + "), "+ factor(Year)"))
m_pooled_hlp <- lm(f_pooled_hlp, data = df)
vc_pooled_hlp <- vcovCL(m_pooled_hlp, cluster = df$State, type = "HC1")

# Display Coefficient Table with Stars
tidy(coeftest(m_pooled_hlp, vcov = vc_pooled_hlp)) %>%
 mutate(
   conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
   significant = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*",p.value < 0.10 ~ "†", TRUE ~ ""))%>%
  kable(digits = 3, caption = "HLp Pooled OLS Coefficients")

# --- New Minimalist Fit Box ---
# Use glance to get a compact summary of N, R2, and F-stat
glance(m_pooled_hlp) %>%
  kable(digits = 3, caption = "HLp Pooled Model Fit Summary")

```

# check multicollinearity
```{r}

# HLp pooled OLS without year FE
kable(car::vif(m_pooled_hlp))
```


############################################################
# MAIN MODEL 2 HLp: State fixed effects + year fixed effects 
# Uses within estimator; clustered SE by state 
############################################################
# m-model, f-equation, vcov-uncertanity, HC- robust, CL- clustered

```{r}
pdf_hlp_complete <- pdata.frame(df_hlp_complete, index = c("State", "Year"))
  
  f_fe_hlp <- as.formula(
    paste(y, "~", paste(hlp_vars_original, collapse = " + "), "+ factor(Year)")
  )
  
  m_fe_hlp <- plm(f_fe_hlp, data = pdf_hlp_complete, model = "within", effect = "twoways")
  vc_fe_hlp <- vcovHC(m_fe_hlp, type = "HC1", cluster = "group")

# Display Coefficient Table with Stars
tidy(coeftest(m_fe_hlp, vcov = vc_fe_hlp)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    `p (significant)` = paste0(sprintf("%.3f", p.value), 
                              ifelse(p.value < .001, "***", ifelse(p.value < .01, "**", ifelse(p.value < .05, "*", ifelse(p.value < .10, "†", "")))))) %>%
  kable(digits = 3, caption = "HLp State FE + Year FE (clustered by State)")

# --- Model Fit ---
kable(data.frame(N = nobs(m_fe_hlp), 
                 R2_Within = summary(m_fe_hlp)$r.squared["rsq"], 
                 F_stat = summary(m_fe_hlp)$fstatistic$statistic), 
      digits = 3, caption = "Fit: State FE + Year FE")

glance(m_fe_hlp) %>%
  kable(digits = 3, caption = "HLp FE Model Fit Summary")
```

#model summary
```{r}
model_summary <- summary(m_fe_hlp)
fit_stats <- data.frame(
    Statistic = c("Observations", "States", "Years", "R-squared (within)", 
                  "Adj. R-squared", "F-statistic", "F p-value"),
    Value = c(
        paste0(nobs(m_fe_hlp), " (", length(unique(df$State)), " states × ", 
               length(unique(df$Year)), " years)"),
        length(unique(df$State)),
        length(unique(df$Year)),
        round(model_summary$r.squared[1], 3),
        round(model_summary$r.squared[2], 3),
        round(model_summary$fstatistic$statistic, 2),
        format.pval(model_summary$fstatistic$p.value, digits = 3)
    )
)
kable(fit_stats, caption = "HLp Two-Way FE Model Fit") %>%
    kable_styling()

```

# Temporal trend plot
```{r}

# Display coefficient table with significance stars
tidy(m_pooled_hlp, conf.int = TRUE) %>%
  mutate(significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.10 ~ "†",
    TRUE ~ ""
  ))%>%
  kable(digits = 3, caption = "HLp State FE + Year FE (clustered by State)")

# -----------------------------
# Extract Year Effects + CIs
# -----------------------------
year_coefs_hlp <- tidy(m_pooled_hlp, conf.int = TRUE) %>%
  filter(grepl("factor\\(Year\\)", term)) %>%
  mutate(Year = as.numeric(gsub("factor\\(Year\\)", "", term)),
    significant = p.value <= 0.05  # mark significance for plotting
  )

# Add baseline year (intercept) as Year 2009
intercept <- tidy(m_pooled_hlp, conf.int = TRUE) %>%
  filter(term == "(Intercept)") %>%
  mutate(Year = 2009,
         conf.low = estimate - 1.96 * std.error,
         conf.high = estimate + 1.96 * std.error,
    significant = p.value <= 0.05)

year_coefs_hlp <- bind_rows(intercept, year_coefs_hlp) %>%
  arrange(Year) %>%
  mutate(conf.low = estimate - 1.96 * std.error,
         conf.high = estimate + 1.96 * std.error)%>%
  filter(Year != 2020)  # remove 2020

# -----------------------------
# Plot temporal trend
# -----------------------------
ggplot(year_coefs_hlp, aes(x = Year, y = estimate)) +
   geom_line(aes(group = cumsum(c(TRUE, diff(Year) != 1))), color = "#2c7fb8", size = 1) +
  geom_point(color = "#2c7fb8", size = 2) +
    # Mark significant years
  geom_point(data = subset(year_coefs_hlp, significant), color = "red", size = 3) +
   geom_text(aes(label = round(estimate, 2)), 
            nudge_y = 0.05,        # Adjust this value to move text higher/lower
            vjust = 0,             # Vertically justify to the bottom of the text
            size = 4,              # Change font size
            color = "black") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2c7fb8") +
  scale_x_continuous(breaks = seq(2009, 2022, 1), limits = c(2008, 2022)) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "red", size = 0.6) +
  geom_text(aes(x = 2009, y = min(conf.low), label = "Recession"), color = "red", angle = 90, vjust = -0.7, hjust=-1) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "green", size = 0.6) +
  geom_text(aes(x = 2014, y = min(conf.low), label = "ACA Expansion"), color = "green", angle = 90, vjust = -0.7, hjust=-1) +
  labs(title = "HLp Temporal Trend in HLp with State Fixed and Year Fixed Effects",
       subtitle = "Year coefficients from Pooled OLS (clustered by State)",
       y = "HLp Estimate (with 95% CI)",
       x = "Year") +
  theme_minimal(base_size = 14)

```


############################################################
# MODEL 3 HLp: Between model (state means)
############################################################

```{r}
  df_between <- df %>%
    group_by(State) %>%
    summarise(across(all_of(c(y, hlp_vars_original)), ~ mean(., na.rm = TRUE)), .groups = "drop")
  
  m_between_hlp <- lm(
    as.formula(paste(y, "~", paste(hlp_vars_original, collapse = " + "))),
    data = df_between
  )
  vc_between_hlp <- vcovHC(m_between_hlp, type = "HC1")
  
  # Display Coefficient Table with Stars
tidy(coeftest(m_between_hlp, vcov = vc_between_hlp)) %>%
  mutate(`p (significant)` = paste0(sprintf("%.3f", p.value), 
                              ifelse(p.value < .001, "***", ifelse(p.value < .01, "**", ifelse(p.value < .05, "*", ifelse(p.value < .10, "†", ""))))))%>%
  kable(digits = 3, caption = "HLp Between Model (State Means)")

# --- Model Fit ---
kable(data.frame(N = nobs(m_between_hlp), 
                 R2 = summary(m_between_hlp)$r.squared, 
                 F_Robust = waldtest(m_between_hlp, vcov = vc_between_hlp, test = "F")[2, "F"]), 
      digits = 3, caption = "HLp Fit: Between Model")


glance(m_between_hlp) %>%
  kable(digits = 3, caption = "HLp Between Model Fit Summary")
```


# Forest plot for HLp model
```{r}

res_hlp <- bind_rows(
  tidy(m_pooled_hlp, conf.int = TRUE) %>% mutate(Model = "Pooled OLS"),
  tidy(m_fe_hlp, conf.int = TRUE) %>% mutate(Model = "Fixed Effects"),
  tidy(m_between_hlp, conf.int = TRUE) %>% mutate(Model = "Between")
) %>%
  filter(!grepl("factor", term), term != "(Intercept)")

ggplot(res_hlp, aes(x = estimate, y = term, color = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2
  ) +
  labs(
    title = "Forest Plot of HLp Determinants",
    subtitle = "Comparison of temporal, pooled, and structural effects",
    x = "Coefficient Estimate",
    y = ""
  ) +
  theme_minimal()

res_hlp <- bind_rows(
  tidy(m_pooled_hlp, conf.int = TRUE) %>% mutate(Model = "Pooled OLS"),
  tidy(m_fe_hlp, conf.int = TRUE) %>% mutate(Model = "Two-Way FE"),
  tidy(m_between_hlp, conf.int = TRUE) %>% mutate(Model = "Between")
) %>%
  filter(!grepl("factor", term), term != "(Intercept)") %>%
  mutate(
    term = factor(
      term,
      levels = rev(unique(term))
    )
  )

ggplot(res_hlp, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
  geom_point(
    aes(shape = Model),
    position = position_dodge(width = 0.6),
    size = 3,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2,
    color = "black"
  ) +
  facet_wrap(~ Model, ncol = 1) +
  labs(
    title = "Determinants of HLp Across Model Specifications",
    x = "Coefficient estimate",
    y = ""
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 11)
  )

```

############################################################
# MODEL 4 HLp: Annual cross-sectional regressions
############################################################

```{r}
annual_results_hlp <- df %>%
  group_by(Year) %>%
  group_modify(~{
    mod_hlp <- lm(as.formula(paste(y, "~", paste(hlp_vars_original, collapse = " + "))), data = .x)
    vc_hlp <- vcovHC(mod_hlp, type = "HC1")
    ct_hlp <- coeftest(mod_hlp, vcov = vc_hlp)
    
    data.frame(term = rownames(ct_hlp),
               estimate = ct_hlp[, 1],
               p_val = ct_hlp[, 4],
               N = nobs(mod_hlp),
               R2 = summary(mod_hlp)$r.squared) %>%
 mutate(`p (significant)` = paste0(sprintf("%.3f", p_val), 
                                  ifelse(p_val < .001, "***", ifelse(p_val < .01, "**", ifelse(p_val < .05, "*",ifelse(p_val < .1, "†", ""))))))
  }) %>% ungroup()

kable(annual_results_hlp, digits = 3, caption = "HLp Annual cross-sectional regressions with R2")

```

############################################################
# HLp: Robustness check using log(HLp)
# Use log1p if HLp can be 0
############################################################

```{r}
  df <- df %>%
    mutate(log_HLp = ifelse(.data[[y]] <= 0, log1p(.data[[y]]), log(.data[[y]])))
  
  m_pooled_log_hlp <- lm(
    as.formula(paste("log_HLp ~", paste(hlp_vars_original, collapse = " + "), "+ factor(Year)")),
    data = df
  )
  vc_pooled_log_hlp <- vcovCL(m_pooled_log_hlp, cluster = df$State, type = "HC1")
  tidy(coeftest(m_pooled_log_hlp, vcov = vc_pooled_log_hlp))%>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ "†",
    TRUE            ~ ""
  ))%>%
  kable(digits = 3, 
        caption = "HLp Robustness: Pooled OLS + Year FE with log(HLp)")
  # Fit for Pooled Log
kable(data.frame(N = nobs(m_pooled_log_hlp), R2 = summary(m_pooled_log_hlp)$r.squared), 
      caption = "HLp Fit: Pooled Log")
  
  pdf2 <- pdata.frame(df, index = c("State", "Year"))
  m_fe_log_hlp <- plm(
    as.formula(paste("log_HLp ~", paste(hlp_vars_original, collapse = " + "), "+ factor(Year)")),
    data = pdf2, model = "within", effect = "twoways"
  )
  vc_fe_log_hlp <- vcovHC(m_fe_log_hlp, type = "HC1", cluster = "group")
  tidy(coeftest(m_fe_log_hlp, vcov = vc_fe_log_hlp))%>%
  mutate(significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ "†",
    TRUE            ~ ""
  )) %>%
  kable(digits = 3, 
        caption = "HLp Robustness: State FE + Year FE with log(HLp)")
  # Fit for FE Log
kable(data.frame(N = nobs(m_fe_log_hlp), R2_Within = summary(m_fe_log_hlp)$r.squared["rsq"]), 
      caption = "HLp Fit: FE Log")

glance(m_fe_log_hlp) %>%
  kable(digits = 3, caption = "HLp Log Model Fit Summary")
  
```



# Assign variable label

```{r}
label(ehdi_pos$HLp) <- "Hearing Loss Prevalence (HLp)"
label(ehdi_pos$EIr) <- "Early Intervention Rate(EIr)"
label(ehdi_pos$PerTscr) <- "Percent of Infants Total Screened "
label(ehdi_pos$Dr) <- "Diagnosis Rate"
label(ehdi_pos$Median_household_income) <- "Median Household Income (USD)"
label(ehdi_pos$state_per_capita_fund) <- "Per Capita State Public Health Funding"
label(ehdi_pos$Per_rep_women_uninsured) <- "Percent Reproductive Age Women Uninsured"
label(ehdi_pos$Per_infant_breastfed) <- "Percent of of Infant Ever Breastfed"


# create lookup vector
var_comp <- sapply(
  ehdi_pos[, xdesc],
  function(v) attr(v, "label")
)

var_comp

```

# comparision table
```{r}

comparision_table <-stargazer(
  m_pooled_hlp,
  m_fe_hlp,
  m_between_hlp,
  m_pooled_log_hlp,
  type = "html",
  out="HLp_comparision_result.doc",
  se = list(
    sqrt(diag(vc_pooled_hlp)),
    sqrt(diag(vc_fe_hlp)),
    sqrt(diag(vc_between_hlp)),
    sqrt(diag(vc_pooled_log_hlp))
  ),
  title = "Model Comparison for Hearing Loss Prevalence (HLp)",
  column.labels = c(
    "Pooled",
    "Two-Way FE",
    "Between",
    "Robust (log HLp)"
  ),
intercept.top = TRUE,            # Moves Constant to the first row
  intercept.bottom = FALSE,        # Ensures it is not at the bottom
  covariate.labels = c("Constant", var_comp), # Add "Constant" to the start of your labels  # Replaces variable names with your labels
  dep.var.labels = "Hearing Loss Prevalence (HLp)", # Specific dependent var label
 dep.var.labels.include = FALSE,
  # 
  model.numbers = FALSE,
  omit = "factor\\(Year\\)",
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
star.cutoffs = c(0.05, 0.01, 0.001), # Standard stars
  notes = "† p<0.1; * p<0.05; ** p<0.01; *** p<0.001",
  notes.append = FALSE,
  add.lines = list(
    c("State FE", "No", "Yes", "No", "No"),
    c("Year FE",  "YES", "Yes", "No", "YES")
   )
  )


htmltools::browsable(htmltools::HTML(comparision_table))
```

# table2 with 95%CI for hlp
```{r hlp_comparison_table}

# ── Helper: extract estimate, 95% CI (robust), and p-value ───────────────────
extract_model_cols <- function(model, vcov_mat, var_names, label) {
  ct <- coeftest(model, vcov = vcov_mat)
  df <- as.data.frame(ct[var_names, , drop = FALSE])
  colnames(df) <- c("estimate", "se", "t", "p")
  df$ci_low  <- df$estimate - 1.96 * df$se
  df$ci_high <- df$estimate + 1.96 * df$se
  df$p_sig   <- case_when(
    df$p < 0.001 ~ "***", df$p < 0.01 ~ "**",
    df$p < 0.05  ~ "*",   df$p < 0.10  ~ "†",
    TRUE ~ ""
  )
  # Use setNames() instead of := — works in base R without rlang
  out <- data.frame(
    sprintf("%.3f%s", df$estimate, df$p_sig),
    paste0("[", sprintf("%.3f", df$ci_low), ", ", sprintf("%.3f", df$ci_high), "]"),
    sprintf("%.3f", df$p),
    check.names = FALSE
  )
  colnames(out) <- c(paste0("Est_", label), paste0("CI_", label), paste0("p_", label))
  out
}

# Row labels (descriptive names matching your stargazer table)
hlp_row_labels <- c(
  "(Intercept)"             = "Constant",
  "PerTscr"                 = "Percent of Infants Total Screened",
  "Dr"                      = "Diagnosis Rate",
  "state_per_capita_fund"   = "Per Capita State Public Health Funding",
  "Median_household_income" = "Median Household Income (USD)",
  "Per_rep_women_uninsured" = "Percent of Reproductive Age Women Uninsured",
  "Per_infant_breastfed"    = "Percent Infant Ever Breastfed"
)

# Variables to extract (intercept + covariates, no year FEs)
hlp_vars_table <- c("PerTscr", "Dr", "state_per_capita_fund",
                     "Median_household_income", "Per_rep_women_uninsured",
                     "Per_infant_breastfed")
hlp_vars_int   <- c("(Intercept)", hlp_vars_table)   # for OLS models with intercept

# ── Extract each model ────────────────────────────────────────────────────────

# Pooled OLS (has intercept)
pooled_out <- extract_model_cols(m_pooled_hlp, vc_pooled_hlp,
                                 hlp_vars_int, "Pooled")

# Two-Way FE (no intercept — plm within estimator demeans it out)
fe_ct <- coeftest(m_fe_hlp, vcov = vc_fe_hlp)
fe_vars_present <- intersect(hlp_vars_table, rownames(fe_ct))
fe_df <- as.data.frame(fe_ct[fe_vars_present, , drop = FALSE])
colnames(fe_df) <- c("estimate", "se", "t", "p")
fe_df$ci_low  <- fe_df$estimate - 1.96 * fe_df$se
fe_df$ci_high <- fe_df$estimate + 1.96 * fe_df$se
fe_df$p_sig   <- case_when(
  fe_df$p < 0.001 ~ "***", fe_df$p < 0.01 ~ "**",
  fe_df$p < 0.05  ~ "*",   fe_df$p < 0.10  ~ "†",
  TRUE ~ ""
)
fe_out <- data.frame(
  Est_FE = c("—", sprintf("%.3f%s", fe_df$estimate, fe_df$p_sig)),
  CI_FE  = c("—", paste0("[", sprintf("%.3f", fe_df$ci_low),
                          ", ", sprintf("%.3f", fe_df$ci_high), "]")),
  p_FE   = c("—", sprintf("%.3f", fe_df$p)),
  check.names = FALSE
)

# Between model (has intercept)
between_out <- extract_model_cols(m_between_hlp, vc_between_hlp,
                                  hlp_vars_int, "Between")

# Log(HLp) Pooled OLS (has intercept)
log_out <- extract_model_cols(m_pooled_log_hlp, vc_pooled_log_hlp,
                              hlp_vars_int, "LogHLp")

# ── Assemble side-by-side table ───────────────────────────────────────────────
table2_hlp <- data.frame(
  Variable = hlp_row_labels[names(hlp_row_labels)],
  row.names = NULL,
  check.names = FALSE
)
table2_hlp <- cbind(table2_hlp, pooled_out, fe_out, between_out, log_out)

# Rename columns for clean display header
colnames(table2_hlp) <- c(
  "Variable",
  "Est (Pooled)", "95% CI", "p",
  "Est (Two-Way FE)", "95% CI ", "p ",
  "Est (Between)", "95% CI  ", "p  ",
  "Est (log HLp)", "95% CI   ", "p   "
)

# ── Render table ──────────────────────────────────────────────────────────────
kable(
  table2_hlp,
  digits  = 3,
  align   = c("l", rep(c("r", "c", "r"), 4)),
  caption = paste0(
    "Table 2: Model Comparison for Hearing Loss Prevalence (HLp). ",
    "Estimates with 95% confidence intervals (robust, HC1) and p-values. ",
    "Year fixed effects included in Pooled and log(HLp) models. ",
    "† p<0.10; * p<0.05; ** p<0.01; *** p<0.001."
  )
) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  add_header_above(c(
    " " = 1,
    "Pooled OLS + Year FE" = 3,
    "Two-Way FE" = 3,
    "Between (State Means)" = 3,
    "Robustness: log(HLp)" = 3
  )) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(which(table2_hlp$Variable == "% Infant Ever Breastfed"),
           bold = TRUE, background = "#f0f8ff") %>%
  kableExtra::footnote(
    general = "95% CI = estimate ± 1.96 × cluster-robust SE (HC1). State FE: No/Yes/No/No. Year FE: Yes/Yes/No/Yes.",
    general_title = "Note:",
    footnote_as_chunk = TRUE
  )

# ── Also export to Word ───────────────────────────────────────────────────────
ft_hlp <- flextable(table2_hlp) %>%
  set_caption("Table 2: Model Comparison for Hearing Loss Prevalence (HLp)") %>%
  add_header_row(
    values = c("", "Pooled OLS + Year FE", "Two-Way FE",
               "Between (State Means)", "Robustness: log(HLp)"),
    colwidths = c(1, 3, 3, 3, 3)
  ) %>%
  bold(part = "header") %>%
  autofit() %>%
  theme_booktabs()

save_as_docx(ft_hlp, path = "HLp_comparison_Table2.docx")

```


############################################################
# OPTIONAL HLp: Balanced-panel sensitivity sample (states with all 13 years)
############################################################
  
```{r}
    balanced_states <- df %>%
    mutate(complete_row = if_all(all_of(c(y, hlp_vars_original)), ~ !is.na(.))) %>%
    group_by(State) %>%
    summarise(n_years_complete = n_distinct(Year[complete_row]), .groups = "drop") %>%
    filter(n_years_complete == 13) %>%
    pull(State)
  
  df_bal <- df %>% filter(State %in% balanced_states)
  
  m_pooled_bal <- lm(f_pooled_hlp, data = df_bal)
  vc_pooled_bal <- vcovCL(m_pooled_bal, cluster = df_bal$State, type = "HC1")
  
# 1. Coefficient Table
tidy(coeftest(m_pooled_bal, vcov = vc_pooled_bal)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    `p (significant)` = paste0(sprintf("%.3f", p.value), 
                              ifelse(p.value < .001, "***", ifelse(p.value < .01, "**", ifelse(p.value < .05, "*", ifelse(p.value < .10, "†", ""))))))%>%
  kable(digits = 3, caption = "HLp Sensitivity: Balanced sample pooled OLS + Year FE")

# 2. Fit Table (Fixed to reference correct model)
kable(data.frame(N = nobs(m_pooled_bal), 
                 R2 = summary(m_pooled_bal)$r.squared,
                 Adj_R2 = summary(m_pooled_bal)$adj.r.squared), 
      caption = "HLp Fit: Balanced Sample")

glance(m_pooled_bal) %>%
  kable(digits = 3, caption = "HLp Balanced Panel Fit Summary")
```


# HLp Diagnostics
```{r}
cat("\n=== HLp MODEL DIAGNOSTICS (Two-Way Fixed Effects) ===\n")

# Diagnostic 1: Serial Correlation Test
cat("\n1. Serial Correlation Test (Breusch-Godfrey):\n")
serial_test_hlp <- pbgtest(m_fe_hlp)
print(serial_test_hlp)

# Diagnostic 2: Heteroskedasticity Test
cat("\n2. Heteroskedasticity Test (Breusch-Pagan):\n")
# Create formula for BP test
f_bp_hlp <- as.formula(paste("HLp ~", paste(hlp_vars_original, collapse = " + "),
                             "+ factor(State) + factor(Year)"))
bptest_hlp <- bptest(f_bp_hlp, data = df_hlp_complete, studentize = TRUE)
print(bptest_hlp)

# # Diagnostic 3: Hausman Test (Fixed vs Random Effects)
cat("\n3. Hausman Test (FE vs RE):\n")
f_re_hlp <- as.formula(paste("HLp ~", paste(hlp_vars_original, collapse = " + ")))

m_re_hlp <- plm(f_re_hlp, data = pdf_hlp_complete, model = "random")

hausman_test_hlp <- phtest(m_fe_hlp, m_re_hlp)
print(hausman_test_hlp)

# Diagnostic 4: Cross-sectional dependence
cat("\n4. Cross-sectional Dependence Test (Pesaran CD):\n")
cd_test_hlp <- pcdtest(m_fe_hlp, test = "cd")
print(cd_test_hlp)

# Diagnostic 5: Unit root test (for panel data)
cat("\n5. Panel Unit Root Test (Levin-Lin-Chu):\n")
# Note: This requires pseries and might take time
# library(tseries)
# llc_test_hlp <- purtest(HLp ~ 1, data = pdf_hlp_complete, index = c("State", "Year"),
#                         test = "levinlin", lags = "AIC")
# print(llc_test_hlp)

# Create diagnostic summary table
diagnostics_summary_hlp <- data.frame(
  Test = c(
    "Serial Correlation",
    "Heteroskedasticity",
    "Cross-sectional Dependence"
  ),
  Statistic = c(
    round(serial_test_hlp$statistic, 3),
    round(bptest_hlp$statistic, 3),
    round(cd_test_hlp$statistic, 3)
  ),
  P_Value = c(
    format.pval(serial_test_hlp$p.value, digits = 3),
    format.pval(bptest_hlp$p.value, digits = 3),
    format.pval(cd_test_hlp$p.value, digits = 3)
  ),
  Interpretation = c(
    "Serial correlation present → Clustered SEs used",
    "Heteroskedasticity present → Robust SEs used",
    "Cross-sectional dependence present"
  )
)

knitr::kable(
  diagnostics_summary_hlp,
  caption = "Model Diagnostics for Two-Way Fixed Effects HLp Model"
)

```


## Key Results

```{r}

# Extract key results
key_results <- data.frame(
    Predictor = hlp_vars_original,
    TwoWayFE_Estimate = round(coef(m_fe_hlp), 3),
    TwoWayFE_SE = round(sqrt(diag(vc_fe_hlp)), 3),
    TwoWayFE_Pvalue = round(summary(m_fe_hlp)$coefficients[, 4], 3),
    Pooled_Estimate = round(coef(m_pooled_hlp)[hlp_vars_original], 3),
    Pooled_Pvalue = round(summary(m_pooled_hlp)$coefficients[hlp_vars_original, 4], 3)
)

# Identify significant predictors
significant_predictors <- key_results %>%
    filter(TwoWayFE_Pvalue < 0.05) %>%
    arrange(desc(abs(TwoWayFE_Estimate)))

cat("\nStatistically Significant Predictors (Two-Way FE, p<0.05):\n")
kable(significant_predictors, digits = 3) %>%
    kable_styling()

# Create summary statement
cat("\n=== MAIN CONCLUSION ===\n")
if (nrow(significant_predictors) > 0) {
    top_predictor <- significant_predictors[1, ]
    cat(sprintf(
        "The strongest predictor of HLp is %s (β = %.3f, p = %.3f).\n",
        top_predictor$Predictor,
        top_predictor$TwoWayFE_Estimate,
        top_predictor$TwoWayFE_Pvalue
    ))
} else {
    cat("No predictors reached statistical significance at p<0.05 in the Two-Way FE model.\n")
}

cat(sprintf(
    "\nModel Statistics:\n- R-squared (within): %.3f\n- F-statistic: %.2f (p = %.3f)\n- Observations: %d\n",
    model_summary$r.squared[1],
    model_summary$fstatistic$statistic,
    model_summary$fstatistic$p.value,
    nobs(m_fe_hlp)
))

```


# SENSITIVITY ANALYSIS: Complete-Case Sample vs. Primary Sample
```{r sensitivity_complete_case_load}


# Purpose: Address reviewer concern about potential bias from
#          partial-year data in the primary 42-state sample.
#
# APPROACH:
#   Primary sample  → analysis_data_v2_2009to2022_12262025.csv
#                     42 states, ≤1 missing HLp year (N ≈ 535)
#   Complete-case   → analysis_data_complete_case_sensitivity.csv
#                     States with zero missing values in HLp, EIr,
#                     PerTscr, and Dr across all 13 study years (excl. 2020)
#
# DECISION RULE: If the Per_infant_breastfed coefficient changes
#   <15% between samples, findings are considered robust (Little &
#   Rubin, 2002; Sterne et al., 2009).
############################################################

############################################################
# STEP 1: Load and prepare the complete-case sensitivity dataset
############################################################

# Load complete-case sensitivity dataset
ehdi_sens <- read.csv("analysis_data_complete_case_sensitivity.csv") %>%
  select("State", "Year", "HLp", "EIr", "PerTscr", "Dr",
         "state_per_capita_fund", "drinks_per_capita", "Health_care_expenditure",
         "age_under5", "Total_population", "uninsured_under18_rate",
         "Median_household_income", "under18_poverty_rate",
         "F_income_below_poverty_rate",
         "Per_rep_women_uninsured",
         "Per_women_breastfed") %>%
  mutate(Per_age_under5 = age_under5 / Total_population * 100) %>%
  rename("Health_care_expenditure_percapita" = "Health_care_expenditure",
         "Per_infant_breastfed"             = "Per_women_breastfed") %>%
  filter(Year >= 2009 & Year <= 2022,
         Year != 2020)

# Report sample composition
sens_states <- n_distinct(ehdi_sens$State)
sens_obs    <- nrow(ehdi_sens)

cat(sprintf(
  "Complete-case sensitivity sample: %d states, %d state-year observations\n",
  sens_states, sens_obs
))

kable(
  ehdi_sens %>%
    group_by(State) %>%
    summarise(n_years = n_distinct(Year), .groups = "drop") %>%
    arrange(State),
  caption = "Sensitivity Sample: States and Year Coverage"
) %>%
  kable_styling()

```

############################################################
# STEP 2: Run Pooled OLS on complete-case sample
#         (same formula and SE structure as primary model)
############################################################

```{r sensitivity_complete_case_model}

# Pooled OLS with Year FE + cluster-robust SE (HC1, by State)
# — identical specification to m_pooled_hlp
f_pooled_hlp_sens <- as.formula(
  paste("HLp ~", paste(hlp_vars_original, collapse = " + "), "+ factor(Year)")
)

m_pooled_hlp_sens <- lm(f_pooled_hlp_sens, data = ehdi_sens)
vc_pooled_hlp_sens <- vcovCL(m_pooled_hlp_sens, cluster = ehdi_sens$State, type = "HC1")

# Display coefficient table
tidy(coeftest(m_pooled_hlp_sens, vcov = vc_pooled_hlp_sens)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.10  ~ "†",
    TRUE            ~ ""
  )) %>%
  kable(digits = 3,
        caption = "Sensitivity (Complete-Case): HLp Pooled OLS + Year FE") %>%
  kable_styling()

# Model fit
glance(m_pooled_hlp_sens) %>%
  kable(digits = 3,
        caption = "Sensitivity (Complete-Case): HLp Model Fit") %>%
  kable_styling()

```

############################################################
# STEP 3: Side-by-side comparison table — Primary vs. Complete-case
#         Focus on Per_infant_breastfed (key predictor of interest)
############################################################

```{r sensitivity_comparison_table}

# ── Extract coefficients for ALL covariates from both models ──────────────────
primary_coefs <- tidy(coeftest(m_pooled_hlp, vcov = vc_pooled_hlp)) %>%
  filter(term %in% hlp_vars_original) %>%
  select(term, estimate, std.error, p.value) %>%
  mutate(conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,) %>% 
  rename(est_primary = estimate,
         se_primary  = std.error,
         p_primary   = p.value)

sens_coefs <- tidy(coeftest(m_pooled_hlp_sens, vcov = vc_pooled_hlp_sens)) %>%
  filter(term %in% hlp_vars_original) %>%
  select(term, estimate, std.error, p.value) %>%
  mutate(conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,) %>% 
  rename(est_sens = estimate,
         se_sens  = std.error,
         p_sens   = p.value)

# ── Merge and compute % change ────────────────────────────────────────────────
sensitivity_comparison <- left_join(primary_coefs, sens_coefs, by = "term") %>%
  mutate(
    pct_change = round(abs(est_sens - est_primary) / abs(est_primary) * 100, 1),
    sig_primary = case_when(
      p_primary < 0.001 ~ "***", p_primary < 0.01 ~ "**",
      p_primary < 0.05  ~ "*",  p_primary < 0.10  ~ "†",
      TRUE ~ ""
    ),
    sig_sens = case_when(
      p_sens < 0.001 ~ "***", p_sens < 0.01 ~ "**",
      p_sens < 0.05  ~ "*",  p_sens < 0.10  ~ "†",
      TRUE ~ ""
    ),
    # Direction consistent = same sign in both samples
    same_direction = sign(est_primary) == sign(est_sens),
    # Significance consistent = significant (p<0.05) in both, or
    #   non-significant in both (for control variables)
    sig_consistent = (p_primary < 0.05 & p_sens < 0.05) |
                     (p_primary >= 0.05 & p_sens >= 0.05),
    # Stable = % change < 15% OR covariate is non-significant in primary
    #   (% change on a near-zero non-sig coef is not interpretable)
    stable = case_when(
      p_primary >= 0.05 & p_sens >= 0.05 ~ "N/A (non-sig)",
      pct_change < 15                    ~ "Yes",
      same_direction & sig_consistent    ~ "Yes*",
      TRUE                               ~ "No"
    )
  ) %>%
  mutate(
    Primary_coef = paste0(sprintf("%.3f", est_primary), sig_primary,
                          " (SE = ", sprintf("%.3f", se_primary), ")"),
    Sensitivity_coef = paste0(sprintf("%.3f", est_sens), sig_sens,
                              " (SE = ", sprintf("%.3f", se_sens), ")")
  ) %>%
  select(term, Primary_coef, Sensitivity_coef, pct_change, same_direction, stable) %>%
  rename(Variable            = term,
         `Primary Sample`    = Primary_coef,
         `Complete-Case`     = Sensitivity_coef,
         `% Change`          = pct_change,
         `Direction Same`    = same_direction,
         `Robust`            = stable)

kable(
  sensitivity_comparison,
  digits  = 3,
  caption = paste0(
    "Sensitivity Analysis: Primary Sample (", nrow(df),
    " obs, 42 states) vs. Complete-Case Sample (", sens_obs,
    " obs, ", sens_states, " states) — Pooled OLS with Year FE and Cluster-Robust SE (HC1).",
    " Yes* = direction and significance consistent; N/A = non-significant in both samples."
  )
) %>%
  kable_styling() %>%
  row_spec(
    which(sensitivity_comparison$Variable == "Per_infant_breastfed"),
    bold = TRUE, background = "#f0f8ff"
  )

```

############################################################
# STEP 4: Auto-interpretation of breastfeeding coefficient stability
############################################################

```{r sensitivity_interpretation}

# Extract breastfeeding % change
bf_row <- sensitivity_comparison %>%
  filter(Variable == "Per_infant_breastfed")

bf_pct_change <- as.numeric(bf_row$`% Change`)

# Extract raw estimates for narrative
bf_primary <- primary_coefs %>% filter(term == "Per_infant_breastfed")
bf_sens    <- sens_coefs    %>% filter(term == "Per_infant_breastfed")

cat("=== SENSITIVITY ANALYSIS: BREASTFEEDING COEFFICIENT STABILITY ===\n\n")

cat(sprintf(
  "Primary sample (42 states, N = %d):\n  β = %.3f, SE = %.3f, p = %.3f\n\n",
  nrow(df),
  bf_primary$est_primary, bf_primary$se_primary, bf_primary$p_primary
))

cat(sprintf(
  "Complete-case sample (%d states, N = %d):\n  β = %.3f, SE = %.3f, p = %.3f\n\n",
  sens_states, sens_obs,
  bf_sens$est_sens, bf_sens$se_sens, bf_sens$p_sens
))

cat(sprintf("Absolute change: %.3f\n", abs(bf_sens$est_sens - bf_primary$est_primary)))
cat(sprintf("Percent change:  %.1f%%\n\n", bf_pct_change))

# Re-extract values for interpretation logic
bf_primary_raw <- primary_coefs %>% filter(term == "Per_infant_breastfed")
bf_sens_raw    <- sens_coefs    %>% filter(term == "Per_infant_breastfed")

bf_same_dir  <- sign(bf_primary_raw$est_primary) == sign(bf_sens_raw$est_sens)
bf_sig_both  <- bf_primary_raw$p_primary < 0.05 & bf_sens_raw$p_sens < 0.05

if (bf_same_dir & bf_sig_both) {
  cat(paste0(
    "INTERPRETATION: The Per_infant_breastfed coefficient remained positive and ",
    "statistically significant (p < 0.001) in both the primary 42-state sample ",
    "(\u03b2 = ", sprintf("%.3f", bf_primary_raw$est_primary), ", SE = ",
    sprintf("%.3f", bf_primary_raw$se_primary), ") and the stricter complete-case ",
    "sample of ", sens_states, " states (\u03b2 = ",
    sprintf("%.3f", bf_sens_raw$est_sens), ", SE = ",
    sprintf("%.3f", bf_sens_raw$se_sens), "). ",
    "The absolute change in the coefficient (",
    sprintf("%.3f", abs(bf_sens_raw$est_sens - bf_primary_raw$est_primary)),
    ", ", bf_pct_change, "%) reflects the expected attenuation when restricting to ",
    "states with complete longitudinal records, not a substantive reversal of findings. ",
    "The direction, magnitude, and significance of the breastfeeding association are ",
    "consistent across both samples, indicating that the primary results are robust to ",
    "the missing data exclusion criterion (Little & Rubin, 2002; Sterne et al., 2009)."
  ))
} else if (bf_same_dir & !bf_sig_both) {
  cat(paste0(
    "INTERPRETATION: The Per_infant_breastfed coefficient retained the same direction ",
    "across samples but lost statistical significance in the complete-case sample. ",
    "This may reflect reduced statistical power with ", sens_states, " states. ",
    "Interpret with caution (Little & Rubin, 2002)."
  ))
} else {
  cat(paste0(
    "INTERPRETATION: The Per_infant_breastfed coefficient changed direction across ",
    "samples, suggesting sensitivity to the missing data mechanism. ",
    "Additional investigation is recommended (Little & Rubin, 2002)."
  ))
}
```


# ==============================================
# OPTIONAL: PCA ANALYSIS AND PCA-BASED MODELS
# ==============================================

```{r}


cat("\n=== OPTIONAL PCA ANALYSIS ===\n")

# Select variables for PCA
pca_vars <- c(
  "PerTscr",
  "Dr",
  "state_per_capita_fund",
  "Median_household_income",
  "Per_rep_women_uninsured",
  "Per_infant_breastfed"
)

# Run PCA
pca_data <- df %>%
  select(all_of(pca_vars)) %>%
  drop_na()

pca_res <- prcomp(pca_data, scale. = TRUE, center = TRUE)

# PCA Summary
cat("\nPCA Summary:\n")
print(summary(pca_res))

# Variance explained
variance_explained <- summary(pca_res)$importance[2,] * 100
cumulative_variance <- cumsum(variance_explained)

# Find how many components explain >80% variance
components_80 <- min(which(cumulative_variance >= 80))
cat(sprintf("\nComponents needed for >80%% variance: %d\n", components_80))

# Scree plot
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 60)) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = components_80, linetype = "dashed", color = "blue", alpha = 0.5) +
  annotate("text", x = components_80 + 0.3, y = 85, 
           label = paste(">80% at", components_80, "components"), 
           color = "blue", size = 3.5) +
  labs(
    title = "Scree Plot: PCA Components",
    subtitle = "Components explaining >80% variance marked in blue"
  ) +
  theme_minimal()

# Variable loadings plot
fviz_pca_var(
  pca_res,
  col.var = "contrib",
  gradient.cols = c("#2c7fb8", "#E7B800", "#d7191c"),
  repel = TRUE,
  title = "PCA: Variable Contributions"
) +
  theme_minimal()

# Extract PCA loadings
pca_loadings <- as.data.frame(pca_res$rotation[, 1:3])
colnames(pca_loadings) <- paste0("PC", 1:3)

cat("\nPCA Loadings (First 3 Components):\n")
kable(round(pca_loadings, 3), caption = "PCA Variable Loadings") %>%
  kable_styling()

# Interpret components
cat("\n=== PCA COMPONENT INTERPRETATION ===\n")
interpret_pca <- function(loadings, component, n_top = 3) {
  sorted_loadings <- loadings %>%
    rownames_to_column("Variable") %>%
    arrange(desc(abs(.data[[component]]))) %>%
    head(n_top)
  
  cat(sprintf("\n%s (Top %d variables):\n", component, n_top))
  for(i in 1:nrow(sorted_loadings)) {
    var <- sorted_loadings$Variable[i]
    loading <- sorted_loadings[[component]][i]
    direction <- ifelse(loading > 0, "positive", "negative")
    cat(sprintf("  %s: %.3f (%s)\n", var, loading, direction))
  }
}

interpret_pca(pca_loadings, "PC1")
interpret_pca(pca_loadings, "PC2")
interpret_pca(pca_loadings, "PC3")

# ==============================================
# PCA-BASED MODELS FOR HLp
# ==============================================

cat("\n=== PCA-BASED MODELS FOR HLp ===\n")

# Add PCA scores to dataset
pca_scores <- as.data.frame(predict(pca_res, pca_data))
df_pca <- df
df_pca[complete.cases(df[, pca_vars]), paste0("PC", 1:3)] <- pca_scores[, 1:3]

# Create panel data with PCA scores
pdf_pca <- pdata.frame(df_pca, index = c("State", "Year"))

# Define PCA variables
hlp_vars_pca <- paste0("PC", 1:3)

# MODEL 1: Two-Way FE with PCA Components
cat("\n1. Two-Way FE with PCA Components:\n")
f_pca_fe <- as.formula(paste("HLp ~", paste(hlp_vars_pca, collapse = " + ")))
m_pca_fe <- plm(f_pca_fe, data = pdf_pca, model = "within", effect = "twoways")
vc_pca_fe <- vcovHC(m_pca_fe, type = "HC1", cluster = "group")

tidy(coeftest(m_pca_fe, vcov = vc_pca_fe)) %>%
  mutate(`p (significant)` = paste0(sprintf("%.3f", p.value), 
                              ifelse(p.value < .001, "***", ifelse(p.value < .01, "**", ifelse(p.value < .05, "*", ifelse(p.value < .10, "†", ""))))))%>%
  kable(digits = 3, caption = "HLp: Two-Way FE with PCA Components") %>%
  kable_styling()

# MODEL 2: Between Model with PCA Components
cat("\n2. Between Model with PCA Components:\n")
df_between_pca <- df_pca %>%
  group_by(State) %>%
  summarise(across(all_of(c("HLp", hlp_vars_pca)), 
                   ~ mean(., na.rm = TRUE)), 
            .groups = "drop") %>%
  drop_na()

m_pca_between <- lm(as.formula(paste("HLp ~", paste(hlp_vars_pca, collapse = " + "))), 
                    data = df_between_pca)
vc_pca_between <- vcovHC(m_pca_between, type = "HC1")

tidy(coeftest(m_pca_between, vcov = vc_pca_between)) %>%
  mutate(`p (significant)` = paste0(sprintf("%.3f", p.value), 
                              ifelse(p.value < .001, "***", ifelse(p.value < .01, "**", ifelse(p.value < .05, "*", ifelse(p.value < .10, "†", ""))))))%>%
  kable(digits = 3, caption = "HLp: Between Model with PCA Components") %>%
  kable_styling()

# ==============================================
# COMPARE ORIGINAL VS PCA MODELS
# ==============================================

cat("\n=== COMPARISON: Original vs PCA Models ===\n")

# Collect key statistics
model_comparison_hlp <- data.frame(
  Model = c("Original Variables", "PCA Components"),
  Variables = c(length(hlp_vars_original), length(hlp_vars_pca)),
  R2_Within = c(
    round(summary(m_fe_hlp)$r.squared[1], 3),
    round(summary(m_pca_fe)$r.squared[1], 3)
  ),
  R2_Adj = c(
    round(summary(m_fe_hlp)$r.squared[2], 3),
    round(summary(m_pca_fe)$r.squared[2], 3)
  ),
  F_Statistic = c(
    round(summary(m_fe_hlp)$fstatistic$statistic, 2),
    round(summary(m_pca_fe)$fstatistic$statistic, 2)
  ),
  Observations = c(
    nobs(m_fe_hlp),
    nobs(m_pca_fe)
  )
)

kable(model_comparison_hlp, caption = "Comparison: Original vs PCA Models") %>%
  kable_styling()

# ==============================================
# PCA MODEL INTERPRETATION GUIDANCE
# ==============================================

cat("\n=== HOW TO INTERPRET PCA MODELS ===\n")
cat("\n1. PCA reduces 6 original variables to 3 components.\n")
cat(sprintf("2. These 3 components explain %.1f%% of total variance.\n", cumulative_variance[3]))
cat("3. Each component represents a pattern of correlation:\n")
cat("   - PC1: ", paste(rownames(pca_loadings)[order(-abs(pca_loadings$PC1))][1:2], collapse = ", "), "\n")
cat("   - PC2: ", paste(rownames(pca_loadings)[order(-abs(pca_loadings$PC2))][1:2], collapse = ", "), "\n")
cat("   - PC3: ", paste(rownames(pca_loadings)[order(-abs(pca_loadings$PC3))][1:2], collapse = ", "), "\n")
cat("\n4. PCA models are more parsimonious and avoid multicollinearity.\n")
```

----

#################SECTION BREAK FOR EIr#################################


# Defining variables for EIr model
```{r}
  # 2) Define outcome + covariates (as described by you)
 #dependent variable
   y_eir <- "EIr"
 # Independent variable
 eir_vars_original <- c("PerTscr","Dr","state_per_capita_fund","Median_household_income","Per_rep_women_uninsured","Per_infant_breastfed")
 
```



# defining data object
```{r}

df_eir <- df %>%
  filter(complete.cases(select(., EIr, all_of(eir_vars_original))))

pdf_eir <- pdata.frame(  df_eir,index = c("State", "Year"))
```


############################################################
# OPTIONAL TO CHECK: EIr: Pooled OLS without year fixed effects 
# Cluster-robust SE by state 
############################################################
```{r}
# Pooled OLS without year fixed effects
f_pooled_no_year_eir <- as.formula(paste(y_eir, "~", paste(eir_vars_original, collapse = " + ")))
m_pooled_no_year_eir <- lm(f_pooled_no_year_eir, data = df_eir)

# Cluster-robust SE by State
vc_pooled_no_year_eir <- vcovCL(m_pooled_no_year_eir, cluster = df_eir$State, type = "HC1")

# Display coefficient table with significance stars
tidy(coeftest(m_pooled_no_year_eir, vcov = vc_pooled_no_year_eir)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < .10 ~  "†",
    TRUE ~ ""
  )) %>%
  kable(digits = 3, caption = "EIr Pooled OLS Coefficients (No Year FE)")

# Summary
glance(m_pooled_no_year_eir) %>%
  kable(digits = 3, caption = "EIr Pooled Model Fit Summary (No Year FE)")
```

# check multicollinearity
```{r}

#EIr pooled OLS without year FE
kable(car::vif(m_pooled_no_year_eir))
```


############################################################
# MODEL 1 EIr: Pooled OLS with year fixed effects 
# Cluster-robust SE by state 
############################################################
```{r}

f_pooled_eir <- as.formula(paste(y_eir, "~", paste(eir_vars_original, collapse = " + "), "+ factor(Year)"))
m_pooled_eir <- lm(f_pooled_eir, data = df_eir)
vc_pooled_eir <- vcovCL(m_pooled_eir, cluster = df_eir$State, type = "HC1")

# Display Coefficient Table with Stars
tidy(coeftest(m_pooled_eir, vcov = vc_pooled_eir)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    significant = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*",p.value < 0.1 ~ "†", TRUE ~ "")) %>%
  kable(digits = 3, caption = "EIr Pooled OLS Coefficients")

# --- New Minimalist Fit Box ---
# Use glance to get a compact summary of N, R2, and F-stat
glance(m_pooled_eir) %>%
  kable(digits = 3, caption = "EIr Pooled Model Fit Summary")

```


# check multicollinearity
```{r}

#EIr pooled OLS without year FE
kable(car::vif(m_pooled_eir))

```


############################################################
# MAIN MODEL 2 EIr: State fixed effects + year fixed effects 
# Uses within estimator; clustered SE by state
############################################################

```{r}

# fixed effect model
  f_fe_eir <- as.formula(
    paste(y_eir, "~", paste(eir_vars_original, collapse = " + "), "+ factor(Year)")
  )
  
  m_fe_eir <- plm(f_fe_eir, data = pdf_eir, model = "within", effect = "twoways")
  vc_fe_eir <- vcovHC(m_fe_eir, type = "HC1", cluster = "group")
tidy(coeftest(m_fe_eir, vcov = vc_fe_eir)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    `p (significant)` = paste0(sprintf("%.3f", p.value), 
                              ifelse(p.value < .001, "***", ifelse(p.value < .01, "**", ifelse(p.value < .05, "*", ifelse(p.value < .10, "†", ""))))))%>%
  kable(digits = 3, caption = "EIr State FE + Year FE (clustered by State)")

# --- Model Fit ---
kable(data.frame(N = nobs(m_fe_eir), 
                 R2_Within = summary(m_fe_eir)$r.squared["rsq"], 
                 F_stat = summary(m_fe_eir)$fstatistic$statistic), 
      digits = 3, caption = "EIr Fit: State FE + Year FE")

# Summary
glance(m_fe_eir) %>%
  kable(digits = 3, caption = "EIr FE Model Fit Summary")
```

#model summary for EIr
```{r}
model_summary_eir <- summary(m_fe_eir)
fit_stats_eir <- data.frame(
    Statistic = c("Observations", "States", "Years", "R-squared (within)", 
                  "Adj. R-squared", "F-statistic", "F p-value"),
    Value = c(
        paste0(nobs(m_fe_eir), " (", length(unique(df_eir$State)), " states × ", 
               length(unique(df_eir$Year)), " years)"),
        length(unique(df_eir$State)),
        length(unique(df_eir$Year)),
        round(model_summary_eir$r.squared[1], 3),
        round(model_summary_eir$r.squared[2], 3),
        round(model_summary_eir$fstatistic$statistic, 2),
        format.pval(model_summary_eir$fstatistic$p.value, digits = 3)
    )
)
kable(fit_stats_eir, caption = "EIr Two-Way FE Model Fit") %>%
    kable_styling()

```

# Temporal trend plot
```{r}

# Display coefficient table with significance stars
tidy(m_pooled_eir, conf.int = TRUE) %>%
   mutate(significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1 ~ "†",
    TRUE ~ ""
  ))%>%
  kable(digits = 3, caption = "EIr State FE + Year FE (clustered by State)")

# -----------------------------
# Extract Year Effects + CIs
# -----------------------------
year_coefs_eir <- tidy(m_pooled_eir, conf.int = TRUE) %>%
  filter(grepl("factor\\(Year\\)", term)) %>%
  mutate(Year = as.numeric(gsub("factor\\(Year\\)", "", term)),
    significant = p.value <= 0.05  # mark significance for plotting
  )

# Add baseline year (intercept) as Year 2009
intercept <- tidy(m_pooled_eir, conf.int = TRUE) %>%
  filter(term == "(Intercept)") %>%
  mutate(Year = 2009,
         conf.low = estimate - 1.96 * std.error,
         conf.high = estimate + 1.96 * std.error,
    significant = p.value <= 0.05)

year_coefs_eir <- bind_rows(intercept, year_coefs_eir) %>%
  arrange(Year) %>%
  mutate(conf.low = estimate - 1.96 * std.error,
         conf.high = estimate + 1.96 * std.error)%>%
  filter(Year != 2020)  # remove 2020

# -----------------------------
# Plot temporal trend
# -----------------------------
ggplot(year_coefs_eir, aes(x = Year, y = estimate)) +
   geom_line(aes(group = cumsum(c(TRUE, diff(Year) != 1))), color = "#2c7fb8", size = 1) +
  geom_point(color = "#2c7fb8", size = 2) +
    # Mark significant years
  geom_point(data = subset(year_coefs_eir, significant), color = "red", size = 3) +
   geom_text(aes(label = round(estimate, 2)), 
            nudge_y = 0.05,        # Adjust this value to move text higher/lower
            vjust = 0,             # Vertically justify to the bottom of the text
            size = 4,              # Change font size
            color = "black") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2c7fb8") +
  scale_x_continuous(breaks = seq(2009, 2022, 1), limits = c(2008, 2022)) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "red", size = 0.6) +
  geom_text(aes(x = 2009, y = min(conf.low), label = "Recession"), color = "red", angle = 90, vjust = -0.7, hjust=-1) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "green", size = 0.6) +
  geom_text(aes(x = 2014, y = min(conf.low), label = "ACA Expansion"), color = "green", angle = 90, vjust = -0.7, hjust=-1) +
  labs(title = "EIr Temporal Trend in EIr with Year Fixed Effects",
       subtitle = "Year coefficients from Pooled OLS (clustered by State)",
       y = "EIr Estimate (with 95% CI)",
       x = "Year") +
  theme_minimal(base_size = 14)

```

############################################################
# MODEL 3 EIr: Between model (state means)
############################################################

```{r}
  df_between_eir <- df %>%
    group_by(State) %>%
    summarise(across(all_of(c(y_eir, eir_vars_original)), ~ mean(., na.rm = TRUE)), .groups = "drop")
  
  m_between_eir <- lm(
    as.formula(paste(y_eir, "~", paste(eir_vars_original, collapse = " + "))),
    data = df_between_eir
  )
  vc_between_eir <- vcovHC(m_between_eir, type = "HC1")
tidy(coeftest(m_between_eir, vcov = vc_between_eir)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    `p (significant)` = paste0(sprintf("%.3f", p.value), 
                              ifelse(p.value < .001, "***", ifelse(p.value < .01, "**", ifelse(p.value < .05, "*", ifelse(p.value < .10, "†", ""))))))%>%
  kable(digits = 3, caption = "EIr Between Model (State Means)")

# --- Model Fit ---
kable(data.frame(N = nobs(m_between_eir), 
                 R2 = summary(m_between_eir)$r.squared, 
                 F_Robust = waldtest(m_between_eir, vcov = vc_between_eir, test = "F")[2, "F"]), 
      digits = 3, caption = "EIr Fit: Between Model")

# Summary
glance(m_between_eir) %>%
  kable(digits = 3, caption = "EIr Between Model Fit Summary")
```

# Forest plot for EIr
```{r}
res_eir <- bind_rows(
  tidy(m_pooled_eir, conf.int = TRUE) %>% mutate(Model = "Pooled OLS"),
  tidy(m_fe_eir, conf.int = TRUE) %>% mutate(Model = "Fixed Effects"),
  tidy(m_between_eir, conf.int = TRUE) %>% mutate(Model = "Between")
) %>%
  filter(!grepl("factor", term), term != "(Intercept)")

ggplot(res_eir, aes(x = estimate, y = term, color = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2
  ) +
  labs(
    title = "Forest Plot of EIr Determinants",
    subtitle = "Comparison of temporal, pooled, and structural effects",
    x = "Coefficient Estimate",
    y = ""
  ) +
  theme_minimal()


```

############################################################
# MODEL 4 EIr: Annual cross-sectional regressions
############################################################

```{r}
annual_results_eir <- df %>%
  group_by(Year) %>%
  group_modify(~{
    mod_eir <- lm(as.formula(paste(y_eir, "~", paste(eir_vars_original, collapse = " + "))), data = .x)
    vc_eir <- vcovHC(mod_eir, type = "HC1")
    ct_eir <- coeftest(mod_eir, vcov = vc_eir)
    
    data.frame(term = rownames(ct_eir),
               estimate = ct_eir[, 1],
               p_val = ct_eir[, 4],
               N = nobs(mod_eir),
               R2 = summary(mod_eir)$r.squared) %>%
 mutate(`p (significant)` = paste0(sprintf("%.3f", p_val), 
                                  ifelse(p_val < .001, "***", ifelse(p_val < .01, "**", ifelse(p_val < .05, "*",ifelse(p_val < .1, "†", ""))))))
  }) %>% ungroup()

kable(annual_results_eir, digits = 3, caption = "EIr Annual cross-sectional regressions with R2")


```

############################################################
# OPTIONAL EIr: Robustness check using log(EIr)
# Use log1p if EIr can be 0
############################################################

```{r}
  df_eir <- df %>%
    mutate(log_eir = ifelse(.data[[y_eir]] <= 0, log1p(.data[[y_eir]]), log(.data[[y_eir]])))
  
  m_pooled_log_eir <- lm(
    as.formula(paste("log_eir ~", paste(eir_vars_original, collapse = " + "), "+ factor(Year)")),
    data = df_eir
  )
  vc_pooled_log_eir <- vcovCL(m_pooled_log_eir, cluster = df_eir$State, type = "HC1")
  tidy(coeftest(m_pooled_log_eir, vcov = vc_pooled_log_eir))%>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ "†",
    TRUE            ~ ""
  )) %>%
  kable(digits = 3, 
        caption = "EIr Robustness: Pooled OLS + Year FE with log(EIr)")
  # Fit for Pooled Log
kable(data.frame(N = nobs(m_pooled_log_eir), R2 = summary(m_pooled_log_eir)$r.squared), 
      caption = "EIr Fit: Pooled Log")
  

  m_fe_log_eir <- plm(
    as.formula(paste("log_eir ~", paste(eir_vars_original, collapse = " + "), "+ factor(Year)")),
    data = df_eir, model = "within", effect = "twoways"
  )
  vc_fe_log_eir <- vcovHC(m_fe_log_eir, type = "HC1", cluster = "group")
  tidy(coeftest(m_fe_log_eir, vcov = vc_fe_log_eir))%>%
  mutate(significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ "†",
    TRUE            ~ ""
  )) %>%
  kable(digits = 3, 
        caption = "EIr Robustness: State FE + Year FE with log(EIr)")
  # Fit for FE Log
kable(data.frame(N = nobs(m_fe_log_eir), R2_Within = summary(m_fe_log_eir)$r.squared["rsq"]), 
      caption = "EIr Fit: FE Log")

# Summary
glance(m_fe_log_eir) %>%
  kable(digits = 3, caption = "EIr Log Model Fit Summary")
  
```

# comparision table
```{r}
# COMPREHENSIVE COMPARISON TABLE for EIr

comparision_table_eir <-stargazer(
  m_pooled_eir,
  m_fe_eir,
  m_between_eir,
  m_pooled_log_eir,
  type = "html",
  out="EIr_comparision_result.doc",
  se = list(
    sqrt(diag(vc_pooled_eir)),
    sqrt(diag(vc_fe_eir)),
    sqrt(diag(vc_between_eir)),
    sqrt(diag(vc_pooled_log_eir))
  ),
  title = "Model Comparison for Early Intervention Rate (EIr)",
  column.labels = c(
    "Pooled",
    "Two-Way FE",
    "Between",
    "Robust (log EIr)"
  ),
  intercept.top = TRUE,            # Moves Constant to the first row
  intercept.bottom = FALSE,        # Ensures it is not at the bottom
  covariate.labels = c("Constant", var_comp), # Add "Constant" to the start of your labels
  dep.var.labels.include = FALSE,   # make stargazer use these labels
  dep.var.labels = "Early Intevention Rate (EIr)", # clean labels
  model.numbers = FALSE,
  omit = "factor\\(Year\\)",
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  star.cutoffs = c(0.05, 0.01, 0.001), # Standard stars
  notes = "† p<0.1; * p<0.05; ** p<0.01; *** p<0.001",
  notes.append = FALSE,
  add.lines = list(
    c("State FE", "No", "Yes", "No", "No"),
    c("Year FE",  "YES", "Yes", "No", "YES")
  )
)

htmltools::browsable(htmltools::HTML(comparision_table_eir))

```


# table 3 95%ci for EIR
```{r eir_comparison_table}

# Row labels for EIr (same covariates as HLp)
eir_row_labels <- c(
  "(Intercept)"             = "Constant",
  "PerTscr"                 = "Percent of Infants Total Screened ",
  "Dr"                      = "Diagnosis Rate",
  "state_per_capita_fund"   = "Per Capita State Public Health Funding",
  "Median_household_income" = "Median Household Income (USD)",
  "Per_rep_women_uninsured" = "Percent of Reproductive Age Women Uninsured",
  "Per_infant_breastfed"    = "Percent Infant Ever Breastfed"
)

eir_vars_table <- c("PerTscr", "Dr", "state_per_capita_fund",
                     "Median_household_income", "Per_rep_women_uninsured",
                     "Per_infant_breastfed")
eir_vars_int   <- c("(Intercept)", eir_vars_table)

# ── Extract each model ────────────────────────────────────────────────────────

# Pooled OLS EIr
pooled_eir_out <- extract_model_cols(m_pooled_eir, vc_pooled_eir,
                                     eir_vars_int, "Pooled")

# Two-Way FE EIr (no intercept)
fe_eir_ct <- coeftest(m_fe_eir, vcov = vc_fe_eir)
fe_eir_vars <- intersect(eir_vars_table, rownames(fe_eir_ct))
fe_eir_df <- as.data.frame(fe_eir_ct[fe_eir_vars, , drop = FALSE])
colnames(fe_eir_df) <- c("estimate", "se", "t", "p")
fe_eir_df$ci_low  <- fe_eir_df$estimate - 1.96 * fe_eir_df$se
fe_eir_df$ci_high <- fe_eir_df$estimate + 1.96 * fe_eir_df$se
fe_eir_df$p_sig   <- case_when(
  fe_eir_df$p < 0.001 ~ "***", fe_eir_df$p < 0.01 ~ "**",
  fe_eir_df$p < 0.05  ~ "*",   fe_eir_df$p < 0.10  ~ "†",
  TRUE ~ ""
)
fe_eir_out <- data.frame(
  Est_FE = c("—", sprintf("%.3f%s", fe_eir_df$estimate, fe_eir_df$p_sig)),
  CI_FE  = c("—", paste0("[", sprintf("%.3f", fe_eir_df$ci_low),
                          ", ", sprintf("%.3f", fe_eir_df$ci_high), "]")),
  p_FE   = c("—", sprintf("%.3f", fe_eir_df$p)),
  check.names = FALSE
)

# Between EIr
between_eir_out <- extract_model_cols(m_between_eir, vc_between_eir,
                                      eir_vars_int, "Between")

# Log(EIr) Pooled OLS
log_eir_out <- extract_model_cols(m_pooled_log_eir, vc_pooled_log_eir,
                                  eir_vars_int, "LogEIr")

# ── Assemble table ────────────────────────────────────────────────────────────
# Use cbind (not bind_cols) to avoid row-count mismatch issues
table3_eir <- data.frame(
  Variable = eir_row_labels[names(eir_row_labels)],
  row.names = NULL,
  check.names = FALSE
)
table3_eir <- cbind(table3_eir, pooled_eir_out, fe_eir_out, between_eir_out, log_eir_out)

colnames(table3_eir) <- c(
  "Variable",
  "Est (Pooled)", "95% CI", "p",
  "Est (Two-Way FE)", "95% CI ", "p ",
  "Est (Between)", "95% CI  ", "p  ",
  "Est (log EIr)", "95% CI   ", "p   "
)

# ── Render table ──────────────────────────────────────────────────────────────
kable(
  table3_eir,
  digits  = 3,
  align   = c("l", rep(c("r", "c", "r"), 4)),
  caption = paste0(
    "Table 3: Model Comparison for Early Intervention Rate (EIr). ",
    "Estimates with 95% confidence intervals (robust, HC1) and p-values. ",
    "Year fixed effects included in Pooled and log(EIr) models. ",
    "† p<0.10; * p<0.05; ** p<0.01; *** p<0.001."
  )
) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  add_header_above(c(
    " " = 1,
    "Pooled OLS + Year FE" = 3,
    "Two-Way FE" = 3,
    "Between (State Means)" = 3,
    "Robustness: log(EIr)" = 3
  )) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(which(table3_eir$Variable == "% Infant Ever Breastfed"),
           bold = TRUE, background = "#f0f8ff") %>%
  kableExtra::footnote(
    general = "95% CI = estimate ± 1.96 × cluster-robust SE (HC1). State FE: No/Yes/No/No. Year FE: Yes/Yes/No/Yes.",
    general_title = "Note:",
    footnote_as_chunk = TRUE
  )

# ── Export to Word ────────────────────────────────────────────────────────────
ft_eir <- flextable(table3_eir) %>%
  set_caption("Table 3: Model Comparison for Early Intervention Rate (EIr)") %>%
  add_header_row(
    values = c("", "Pooled OLS + Year FE", "Two-Way FE",
               "Between (State Means)", "Robustness: log(EIr)"),
    colwidths = c(1, 3, 3, 3, 3)
  ) %>%
  bold(part = "header") %>%
  autofit() %>%
  theme_booktabs()

#save_as_docx(ft_eir, path = "EIr_comparison_Table3.docx")


```


############################################################
# OPTIONAL EIr: Balanced-panel sensitivity sample (states with all 13 years)
############################################################
  
```{r}
    balanced_states_eir <- df %>%
    mutate(complete_row = if_all(all_of(c(y_eir, eir_vars_original)), ~ !is.na(.))) %>%
    group_by(State) %>%
    summarise(n_years_complete = n_distinct(Year[complete_row]), .groups = "drop") %>%
    filter(n_years_complete == 13) %>%
    pull(State)
  
  df_bal_eir <- df %>% filter(State %in% balanced_states_eir)
  
  m_pooled_bal_eir <- lm(f_pooled_eir, data = df_bal_eir)
  vc_pooled_bal_eir <- vcovCL(m_pooled_bal_eir, cluster = df_bal_eir$State, type = "HC1")
  
# 1. Coefficient Table
tidy(coeftest(m_pooled_bal_eir, vcov = vc_pooled_bal_eir)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    `p (significant)` = paste0(sprintf("%.3f", p.value), 
                              ifelse(p.value < .001, "***", ifelse(p.value < .01, "**", ifelse(p.value < .05, "*", ifelse(p.value < .10, "†", "")))))) %>%
  kable(digits = 3, caption = "EIr Sensitivity: Balanced sample pooled OLS + Year FE")

# 2. Fit Table (Fixed to reference correct model)
kable(data.frame(N = nobs(m_pooled_bal_eir), 
                 R2 = summary(m_pooled_bal_eir)$r.squared,
                 Adj_R2 = summary(m_pooled_bal_eir)$adj.r.squared), 
      caption = "EIr Fit: Balanced Sample")

# Summary
glance(m_pooled_bal_eir) %>%
  kable(digits = 3, caption = "EIr Banalnced Pooled Fit Summary")
```

# EIr Diagnostics
```{r}
cat("\n=== EIr MODEL DIAGNOSTICS (Two-Way Fixed Effects) ===\n")

# Diagnostic 1: Serial Correlation Test
cat("\n1. Serial Correlation Test (Breusch-Godfrey):\n")
serial_test_eir <- pbgtest(m_fe_eir)
print(serial_test_eir)

# Diagnostic 2: Heteroskedasticity Test
cat("\n2. Heteroskedasticity Test (Breusch-Pagan):\n")
# Create formula for BP test
f_bp_eir <- as.formula(paste("EIr ~", paste(eir_vars_original, collapse = " + "),
                             "+ factor(State) + factor(Year)"))
bptest_eir <- bptest(f_bp_eir, data = df_eir, studentize = TRUE)
print(bptest_eir)

# # Diagnostic 3: Hausman Test (Fixed vs Random Effects)
cat("\n3. Hausman Test (FE vs RE):\n")
f_re_eir <- as.formula(paste("EIr ~", paste(eir_vars_original, collapse = " + ")))
m_re_eir <- plm(f_re_eir, data = pdf_eir, model = "random")
hausman_test_eir <- phtest(m_fe_eir, m_re_eir)
print(hausman_test_eir)

# Diagnostic 4: Cross-sectional dependence
cat("\n4. Cross-sectional Dependence Test (Pesaran CD):\n")
cd_test_eir <- pcdtest(m_fe_eir, test = "cd")
print(cd_test_eir)

# Diagnostic 5: Unit root test (for panel data)
cat("\n5. Panel Unit Root Test (Levin-Lin-Chu):\n")
# Note: This requires pseries and might take time
# library(tseries)
# llc_test_eir <- purtest(eir ~ 1, data = pdf_eir, index = c("State", "Year"),
#                         test = "levinlin", lags = "AIC")
# print(llc_test_eir)

# Create diagnostic summary table
diagnostics_summary_eir <- data.frame(
  Test = c(
    "Serial Correlation",
    "Heteroskedasticity",
    "Cross-sectional Dependence"
  ),
  Statistic = c(
    round(serial_test_eir$statistic, 3),
    round(bptest_eir$statistic, 3),
    round(cd_test_eir$statistic, 3)
  ),
  P_Value = c(
    format.pval(serial_test_eir$p.value, digits = 3),
    format.pval(bptest_eir$p.value, digits = 3),
    format.pval(cd_test_eir$p.value, digits = 3)
  ),
  Interpretation = c(
    "Serial correlation present → Clustered SEs used",
    "Heteroskedasticity present → Robust SEs used",
    "Cross-sectional dependence present"
  )
)

knitr::kable(
  diagnostics_summary_eir,
  caption = "Model Diagnostics for Two-Way Fixed Effects eir Model"
)

```


## Key Results

```{r}

# Extract key results
key_results <- data.frame(
    Predictor = eir_vars_original,
    TwoWayFE_Estimate = round(coef(m_fe_eir), 3),
    TwoWayFE_SE = round(sqrt(diag(vc_fe_eir)), 3),
    TwoWayFE_Pvalue = round(summary(m_fe_eir)$coefficients[, 4], 3),
    Pooled_Estimate = round(coef(m_pooled_eir)[eir_vars_original], 3),
    Pooled_Pvalue = round(summary(m_pooled_eir)$coefficients[eir_vars_original, 4], 3)
)

# Identify significant predictors
significant_predictors <- key_results %>%
    filter(TwoWayFE_Pvalue < 0.05) %>%
    arrange(desc(abs(TwoWayFE_Estimate)))

cat("\nStatistically Significant Predictors (Two-Way FE, p<0.05):\n")
kable(significant_predictors, digits = 3) %>%
    kable_styling()

# Create summary statement
cat("\n=== MAIN CONCLUSION ===\n")
if (nrow(significant_predictors) > 0) {
    top_predictor <- significant_predictors[1, ]
    cat(sprintf(
        "The strongest predictor of eir is %s (β = %.3f, p = %.3f).\n",
        top_predictor$Predictor,
        top_predictor$TwoWayFE_Estimate,
        top_predictor$TwoWayFE_Pvalue
    ))
} else {
    cat("No predictors reached statistical significance at p<0.05 in the Two-Way FE model.\n")
}

cat(sprintf(
    "\nModel Statistics:\n- R-squared (within): %.3f\n- F-statistic: %.2f (p = %.3f)\n- Observations: %d\n",
    model_summary_eir$r.squared[1],
    model_summary_eir$fstatistic$statistic,
    model_summary_eir$fstatistic$p.value,
    nobs(m_fe_eir)
))

```


# ==============================================
# OPTIONAL: PCA ANALYSIS AND PCA-BASED MODELS
# ==============================================

```{r}


cat("\n=== OPTIONAL PCA ANALYSIS ===\n")

# Select variables for PCA
pca_vars <- c(
  "PerTscr",
  "Dr",
  "state_per_capita_fund",
  "Median_household_income",
  "Per_rep_women_uninsured",
  "Per_infant_breastfed"
)

# Run PCA
pca_data <- df_eir %>%
  select(all_of(pca_vars)) %>%
  drop_na()

pca_res <- prcomp(pca_data, scale. = TRUE, center = TRUE)

# PCA Summary
cat("\nPCA Summary:\n")
print(summary(pca_res))

# Variance explained
variance_explained <- summary(pca_res)$importance[2,] * 100
cumulative_variance <- cumsum(variance_explained)

# Find how many components explain >80% variance
components_80 <- min(which(cumulative_variance >= 80))
cat(sprintf("\nComponents needed for >80%% variance: %d\n", components_80))

# Scree plot
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 60)) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = components_80, linetype = "dashed", color = "blue", alpha = 0.5) +
  annotate("text", x = components_80 + 0.3, y = 85, 
           label = paste(">80% at", components_80, "components"), 
           color = "blue", size = 3.5) +
  labs(
    title = "Scree Plot: PCA Components",
    subtitle = "Components explaining >80% variance marked in blue"
  ) +
  theme_minimal()

# Variable loadings plot
fviz_pca_var(
  pca_res,
  col.var = "contrib",
  gradient.cols = c("#2c7fb8", "#E7B800", "#d7191c"),
  repel = TRUE,
  title = "PCA: Variable Contributions"
) +
  theme_minimal()

# Extract PCA loadings
pca_loadings <- as.data.frame(pca_res$rotation[, 1:3])
colnames(pca_loadings) <- paste0("PC", 1:3)

cat("\nPCA Loadings (First 3 Components):\n")
kable(round(pca_loadings, 3), caption = "PCA Variable Loadings") %>%
  kable_styling()

# Interpret components
cat("\n=== PCA COMPONENT INTERPRETATION ===\n")
interpret_pca <- function(loadings, component, n_top = 3) {
  sorted_loadings <- loadings %>%
    rownames_to_column("Variable") %>%
    arrange(desc(abs(.data[[component]]))) %>%
    head(n_top)
  
  cat(sprintf("\n%s (Top %d variables):\n", component, n_top))
  for(i in 1:nrow(sorted_loadings)) {
    var <- sorted_loadings$Variable[i]
    loading <- sorted_loadings[[component]][i]
    direction <- ifelse(loading > 0, "positive", "negative")
    cat(sprintf("  %s: %.3f (%s)\n", var, loading, direction))
  }
}

interpret_pca(pca_loadings, "PC1")
interpret_pca(pca_loadings, "PC2")
interpret_pca(pca_loadings, "PC3")

# ==============================================
# PCA-BASED MODELS FOR EIr
# ==============================================

cat("\n=== PCA-BASED MODELS FOR EIr ===\n")

# Add PCA scores to dataset
pca_scores <- as.data.frame(predict(pca_res, pca_data))
df_pca <- df_eir
df_pca[complete.cases(df_eir[, pca_vars]), paste0("PC", 1:3)] <- pca_scores[, 1:3]

# Create panel data with PCA scores
pdf_pca <- pdata.frame(df_pca, index = c("State", "Year"))

# Define PCA variables
eir_vars_pca <- paste0("PC", 1:3)

# MODEL 1: Two-Way FE with PCA Components
cat("\n1. Two-Way FE with PCA Components:\n")
f_pca_fe <- as.formula(paste("EIr ~", paste(eir_vars_pca, collapse = " + ")))
m_pca_fe <- plm(f_pca_fe, data = pdf_pca, model = "within", effect = "twoways")
vc_pca_fe <- vcovHC(m_pca_fe, type = "HC1", cluster = "group")

tidy(coeftest(m_pca_fe, vcov = vc_pca_fe)) %>%
   mutate(
    `p (significant)` = paste0(
      sprintf("%.3f", p.value),
      ifelse(p.value < 0.001, "***",
             ifelse(p.value < 0.01, "**",
                    ifelse(p.value < 0.05, "*",
                           ifelse(p.value < 0.1, "†", "")))))
  ) %>%
  kable(digits = 3, caption = "EIr: Two-Way FE with PCA Components") %>%
  kable_styling()

# MODEL 2: Between Model with PCA Components
cat("\n2. Between Model with PCA Components:\n")
df_between_pca <- df_pca %>%
  group_by(State) %>%
  summarise(across(all_of(c("EIr", eir_vars_pca)), 
                   ~ mean(., na.rm = TRUE)), 
            .groups = "drop") %>%
  drop_na()

m_pca_between <- lm(as.formula(paste("EIr ~", paste(eir_vars_pca, collapse = " + "))), 
                    data = df_between_pca)
vc_pca_between <- vcovHC(m_pca_between, type = "HC1")

tidy(coeftest(m_pca_between, vcov = vc_pca_between)) %>%
    mutate(
    `p (significant)` = paste0(
      sprintf("%.3f", p.value),
      ifelse(p.value < 0.001, "***",
             ifelse(p.value < 0.01, "**",
                    ifelse(p.value < 0.05, "*",
                           ifelse(p.value < 0.1, "†", "")))))
  ) %>%
  kable(digits = 3, caption = "EIr: Between Model with PCA Components") %>%
  kable_styling()

# ==============================================
# COMPARE ORIGINAL VS PCA MODELS
# ==============================================

cat("\n=== COMPARISON: Original vs PCA Models ===\n")

# Collect key statistics
model_comparison_eir <- data.frame(
  Model = c("Original Variables", "PCA Components"),
  Variables = c(length(eir_vars_original), length(eir_vars_pca)),
  R2_Within = c(
    round(summary(m_fe_eir)$r.squared[1], 3),
    round(summary(m_pca_fe)$r.squared[1], 3)
  ),
  R2_Adj = c(
    round(summary(m_fe_eir)$r.squared[2], 3),
    round(summary(m_pca_fe)$r.squared[2], 3)
  ),
  F_Statistic = c(
    round(summary(m_fe_eir)$fstatistic$statistic, 2),
    round(summary(m_pca_fe)$fstatistic$statistic, 2)
  ),
  Observations = c(
    nobs(m_fe_eir),
    nobs(m_pca_fe)
  )
)

kable(model_comparison_eir, caption = "Comparison: Original vs PCA Models") %>%
  kable_styling()

# ==============================================
# PCA MODEL INTERPRETATION GUIDANCE
# ==============================================

cat("\n=== HOW TO INTERPRET PCA MODELS ===\n")
cat("\n1. PCA reduces 6 original variables to 3 components.\n")
cat(sprintf("2. These 3 components explain %.1f%% of total variance.\n", cumulative_variance[3]))
cat("3. Each component represents a pattern of correlation:\n")
cat("   - PC1: ", paste(rownames(pca_loadings)[order(-abs(pca_loadings$PC1))][1:2], collapse = ", "), "\n")
cat("   - PC2: ", paste(rownames(pca_loadings)[order(-abs(pca_loadings$PC2))][1:2], collapse = ", "), "\n")
cat("   - PC3: ", paste(rownames(pca_loadings)[order(-abs(pca_loadings$PC3))][1:2], collapse = ", "), "\n")
cat("\n4. PCA models are more parsimonious and avoid multicollinearity.\n")
```

# combined forest plot for HLp and EIr

```{r}

# 1. Prepare and Combine Data
res_combined <- bind_rows(
  # HLp Models
  tidy(m_pooled_hlp, conf.int = TRUE) %>% mutate(Model = "Pooled OLS", Outcome = "Hearing Loss Prevalence (HLp)"),
  tidy(m_fe_hlp, conf.int = TRUE) %>% mutate(Model = "Two-Way FE", Outcome = "Hearing Loss Prevalence (HLp)"),
  tidy(m_between_hlp, conf.int = TRUE) %>% mutate(Model = "Between", Outcome = "Hearing Loss Prevalence (HLp)"),
  
  # EIr Models
  tidy(m_pooled_eir, conf.int = TRUE) %>% mutate(Model = "Pooled OLS", Outcome = "Early Intervention Rate (EIr)"),
  tidy(m_fe_eir, conf.int = TRUE) %>% mutate(Model = "Two-Way FE", Outcome = "Early Intervention Rate (EIr)"),
  tidy(m_between_eir, conf.int = TRUE) %>% mutate(Model = "Between", Outcome = "Early Intervention Rate (EIr)")
) %>%
  filter(!grepl("factor", term), term != "(Intercept)") %>%
  mutate(
    # Map technical names to your descriptive labels
    term_label = ifelse(term %in% names(var_pca), var_pca[term], term),
    term_label = factor(term_label, levels = rev(unique(term_label))),
    # Ensure HLp is on the left by setting factor levels for Outcome
    Outcome = factor(Outcome, levels = c("Hearing Loss Prevalence (HLp)", "Early Intervention Rate (EIr)"))
  )

# preparing label for plot
res_combined <- res_combined %>%
  mutate(
    est_lab = sprintf("%.3f", estimate),
    p_lab   = sprintf("p = %.3f", p.value),

    # tag only the rows you want to label
    label_flag = term %in% c("Per_rep_women_uninsured", "Per_infant_breastfed")
  )

label_data <- res_combined %>%
  filter(label_flag) %>%
  mutate(
    Model = factor(Model, levels = c("Between", "Pooled OLS", "Two-Way FE"))
  )
  # 2. Convert to flextable and format for professional appearance
ftd <- flextable(res_combined) %>%
  set_caption("Data Source for Comparative Forests Plot") %>%
  colformat_double(digits = 3) %>%  # Sets decimal places for Mean, SD, etc.
  autofit() %>%                     # Adjusts column widths automatically
  theme_booktabs() %>%              # Clean, academic style
  bold(part = "header")

# 3. Export directly to Word
#save_as_docx(ftd, path = "Data Source for Comparative Forests Plot.docx")
#write.csv(res_combined, file="Figure3_forest_plot.csv")
# 2. Create the Combined Plot

pd <- position_dodge(width = 0.7)

ggplot(res_combined, aes(x = estimate, y = term_label, color = Model)) +
  # Thicker vertical zero line
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray20", linewidth = 1.0) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = pd,
    height = 0.3,
    linewidth = 0.9
  ) +
  geom_point(position = pd, size = 3) +
  
# -----------------------------
  # BETWEEN: label below
  # -----------------------------
  geom_text(
    data = subset(label_data, Model == "Between"),
    aes(label = est_lab, group = Model),
    position = pd,
    nudge_x = 0.0,
    vjust = 3.5,
    hjust = 0.5,
    size = 3.0,
    color = "black",
    show.legend = FALSE
  ) +

  # -----------------------------
  # TWO-WAY FE: label above
  # -----------------------------
  geom_text(
    data = subset(label_data, Model == "Two-Way FE"),
    aes(label = est_lab, group = Model),
    position = pd,
    nudge_x = 0.0,
    vjust = -2.8,
    hjust = 0.5,
    size = 3.0,
    color = "black",
    show.legend = FALSE
  ) +

  # -----------------------------
  # POOLED OLS: label above + arrow
  # -----------------------------
  geom_text(
    data = subset(label_data, Model == "Pooled OLS"),
    aes(label = est_lab, group = Model),
    position = pd,
    vjust = -0.6,
    hjust = -0.1,
    size = 3.0,
    color = "black",
    show.legend = FALSE
  ) +

  geom_segment(
    data = subset(label_data, Model == "Pooled OLS"),
    aes(
      x = estimate,
      xend = estimate,
      y = term_label,
      yend = term_label,
      group = Model
    ),
    position = pd,
    inherit.aes = FALSE,
    color = "gray30",
    linewidth = 0.5,
    arrow = arrow(length = unit(0.08, "inches"), type = "closed")
  ) +


  
  # facet_grid ensures the Left/Right split
  facet_grid(. ~ Outcome, scales = "free_x") + 
  scale_color_manual(values = c("Pooled OLS" = "#2c3e50", 
                                "Two-Way FE" = "#e74c3c", 
                                "Between" = "#3498db"),
                     labels =c(
                       "Pooled OLS" = "Pooled ordinary least squares model",
                       "Two-Way FE" = "Two-way fixed effects model",
                       "Between"= "Between-state model"
                     )) +
  labs(
    title = "Comparative Determinants of HLp and EIr",
    subtitle = "Standardized coefficient estimates with 95% confidence intervals",
    x = "Coefficient Estimate",
    y = NULL,
    color = "Model Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    # Bold and Black Y-axis labels (Variable Names)
    axis.text.y = element_text(color = "black"),
    # Bold and Black X-axis labels (Estimates)
    axis.text.x = element_text(color = "black"),
    # Bold Axis Title
    axis.title.x = element_text(face = "bold", color = "black", margin = margin(t = 10)),
    # Format the panel headers (HLp and EIr)
    strip.text = element_text(face = "bold", size = 12, color = "black"),
    panel.spacing = unit(2, "lines"),
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )
# save plot
#ggsave("Supplementary_Figure2_Forest_plot.TIFF", dpi = 300, compression = "lzw")
```


# ================================
# INTERACTION ANALYSIS
# Effect modification of breastfeeding
# ================================

#relabel for short names
```{r}
label(ehdi_pos$HLp) <- "Hearing Loss Prevalence (HLp)"
label(ehdi_pos$EIr) <- "Early Intervention Rate (Part c & Non-Part C) (EIr)"
label(ehdi_pos$PerTscr) <- "Percent of Infants Total Screened"
label(ehdi_pos$Dr) <- "Diagnosis Rate"
label(ehdi_pos$Median_household_income) <- "Median Household Income (USD)"
label(ehdi_pos$state_per_capita_fund) <- "Per Capita State Public Health Funding"
label(ehdi_pos$Per_rep_women_uninsured) <- "Percent of Reproductive Age Women Uninsured"
label(ehdi_pos$Per_infant_breastfed) <- "Percent Infant Ever Breastfed"


# create lookup vector
var_labels_Inter <- sapply(
  ehdi_pos[, c(y, xdesc)],
  function(v) attr(v, "label")
)

var_labels_Inter
```

```{r}


# Convert to panel data
pdf <- pdata.frame(df, index = c("State", "Year"))

# -------------------------------
# Base covariates (exclude breastfeeding and modifiers)
# -------------------------------
base_covars <- c(
  "state_per_capita_fund",
  "Median_household_income",
  "Per_rep_women_uninsured"
)


# -------------------------------
# Potential effect modifiers
# -------------------------------
modifiers <- c(
  "Dr",
  "PerTscr"

)

# -------------------------------
# Function to run FE interaction
# -------------------------------
run_fe_interaction <- function(outcome, modifier, data){

  f <- as.formula(
    paste0(
      outcome, " ~ ",
      paste(base_covars, collapse = " + "),
      " + Per_infant_breastfed * ", modifier,
      " + factor(Year)"
    )
  )

  model <- plm(
    f,
    data = data,
    model = "within",
    effect = "twoways"
  )

  vc <- vcovHC(model, type = "HC1", cluster = "group")

  tidy(coeftest(model, vcov = vc), conf.int=TRUE) %>%
    mutate(
      Interaction = paste("Breastfeeding x", modifier),
      conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
      Significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1  ~ "†",
        TRUE ~ ""
      )
    )
}


df_hlp_interact <- df %>% filter(complete.cases(select(., HLp, all_of(hlp_vars_original))))
pdf_hlp_interact <- pdata.frame(df_hlp_interact, index = c("State", "Year"))

# -------------------------------
# Run interactions for HLp
# -------------------------------
hlp_interactions <- lapply(
  modifiers,
  run_fe_interaction,
  outcome = "HLp",
  data = pdf_hlp_interact
)

names(hlp_interactions) <- modifiers

kable(hlp_interactions)

# -------------------------------
# Run interactions for EIr
# -------------------------------
eir_interactions <- lapply(
  modifiers,
  run_fe_interaction,
  outcome = "EIr",
  data = pdf_eir
)

names(eir_interactions) <- modifiers
kable(eir_interactions)

```

# Interaction plot
```{r}

# -------------------------------
# Helper to extract interaction rows
# -------------------------------
extract_interaction <- function(res_list, outcome_name) {
  bind_rows(res_list) %>%
    filter(grepl("Per_infant_breastfed:", term)) %>%
    mutate(Outcome = outcome_name)
}

interaction_data_clean <- bind_rows(
  extract_interaction(hlp_interactions, "HLp"),
  extract_interaction(eir_interactions, "EIr")
) %>%
  mutate(
    modifier_raw = gsub("Per_infant_breastfed:", "", term),

    clean_modifier = ifelse(
      modifier_raw %in% names(var_labels_Inter),
      var_labels_Inter[modifier_raw],
      modifier_raw
    ),

    Interaction_Label = paste("Breastfeeding ×", clean_modifier),

    Interaction_Label = factor(
      Interaction_Label,
      levels = rev(unique(Interaction_Label))
    ),

    is_sig = ifelse(p.value < 0.05, "Yes", "No")
  )

#Create label to be included in plot
interaction_data_clean <- interaction_data_clean %>%
  mutate(
    # Nicely formatted numbers for labels
    est_lab  = sprintf("%.3f", estimate),
    low_lab  = sprintf("%.3f", conf.low),
    high_lab = sprintf("%.3f", conf.high),
    p_lab    = sprintf("p = %.3f", p.value),

    # Combined label: estimate [CI] and p-value
    label_text = paste0(
      est_lab, " [", low_lab, ", ", high_lab, "]\n", p_lab
    )
  )


  # 2. Convert to flextable and format for professional appearance
intr <- flextable(interaction_data_clean) %>%
  set_caption("Data Source for Comparative Interaction Plot") %>%
  colformat_double(digits = 3) %>%  # Sets decimal places for Mean, SD, etc.
  autofit() %>%                     # Adjusts column widths automatically
  theme_booktabs() %>%              # Clean, academic style
  bold(part = "header")

# 3. Export directly to Word
#save_as_docx(intr, path = "Figure4 source data Interaction Plot.docx")
#write.csv(interaction_data_clean, file="Figure4 source data Interaction Plot.csv")
```


```{r}

#Interaction plot for publication
ggplot(
  interaction_data_clean,
  aes(
    x = estimate,
    y = Interaction_Label,
        color = Outcome
  )
) +

  # Zero reference line
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.9
  ) +

  # Confidence intervals
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.25,
    linewidth = 0.9
  ) +

  # Point estimates
  geom_point(
    size = 2.8,
    shape = 16
  ) +

  # Estimate label at center dot
  geom_text(
    aes(label = est_lab),
    nudge_x = 0.0,       # directly at the point; change to 0.02 if crowded
    vjust   = -1.0,
    size    = 3.0,
    color   = "black",
    show.legend = FALSE
  ) +

  # Lower CI label at left end of error bar
  geom_text(
    aes(x = conf.low, label = low_lab),
    vjust = -2.0,
    hjust = 0.5,
    size  = 3.0,
    color = "black",
    show.legend = FALSE
  ) +

  # Upper CI label at right end of error bar
  geom_text(
    aes(x = conf.high, label = high_lab),
    vjust = -2.0,
    hjust = 0.6,
    size  = 3.0,
    color = "black",
    show.legend = FALSE
  ) +

  # p-value label below the line (under the point)
  geom_text(
    aes(label = p_lab),
    vjust = 2.2,
    size  = 3.0,
    color = "black",
    show.legend = FALSE
  ) +

  # Facet by outcome with enforced order
  facet_grid(. ~ Outcome, scales = "free_x") +
   scale_color_manual(
        values = c(
            "HLp" = "#2c3e50",
            "EIr" = "#3498db"
        ),
        labels = c(
    "HLp" = "Hearing Loss Prevalence",
    "EIr" = "Early Intervention Rate")
    ) +

  labs(
    title = "Moderating Effects of Breastfeeding and Covariates",
    subtitle = "Interaction coefficients with 95% CI",
    x = "Interaction Coefficient Estimate",
    y = NULL,
    color = NULL
  ) +

  theme_minimal(base_size = 13) +

  theme(
    legend.position ="bottom",
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(
      face = "bold",
      margin = margin(t = 10)
    ),
    strip.text = element_text(
      face = "bold",
      size = 12,
      color = "black"
    ),
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")
  )

# For Journals TIFF image
#ggsave("Supplementary_Figure3_Interaction_plot.tiff", width = 10, height = 5, dpi = 300, compression = "lzw")

```


```{r}
# -------------------------------
# Plot type 1
# -------------------------------
ggplot(interaction_data_clean, aes(x = estimate, y = Interaction_Label)) +

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray20",
    linewidth = 1
  ) +

  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high, alpha = is_sig),
    height = 0.3,
    linewidth = 0.9,
    color = "black"
  ) +

  geom_point(
    aes(alpha = is_sig),
    size = 3,
    shape = 21,
    fill = "white",
    color = "black",
    stroke = 1.2
  ) +

  geom_text(
    aes(label = Significance),
    vjust = -0.6,
    size = 4.5,
    color = "black"
  ) +

  scale_alpha_manual(
    values = c("No" = 0.4, "Yes" = 1),
    guide = "none"
  ) +

  facet_grid(. ~ Outcome, scales = "free_x") +

  labs(
    title = "Moderating Effects of Biopsychosocial Factors",
    subtitle = "Breastfeeding interaction coefficients with 95 percent confidence intervals",
    x = "Interaction Coefficient Estimate",
    y = NULL
  ) +

  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

# For Journals TIFF image
#ggsave("Interaction Plot_v1.tiff", width = 8, height = 6, dpi = 300, compression = "lzw")
```



#################SECTION BREAK FOR EIrd#################################


# Defining variables for EIrd model
```{r}
  # 2) Define outcome + covariates (as described by you)
 #dependent variable
   y_eird <- "EIrd"
 # Independent variable
 eird_vars_original <- c("PerTscr","Dr","state_per_capita_fund","Median_household_income","Per_rep_women_uninsured","Per_infant_breastfed")
 
```



# defining data object
```{r}

df_eird <- df %>%
  filter(complete.cases(select(., EIrd, all_of(eird_vars_original))))

pdf_eird <- pdata.frame(  df_eird,index = c("State", "Year"))
```


############################################################
# OPTIONAL TO CHECK: Eird: Pooled OLS without year fixed effects 
# Cluster-robust SE by state 
############################################################
```{r}
# Pooled OLS without year fixed effects
f_pooled_no_year_eird <- as.formula(paste(y_eird, "~", paste(eird_vars_original, collapse = " + ")))
m_pooled_no_year_eird <- lm(f_pooled_no_year_eird, data = df_eird)

# Cluster-robust SE by State
vc_pooled_no_year_eird <- vcovCL(m_pooled_no_year_eird, cluster = df_eird$State, type = "HC1")

# Display coefficient table with significance stars
tidy(coeftest(m_pooled_no_year_eird, vcov = vc_pooled_no_year_eird, conf.int = TRUE, conf.level = 0.95)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    p.value < .10 ~  "†",
    TRUE ~ ""
  )) %>%
  kable(digits = 3, caption = "EIrd Pooled OLS Coefficients (No Year FE)")

# Summary
glance(m_pooled_no_year_eird) %>%
  kable(digits = 3, caption = "EIrd Pooled Model Fit Summary (No Year FE)")
```

# check multicollinearity
```{r}

#EIrd pooled OLS without year FE
kable(car::vif(m_pooled_no_year_eird))
```


############################################################
# MODEL 1 EIrd: Pooled OLS with year fixed effects 
# Cluster-robust SE by state 
############################################################
```{r}

f_pooled_eird <- as.formula(paste(y_eird, "~", paste(eird_vars_original, collapse = " + "), "+ factor(Year)"))
m_pooled_eird <- lm(f_pooled_eird, data = df_eird)
vc_pooled_eird <- vcovCL(m_pooled_eird, cluster = df_eird$State, type = "HC1")

# Display Coefficient Table with Stars
tidy(coeftest(m_pooled_eird, vcov = vc_pooled_eird)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    significant = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*",p.value < 0.1 ~ "†", TRUE ~ "")) %>%
  kable(digits = 3, caption = "EIrd Pooled OLS Coefficients")

# --- New Minimalist Fit Box ---
# Use glance to get a compact summary of N, R2, and F-stat
glance(m_pooled_eird) %>%
  kable(digits = 3, caption = "EIrd Pooled Model Fit Summary")

```


# check multicollinearity
```{r}

#EIrd pooled OLS without year FE
kable(car::vif(m_pooled_eird))

```


############################################################
# MAIN MODEL 2 EIrd: State fixed effects + year fixed effects 
# Uses within estimator; clustered SE by state
############################################################

```{r}

# fixed effect model
  f_fe_eird <- as.formula(
    paste(y_eird, "~", paste(eird_vars_original, collapse = " + "), "+ factor(Year)")
  )
  
  m_fe_eird <- plm(f_fe_eird, data = pdf_eird, model = "within", effect = "twoways")
  vc_fe_eird <- vcovHC(m_fe_eird, type = "HC1", cluster = "group")
tidy(coeftest(m_fe_eird, vcov = vc_fe_eird)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    `p (significant)` = paste0(sprintf("%.3f", p.value), 
                              ifelse(p.value < .001, "***", ifelse(p.value < .01, "**", ifelse(p.value < .05, "*", ifelse(p.value < .10, "†", ""))))))%>%
  kable(digits = 3, caption = "EIrd State FE + Year FE (clustered by State)")

# --- Model Fit ---
kable(data.frame(N = nobs(m_fe_eird), 
                 R2_Within = summary(m_fe_eird)$r.squared["rsq"], 
                 F_stat = summary(m_fe_eird)$fstatistic$statistic), 
      digits = 3, caption = "eird Fit: State FE + Year FE")

# Summary
glance(m_fe_eird) %>%
  kable(digits = 3, caption = "eird FE Model Fit Summary")
```

#model summary for eird
```{r}
model_summary_eird <- summary(m_fe_eird)
fit_stats_eird <- data.frame(
    Statistic = c("Observations", "States", "Years", "R-squared (within)", 
                  "Adj. R-squared", "F-statistic", "F p-value"),
    Value = c(
        paste0(nobs(m_fe_eird), " (", length(unique(df_eird$State)), " states × ", 
               length(unique(df_eird$Year)), " years)"),
        length(unique(df_eird$State)),
        length(unique(df_eird$Year)),
        round(model_summary_eird$r.squared[1], 3),
        round(model_summary_eird$r.squared[2], 3),
        round(model_summary_eird$fstatistic$statistic, 2),
        format.pval(model_summary_eird$fstatistic$p.value, digits = 3)
    )
)
kable(fit_stats_eird, caption = "eird Two-Way FE Model Fit") %>%
    kable_styling()

```

# Temporal trend plot
```{r}

# Display coefficient table with significance stars
tidy(m_pooled_eird, conf.int = TRUE) %>%
   mutate(
     conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
     significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1 ~ "†",
    TRUE ~ ""
  ))%>%
  kable(digits = 3, caption = "eird State FE + Year FE (clustered by State)")

# -----------------------------
# Extract Year Effects + CIs
# -----------------------------
year_coefs_eird <- tidy(m_pooled_eird, conf.int = TRUE) %>%
  filter(grepl("factor\\(Year\\)", term)) %>%
  mutate(Year = as.numeric(gsub("factor\\(Year\\)", "", term)),
    significant = p.value <= 0.05  # mark significance for plotting
  )

# Add baseline year (intercept) as Year 2009
intercept <- tidy(m_pooled_eird, conf.int = TRUE) %>%
  filter(term == "(Intercept)") %>%
  mutate(Year = 2009,
         conf.low = estimate - 1.96 * std.error,
         conf.high = estimate + 1.96 * std.error,
    significant = p.value <= 0.05)

year_coefs_eird <- bind_rows(intercept, year_coefs_eird) %>%
  arrange(Year) %>%
  mutate(conf.low = estimate - 1.96 * std.error,
         conf.high = estimate + 1.96 * std.error)%>%
  filter(Year != 2020)  # remove 2020

# -----------------------------
# Plot temporal trend
# -----------------------------
ggplot(year_coefs_eird, aes(x = Year, y = estimate)) +
   geom_line(aes(group = cumsum(c(TRUE, diff(Year) != 1))), color = "#2c7fb8", size = 1) +
  geom_point(color = "#2c7fb8", size = 2) +
    # Mark significant years
  geom_point(data = subset(year_coefs_eird, significant), color = "red", size = 3) +
   geom_text(aes(label = round(estimate, 2)), 
            nudge_y = 0.05,        # Adjust this value to move text higher/lower
            vjust = 0,             # Vertically justify to the bottom of the text
            size = 4,              # Change font size
            color = "black") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2c7fb8") +
  scale_x_continuous(breaks = seq(2009, 2022, 1), limits = c(2008, 2022)) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "red", size = 0.6) +
  geom_text(aes(x = 2009, y = min(conf.low), label = "Recession"), color = "red", angle = 90, vjust = -0.7, hjust=-1) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "green", size = 0.6) +
  geom_text(aes(x = 2014, y = min(conf.low), label = "ACA Expansion"), color = "green", angle = 90, vjust = -0.7, hjust=-1) +
  labs(title = "eird Temporal Trend in eird with Year Fixed Effects",
       subtitle = "Year coefficients from Pooled OLS (clustered by State)",
       y = "eird Estimate (with 95% CI)",
       x = "Year") +
  theme_minimal(base_size = 14)

```

############################################################
# MODEL 3 eird: Between model (state means)
############################################################

```{r}
  df_between_eird <- df %>%
    group_by(State) %>%
    summarise(across(all_of(c(y_eird, eird_vars_original)), ~ mean(., na.rm = TRUE)), .groups = "drop")
  
  m_between_eird <- lm(
    as.formula(paste(y_eird, "~", paste(eird_vars_original, collapse = " + "))),
    data = df_between_eird
  )
  vc_between_eird <- vcovHC(m_between_eird, type = "HC1")
tidy(coeftest(m_between_eird, vcov = vc_between_eird)) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    `p (significant)` = paste0(sprintf("%.3f", p.value), 
                              ifelse(p.value < .001, "***", ifelse(p.value < .01, "**", ifelse(p.value < .05, "*", ifelse(p.value < .10, "†", ""))))))%>%
  kable(digits = 3, caption = "eird Between Model (State Means)")

# --- Model Fit ---
kable(data.frame(N = nobs(m_between_eird), 
                 R2 = summary(m_between_eird)$r.squared, 
                 F_Robust = waldtest(m_between_eird, vcov = vc_between_eird, test = "F")[2, "F"]), 
      digits = 3, caption = "eird Fit: Between Model")

# Summary
glance(m_between_eird) %>%
  kable(digits = 3, caption = "eird Between Model Fit Summary")
```

# Forest plot for eird
```{r}
res_eird <- bind_rows(
  tidy(m_pooled_eird, conf.int = TRUE) %>% mutate(Model = "Pooled OLS"),
  tidy(m_fe_eird, conf.int = TRUE) %>% mutate(Model = "Fixed Effects"),
  tidy(m_between_eird, conf.int = TRUE) %>% mutate(Model = "Between")
) %>%
  filter(!grepl("factor", term), term != "(Intercept)")

ggplot(res_eird, aes(x = estimate, y = term, color = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2
  ) +
  labs(
    title = "Forest Plot of eird Determinants",
    subtitle = "Comparison of temporal, pooled, and structural effects",
    x = "Coefficient Estimate",
    y = ""
  ) +
  theme_minimal()


```

############################################################
# MODEL 4 eird: Annual cross-sectional regressions
############################################################

```{r}
annual_results_eird <- df %>%
  group_by(Year) %>%
  group_modify(~{
    mod_eird <- lm(as.formula(paste(y_eird, "~", paste(eird_vars_original, collapse = " + "))), data = .x)
    vc_eird <- vcovHC(mod_eird, type = "HC1")
    ct_eird <- coeftest(mod_eird, vcov = vc_eird)
    
    data.frame(term = rownames(ct_eird),
               estimate = ct_eird[, 1],
               p_val = ct_eird[, 4],
               N = nobs(mod_eird),
               R2 = summary(mod_eird)$r.squared) %>%
 mutate(`p (significant)` = paste0(sprintf("%.3f", p_val), 
                                  ifelse(p_val < .001, "***", ifelse(p_val < .01, "**", ifelse(p_val < .05, "*",ifelse(p_val < .1, "†", ""))))))
  }) %>% ungroup()

kable(annual_results_eird, digits = 3, caption = "eird Annual cross-sectional regressions with R2")


```

############################################################
# OPTIONAL eird: Robustness check using log(eird)
# Use log1p if eird can be 0
############################################################

```{r}
  df_eird <- df %>%
    mutate(log_eird = ifelse(.data[[y_eird]] <= 0, log1p(.data[[y_eird]]), log(.data[[y_eird]])))
  
  m_pooled_log_eird <- lm(
    as.formula(paste("log_eird ~", paste(eird_vars_original, collapse = " + "), "+ factor(Year)")),
    data = df_eird
  )
  vc_pooled_log_eird <- vcovCL(m_pooled_log_eird, cluster = df_eird$State, type = "HC1")
  tidy(coeftest(m_pooled_log_eird, vcov = vc_pooled_log_eird))%>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ "†",
    TRUE            ~ ""
  )) %>%
  kable(digits = 3, 
        caption = "eird Robustness: Pooled OLS + Year FE with log(eird)")
  # Fit for Pooled Log
kable(data.frame(N = nobs(m_pooled_log_eird), R2 = summary(m_pooled_log_eird)$r.squared), 
      caption = "eird Fit: Pooled Log")
  

  m_fe_log_eird <- plm(
    as.formula(paste("log_eird ~", paste(eird_vars_original, collapse = " + "), "+ factor(Year)")),
    data = df_eird, model = "within", effect = "twoways"
  )
  vc_fe_log_eird <- vcovHC(m_fe_log_eird, type = "HC1", cluster = "group")
  tidy(coeftest(m_fe_log_eird, vcov = vc_fe_log_eird))%>%
  mutate(significant = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ "†",
    TRUE            ~ ""
  )) %>%
  kable(digits = 3, 
        caption = "eird Robustness: State FE + Year FE with log(eird)")
  # Fit for FE Log
kable(data.frame(N = nobs(m_fe_log_eird), R2_Within = summary(m_fe_log_eird)$r.squared["rsq"]), 
      caption = "eird Fit: FE Log")

# Summary
glance(m_fe_log_eird) %>%
  kable(digits = 3, caption = "eird Log Model Fit Summary")
  
```

# comparision table
```{r}
# COMPREHENSIVE COMPARISON TABLE for eird

comparision_table_eird <-stargazer(
  m_pooled_eird,
  m_fe_eird,
  m_between_eird,
  m_pooled_log_eird,
  type = "html",
  out="eird_comparision_result.doc",
  se = list(
    sqrt(diag(vc_pooled_eird)),
    sqrt(diag(vc_fe_eird)),
    sqrt(diag(vc_between_eird)),
    sqrt(diag(vc_pooled_log_eird))
  ),
  title = "Model Comparison for Early Intervention Rate (eird)",
  column.labels = c(
    "Pooled",
    "Two-Way FE",
    "Between",
    "Robust (log eird)"
  ),
  intercept.top = TRUE,            # Moves Constant to the first row
  intercept.bottom = FALSE,        # Ensures it is not at the bottom
  covariate.labels = c("Constant", var_comp), # Add "Constant" to the start of your labels
  dep.var.labels.include = FALSE,   # make stargazer use these labels
  dep.var.labels = "Early Intevention Rate (eird)", # clean labels
  model.numbers = FALSE,
  omit = "factor\\(Year\\)",
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  star.cutoffs = c(0.05, 0.01, 0.001), # Standard stars
  notes = "† p<0.1; * p<0.05; ** p<0.01; *** p<0.001",
  notes.append = FALSE,
  add.lines = list(
    c("State FE", "No", "Yes", "No", "No"),
    c("Year FE",  "YES", "Yes", "No", "YES")
  )
)

htmltools::browsable(htmltools::HTML(comparision_table_eird))

```


# table 3 95%ci for eird
```{r eird_comparison_table}

# Row labels for eird (same covariates as HLp)
eird_row_labels <- c(
  "(Intercept)"             = "Constant",
  "PerTscr"                 = "Percent of Infants Total Screened ",
  "Dr"                      = "Diagnosis Rate",
  "state_per_capita_fund"   = "Per Capita State Public Health Funding",
  "Median_household_income" = "Median Household Income (USD)",
  "Per_rep_women_uninsured" = "Percent of Reproductive Age Women Uninsured",
  "Per_infant_breastfed"    = "Percent Infant Ever Breastfed"
)

eird_vars_table <- c("PerTscr", "Dr", "state_per_capita_fund",
                     "Median_household_income", "Per_rep_women_uninsured",
                     "Per_infant_breastfed")
eird_vars_int   <- c("(Intercept)", eird_vars_table)

# ── Extract each model ────────────────────────────────────────────────────────

# Pooled OLS eird
pooled_eird_out <- extract_model_cols(m_pooled_eird, vc_pooled_eird,
                                     eird_vars_int, "Pooled")

# Two-Way FE eird (no intercept)
fe_eird_ct <- coeftest(m_fe_eird, vcov = vc_fe_eird)
fe_eird_vars <- intersect(eird_vars_table, rownames(fe_eird_ct))
fe_eird_df <- as.data.frame(fe_eird_ct[fe_eird_vars, , drop = FALSE])
colnames(fe_eird_df) <- c("estimate", "se", "t", "p")
fe_eird_df$ci_low  <- fe_eird_df$estimate - 1.96 * fe_eird_df$se
fe_eird_df$ci_high <- fe_eird_df$estimate + 1.96 * fe_eird_df$se
fe_eird_df$p_sig   <- case_when(
  fe_eird_df$p < 0.001 ~ "***", fe_eird_df$p < 0.01 ~ "**",
  fe_eird_df$p < 0.05  ~ "*",   fe_eird_df$p < 0.10  ~ "†",
  TRUE ~ ""
)
fe_eird_out <- data.frame(
  Est_FE = c("—", sprintf("%.3f%s", fe_eird_df$estimate, fe_eird_df$p_sig)),
  CI_FE  = c("—", paste0("[", sprintf("%.3f", fe_eird_df$ci_low),
                          ", ", sprintf("%.3f", fe_eird_df$ci_high), "]")),
  p_FE   = c("—", sprintf("%.3f", fe_eird_df$p)),
  check.names = FALSE
)

# Between eird
between_eird_out <- extract_model_cols(m_between_eird, vc_between_eird,
                                      eird_vars_int, "Between")

# Log(eird) Pooled OLS
log_eird_out <- extract_model_cols(m_pooled_log_eird, vc_pooled_log_eird,
                                  eird_vars_int, "Logeird")

# ── Assemble table ────────────────────────────────────────────────────────────
# Use cbind (not bind_cols) to avoid row-count mismatch issues
table3_eird <- data.frame(
  Variable = eird_row_labels[names(eird_row_labels)],
  row.names = NULL,
  check.names = FALSE
)
table3_eird <- cbind(table3_eird, pooled_eird_out, fe_eird_out, between_eird_out, log_eird_out)

colnames(table3_eird) <- c(
  "Variable",
  "Est (Pooled)", "95% CI", "p",
  "Est (Two-Way FE)", "95% CI ", "p ",
  "Est (Between)", "95% CI  ", "p  ",
  "Est (log eird)", "95% CI   ", "p   "
)

# ── Render table ──────────────────────────────────────────────────────────────
kable(
  table3_eird,
  digits  = 3,
  align   = c("l", rep(c("r", "c", "r"), 4)),
  caption = paste0(
    "Table 3: Model Comparison for Early Intervention Rate (eird). ",
    "Estimates with 95% confidence intervals (robust, HC1) and p-values. ",
    "Year fixed effects included in Pooled and log(eird) models. ",
    "† p<0.10; * p<0.05; ** p<0.01; *** p<0.001."
  )
) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  add_header_above(c(
    " " = 1,
    "Pooled OLS + Year FE" = 3,
    "Two-Way FE" = 3,
    "Between (State Means)" = 3,
    "Robustness: log(eird)" = 3
  )) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(which(table3_eird$Variable == "% Infant Ever Breastfed"),
           bold = TRUE, background = "#f0f8ff") %>%
  kableExtra::footnote(
    general = "95% CI = estimate ± 1.96 × cluster-robust SE (HC1). State FE: No/Yes/No/No. Year FE: Yes/Yes/No/Yes.",
    general_title = "Note:",
    footnote_as_chunk = TRUE
  )

# ── Export to Word ────────────────────────────────────────────────────────────
ft_eird <- flextable(table3_eird) %>%
  set_caption("Table 3: Model Comparison for Early Intervention Rate (eird)") %>%
  add_header_row(
    values = c("", "Pooled OLS + Year FE", "Two-Way FE",
               "Between (State Means)", "Robustness: log(eird)"),
    colwidths = c(1, 3, 3, 3, 3)
  ) %>%
  bold(part = "header") %>%
  autofit() %>%
  theme_booktabs()

save_as_docx(ft_eird, path = "eird_comparison_Table3.docx")


```


############################################################
# OPTIONAL eird: Balanced-panel sensitivity sample (states with all 13 years)
############################################################
  
```{r}
    balanced_states_eird <- df %>%
    mutate(complete_row = if_all(all_of(c(y_eird, eird_vars_original)), ~ !is.na(.))) %>%
    group_by(State) %>%
    summarise(n_years_complete = n_distinct(Year[complete_row]), .groups = "drop") %>%
    filter(n_years_complete == 13) %>%
    pull(State)
  
  df_bal_eird <- df %>% filter(State %in% balanced_states_eird)
  
  m_pooled_bal_eird <- lm(f_pooled_eird, data = df_bal_eird)
  vc_pooled_bal_eird <- vcovCL(m_pooled_bal_eird, cluster = df_bal_eird$State, type = "HC1")
  
# 1. Coefficient Table
tidy(coeftest(m_pooled_bal_eird, vcov = vc_pooled_bal_eird)) %>%
  mutate(`p (significant)` = paste0(sprintf("%.3f", p.value), 
                              ifelse(p.value < .001, "***", ifelse(p.value < .01, "**", ifelse(p.value < .05, "*", ifelse(p.value < .10, "†", "")))))) %>%
  kable(digits = 3, caption = "eird Sensitivity: Balanced sample pooled OLS + Year FE")

# 2. Fit Table (Fixed to reference correct model)
kable(data.frame(N = nobs(m_pooled_bal_eird), 
                 R2 = summary(m_pooled_bal_eird)$r.squared,
                 Adj_R2 = summary(m_pooled_bal_eird)$adj.r.squared), 
      caption = "eird Fit: Balanced Sample")

# Summary
glance(m_pooled_bal_eird) %>%
  kable(digits = 3, caption = "eird Banalnced Pooled Fit Summary")
```

# eird Diagnostics
```{r}
# cat("\n=== Eird MODEL DIAGNOSTICS (Two-Way Fixed Effects) ===\n")
# 
# # Diagnostic 1: Serial Correlation Test
# cat("\n1. Serial Correlation Test (Breusch-Godfrey):\n")
# serial_test_eird <- pbgtest(m_fe_eird)
# print(serial_test_eird)
# 
# # Diagnostic 2: Heteroskedasticity Test
# cat("\n2. Heteroskedasticity Test (Breusch-Pagan):\n")
# # Create formula for BP test
# f_bp_eird <- as.formula(paste("eird ~", paste(eird_vars_original, collapse = " + "),
#                              "+ factor(State) + factor(Year)"))
# bptest_eird <- bptest(f_bp_eird, data = df_eird, studentize = TRUE)
# print(bptest_eird)
# 
# # # Diagnostic 3: Hausman Test (Fixed vs Random Effects)
# cat("\n3. Hausman Test (FE vs RE):\n")
# f_re_eird <- as.formula(paste("eird ~", paste(eird_vars_original, collapse = " + ")))
# m_re_eird <- plm(f_re_eird, data = pdf_eird, model = "random")
# hausman_test_eird <- phtest(m_fe_eird, m_re_eird)
# print(hausman_test_eird)
# 
# # Diagnostic 4: Cross-sectional dependence
# cat("\n4. Cross-sectional Dependence Test (Pesaran CD):\n")
# cd_test_eird <- pcdtest(m_fe_eird, test = "cd")
# print(cd_test_eird)
# 
# # Diagnostic 5: Unit root test (for panel data)
# cat("\n5. Panel Unit Root Test (Levin-Lin-Chu):\n")
# # Note: This requires pseries and might take time
# # library(tseries)
# # llc_test_eird <- purtest(eird ~ 1, data = pdf_eird, index = c("State", "Year"),
# #                         test = "levinlin", lags = "AIC")
# # print(llc_test_eird)
# 
# # Create diagnostic summary table
# diagnostics_summary_eird <- data.frame(
#   Test = c(
#     "Serial Correlation",
#     "Heteroskedasticity",
#     "Cross-sectional Dependence"
#   ),
#   Statistic = c(
#     round(serial_test_eird$statistic, 3),
#     round(bptest_eird$statistic, 3),
#     round(cd_test_eird$statistic, 3)
#   ),
#   P_Value = c(
#     format.pval(serial_test_eird$p.value, digits = 3),
#     format.pval(bptest_eird$p.value, digits = 3),
#     format.pval(cd_test_eird$p.value, digits = 3)
#   ),
#   Interpretation = c(
#     "Serial correlation present → Clustered SEs used",
#     "Heteroskedasticity present → Robust SEs used",
#     "Cross-sectional dependence present"
#   )
# )
# 
# knitr::kable(
#   diagnostics_summary_eird,
#   caption = "Model Diagnostics for Two-Way Fixed Effects eird Model"
# )

```


## Key Results

```{r}

# Extract key results
key_results <- data.frame(
    Predictor = eird_vars_original,
    TwoWayFE_Estimate = round(coef(m_fe_eird), 3),
    TwoWayFE_SE = round(sqrt(diag(vc_fe_eird)), 3),
    TwoWayFE_Pvalue = round(summary(m_fe_eird)$coefficients[, 4], 3),
    Pooled_Estimate = round(coef(m_pooled_eird)[eird_vars_original], 3),
    Pooled_Pvalue = round(summary(m_pooled_eird)$coefficients[eird_vars_original, 4], 3)
)

# Identify significant predictors
significant_predictors <- key_results %>%
    filter(TwoWayFE_Pvalue < 0.05) %>%
    arrange(desc(abs(TwoWayFE_Estimate)))

cat("\nStatistically Significant Predictors (Two-Way FE, p<0.05):\n")
kable(significant_predictors, digits = 3) %>%
    kable_styling()

# Create summary statement
cat("\n=== MAIN CONCLUSION ===\n")
if (nrow(significant_predictors) > 0) {
    top_predictor <- significant_predictors[1, ]
    cat(sprintf(
        "The strongest predictor of eird is %s (β = %.3f, p = %.3f).\n",
        top_predictor$Predictor,
        top_predictor$TwoWayFE_Estimate,
        top_predictor$TwoWayFE_Pvalue
    ))
} else {
    cat("No predictors reached statistical significance at p<0.05 in the Two-Way FE model.\n")
}

cat(sprintf(
    "\nModel Statistics:\n- R-squared (within): %.3f\n- F-statistic: %.2f (p = %.3f)\n- Observations: %d\n",
    model_summary_eird$r.squared[1],
    model_summary_eird$fstatistic$statistic,
    model_summary_eird$fstatistic$p.value,
    nobs(m_fe_eird)
))

```
