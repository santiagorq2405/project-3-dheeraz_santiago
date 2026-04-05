# Project 3: Credit Repayment Risk Assessment

**MATH-516 Applied Statistics, EPFL**

**Authors:** Shanmukha Sai Dheeraz Chavali & Santiago Rivadeneira Quintero

**Date:** 2026-04-05

## Overview

This project investigates whether enrollment in a Payment Protection Plan (PPP) causally affects the risk of credit card payment default. We estimate the Average Causal Effect (ACE) using outcome regression (standardization) and inverse probability weighting (IPW), each applied with four covariate adjustment sets: all 22 covariates, a LASSO-selected subset, and two sets derived from causal discovery algorithms (PC and SI-HITON-PC). Uncertainty is quantified via nonparametric bootstrap with 1000 resamples.

## Repository Contents

| File | Description |
|------|-------------|
| `report.pdf` | Final PDF report (generated from `report.qmd`) |
| `report.qmd` | Quarto source file containing all code to reproduce the analysis |
| `Project3.R` | Standalone R script with the full analysis pipeline |
| `references.bib` | BibTeX bibliography |
| `updated_datasets/low3007upd.csv` | Dataset (500 customers, 24 variables) |

## Requirements

- **R** version 4.5.1 or later
- **Quarto** version 1.6 or later
- **LaTeX** distribution (e.g., TeX Live 2025 or TinyTeX)

### R Packages

```r
install.packages(c(
  "tidyverse", "scales", "patchwork",
  "knitr", "kableExtra",
  "glmnet", "bnlearn", "boot", "igraph"
))
```

## How to Reproduce the PDF Report

1. Clone the repository:
   ```bash
   git clone https://github.com/santiagorq2405/project-3-dheeraz_santiago.git
   cd project-3-dheeraz_santiago
   ```

2. Install the required R packages (see above).

3. Render the report:
   ```bash
   quarto render report.qmd --to pdf
   ```

   This will execute all R code chunks and produce `report.pdf`.

## Data

The dataset (`updated_datasets/low3007upd.csv`) contains 500 simulated customer records based on the UCI credit card repayment dataset (Yeh and Lien, 2009), with an additional binary treatment variable indicating PPP enrollment. Variables include demographics, credit limit, and six months of payment history.
