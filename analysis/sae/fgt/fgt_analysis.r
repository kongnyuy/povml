# install.packages(c("survey", "dplyr"))
library(survey)
library(dplyr)



root <- "F:/Workspaces/academics/masters_thesis/"
setwd(root)

# Load the data
#data <- read.csv("data/processed/main_meaningfull_names_dhs_hr_gis_2018.csv")
data <- read.csv("data/processed/main_dhs_names_dhs_hr_gis_2018.csv")
data_named <- read.csv("data/processed/main_meaningfull_names_dhs_hr_gis_2018.csv")

View(head(data_named))

independent_vars <- c("hv106_01","hv107_01",#"hv109_01", #education_index
                      "hv206","hv209","hv213","hv201" #living_standards_index
                      #,"hv271" # wealth and employment index    
)

# model specification

z <- 1.90         # World Bank international poverty line in USD PPP

# survey design

dhs_design <- svydesign(
  id      = ~cluster,
  strata  = ~strata,
  weights = ~weight,
  data    = cameroon_dhs,
  nest    = TRUE
)


# --------------
# Functions for computing the FGT Measures
# --------------

# definition of a generic parametric model for computing FGT measures


compute_fgt <- function(design, poverty_line, alpha) {
  svyby(
    formula = ~ ((poverty_line - cons_pc) / poverty_line)^alpha * I(cons_pc < poverty_line),
    by      = ~ sub_div,
    design  = design,
    FUN     = svymean,        # weighted mean
    na.rm   = TRUE,
    vartype = c("se", "ci")
  ) -> fgt

  names(fgt) <- c("sub_div", paste0("FGT", alpha), paste0("SE_FGT", alpha), paste0("CI_L", alpha), paste0("CI_U", alpha))
  return(fgt)
}


# -------------
# Compute Headcount, Gap, and Severity
# -------------

# Headcount (alpha = 0)
fgt0 <- compute_fgt(dhs_design, z, alpha = 0)

# Poverty Gap (alpha = 1)
fgt1 <- compute_fgt(dhs_design, z, alpha = 1)

# Poverty Severity (alpha = 2)
fgt2 <- compute_fgt(dhs_design, z, alpha = 2)

# Combine results
poverty_results <- fgt0 %>%
  left_join(fgt1, by = "sub_div") %>%
  left_join(fgt2, by = "sub_div")

print(poverty_results)


# ---------------
# National aggregates
# ---------------

national_fgt0 <- svymean(~ I(cons_pc < z), dhs_design)
national_fgt1 <- svymean(~ ((z - cons_pc)/z) * I(cons_pc < z), dhs_design)
national_fgt2 <- svymean(~ ((z - cons_pc)/z)^2 * I(cons_pc < z), dhs_design)


