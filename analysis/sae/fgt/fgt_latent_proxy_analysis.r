# -----------------------
# Construction of a proxied wealth indicator because of absense of monetary survey variable
# -----------------------

# Poverty line calibration
# z=quantile(hv271,0.375) to ensure that the FGT_DHS is approximately equal to the FGT_official using 37.5% poverty rate
# as reported by the ECAM IV database {TODO}. retry with ECAM V value of 37.7%

library(haven)
library(dplyr)
library(survey)


dhs <- read_dta("CMHR7FL.DTA")  # Cameroon DHS 2018 HR file

# preparation of varaibles

dhs <- dhs %>%
  mutate(
    weight = hv005 / 1e6,   # DHS weights
    welfare = hv271         # wealth index score
  ) %>%
  filter(!is.na(welfare), !is.na(sub_div_id))


# survey design

dhs_design <- svydesign(
  id = ~hv021,
  strata = ~hv022,
  weights = ~weight,
  data = dhs,
  nest = TRUE
)

# calibration of the poverty line

poverty_rate <- 0.375 # according to ECAM IV results

z <- as.numeric(
  svyquantile(
    ~welfare,
    dhs_design,
    quantiles = poverty_rate,
    ci = FALSE
  )
)


# Estimating FGT indicators at sub-division level

# [TODO] rejoin survey dataset and spatial dataset using sub-division attribute sub_div_id

fgt_by_area <- function(alpha) {
  svyby(
    ~ ((z - welfare) / z)^alpha * I(welfare < z),
    ~ sub_div_id,
    dhs_design,
    svymean,
    na.rm = TRUE
  )
}

# Poverty headcount (FGT(0))

fgt0 <- svyby(
  ~ I(welfare < z),
  ~ sub_div_id,
  dhs_design,
  svymean,
  na.rm = TRUE
)
names(fgt0)[2] <- "poverty_headcount"

# Poverty gap (FGT(1))

fgt1 <- fgt_by_area(alpha = 1)
names(fgt1)[2] <- "poverty_gap"


# Poverty severity (FGT(2))

fgt2 <- fgt_by_area(alpha = 2)
names(fgt2)[2] <- "poverty_severity"


# merging the results into one dataframe

poverty_subdiv <- fgt0 %>%
  left_join(fgt1, by = "sub_div_id") %>%
  left_join(fgt2, by = "sub_div_id")

print(poverty_subdiv)
