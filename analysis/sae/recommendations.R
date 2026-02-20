library(dplyr)
library(stringr)



root <- "F:/Workspaces/academics/masters_thesis/"
setwd(root)

subdiv_aggs <- read.csv("data/processed/out/subdiv_result.csv")

View(subdiv_aggs)

thres = 89610  # 60% of the median(wealthindicator) Eurostat


# poor or not poor --------------------------------------------------------

subdivs_pov_cond = subdiv_aggs %>% mutate(
  ##conditon = Mean < thres
  status = ifelse(Mean < thres, yes = "poor", no = "rich")
)

write.csv(subdivs_pov_cond, "data/processed/out/subdiv.aggs.with_poor_cond.csv") # poor = true | not poor = false




below_mean_subdivs = subdiv_aggs %>% filter(Mean < thres)

# text searialization of those areas
bm_subdivs_str = stringr::str_flatten(below_mean_subdivs$subdivision, collapse = ',')


View(seg_data)



# Establishment of wealth index quintiles for generated aggregates --------

wmean = sort(subdiv_aggs$Mean, decreasing = TRUE)

quintile_len = 5 # -> 20%

wmean_max = wmean[1]
wmean_min = wmean[360]
tmargin = (wmean_max - wmean_min) / quintile_len # 272893.4

#quintiles: Richest, Richer, Middle, Poorer, Poorest


fun_calc_quintile_range <- function(i,n = 5) {
  l = wmean_min + ((n - i) * tmargin ) + (n - i)
  h = l + tmargin
  return (c(l,h))
}


# f2

fun_calc_quintile_range <- function(i,n = 5) {
  l = wmean_min + ((n - i) * tmargin )
  h = l + tmargin
  return (c(l,h))
}

q_Richest = fun_calc_quintile_range(1)
q_Richer = fun_calc_quintile_range(2)
q_Middle = fun_calc_quintile_range(3)
q_Poorer = fun_calc_quintile_range(4)
q_Poorest = fun_calc_quintile_range(5)


# Quintiles end --------------


# update subdiv_aggs with quintile information ----------------------------

aug = subdiv_aggs %>% mutate(
  wealthScore = case_when(
    Mean >= q_Richest[1] & Mean <= q_Richest[2] ~ "Richest",
    Mean >= q_Richer[1] & Mean <= q_Richer[2] ~ "Richer",
    Mean >= q_Middle[1] & Mean <= q_Middle[2] ~ "Middle",
    Mean >= q_Poorer[1] & Mean <= q_Poorer[2] ~ "Poorer",
    Mean >= q_Poorest[1] & Mean <= q_Poorest[2] ~ "Poorest"
  )
    
  )

# End of quantiles / binning augmenteation


qs <- quantile(subdiv_aggs$Mean, probs = seq(0, 1, 0.2), na.rm = TRUE)

aug1 <- subdiv_aggs %>%
  mutate(
    wealthScore = case_when(
      Mean <= qs[2] ~ "Poorest",
      Mean <= qs[3] ~ "Poorer",
      Mean <= qs[4] ~ "Middle",
      Mean <= qs[5] ~ "Richer",
      TRUE          ~ "Richest"
    )
  )

write.csv(aug1, "data/processed/out/subdiv_result_with_quantiles.csv")
