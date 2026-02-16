# ln(ci​)=Xi​β+εi
# where ci : is the pseudo consumption latent dependent variable

# Pseudo-consumption model

library(lme4)

dhs <- dhs %>%
  mutate(
    ln_c = scale(hv271),   # proxy for log consumption
    hhsize = hv009,
    urban = ifelse(hv025 == 1, 1, 0)
  )

welfare_model <- lmer(
  ln_c ~ hv206 + hv205 + hv201 + hv216 +
         hhsize + urban + hv105 + hv106 +
         (1 | sub_div_id),
  data = dhs,
  weights = weight
)



# -> Monte Carlo simulation (ELL method)

R <- 100
sigma_eps <- sigma(welfare_model)

sim_data <- replicate(R, {
  u_draw <- rnorm(length(unique(dhs$sub_div_id)), 0,
                  sd = sqrt(VarCorr(welfare_model)$sub_div_id[1]))
  eps_draw <- rnorm(nrow(dhs), 0, sigma_eps)

  exp(predict(welfare_model) + eps_draw)
})


# -> Poverty estimation (Monte Carlo FGT)

z <- poverty_line  # national line

fgt0_mc <- apply(sim_data, 2, function(csim) {
  svymean(~ I(csim < z), dhs_design)
})

FGT0_unit <- mean(fgt0_mc)


## NB: the approach implemented above is appropriate when census microdata is absent
## This is unit-level SAE, WB-compliant when census microdata are absent.
