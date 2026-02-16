library(dplyr)
library(emdi)

root <- "F:/Workspaces/academics/masters_thesis/"
setwd(root)


e3 <- readRDS("data/processed/out/ebp_model_admin3.rds")
e2 <- readRDS("data/processed/out/ebp_model_admin2.rds")
e1 <- readRDS("data/processed/out/ebp_model_admin1.rds")




# direct

d1 = readRDS("data/processed/out/model_direct_admin1.rds")
d2 = readRDS("data/processed/out/model_direct_admin2.rds")
d3 = readRDS("data/processed/out/model_direct_admin3.rds")



# Save csv versions -------------------------------------------------------

write.csv(e3$ind, file ="data/processed/out/csv/cm.subdiv.ebp.ind.csv")
write.csv(e3$MSE, file ="data/processed/out/csv/cm.subdiv.ebp.mse.csv")

write.csv(e2$ind, file ="data/processed/out/csv/cm.div.ebp.ind.csv")
write.csv(e2$MSE, file ="data/processed/out/csv/cm.div.ebp.mse.csv")

write.csv(e1$ind, file ="data/processed/out/csv/cm.region.ebp.ind.csv")
write.csv(e1$MSE, file ="data/processed/out/csv/cm.region.ebp.mse.csv")



emdi::write.excel(e3, file ="data/processed/out/subdiv_ebp_model.xlsx")
emdi::write.excel(e2, file ="data/processed/out/div_ebp_model.xlsx")
emdi::write.excel(e1, file ="data/processed/out/region_ebp_model.xlsx")

emdi::write.excel(d3, file ="data/processed/out/subdiv_direct_model.xlsx")
emdi::write.excel(d2, file ="data/processed/out/div_direct_model.xlsx")
emdi::write.excel(d1, file ="data/processed/out/region_direct_model.xlsx")

