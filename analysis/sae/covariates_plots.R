library(ggplot2)
library(ggthemes)
library(dplyr)
library(patchwork)
library(gridExtra)



root <- "F:/Workspaces/academics/masters_thesis/"
setwd(root)

df <- readRDS("data/processed/admin321_covariates.rds")


# covariataes: "cluster,id,weight,strata,age,wealth,wealthscore,wealthscoreshifted
#,reg,typeresidence,edu,water_drink,electricity,automobile,cook_fuel,toilet,mat_floor
#,malaria_rdt,anemia_level,household_size,num_rooms,region,division,subdivision,crowding"



# Global theme ------------------------------------------------------------
gtheme <- theme_minimal(base_size = 12) + theme(
  text = element_text(color = "black"),
  axis.text = element_text(color = "black"),
  axis.title = element_text(color = "black"),
  plot.title = element_text(color = "black", face = "bold"),
  #panel.grid = element_line(color = "gray", alpha = 0.1),
  #axis.line = element_line(color = "orange"),
  legend.text = element_text(color = "black"),
  legend.title = element_text(color = "black"),
)

FILL = "#fdcb7c"
COLOR = "black"


theme_set(gtheme)

ggplot(df, aes(x = edu)) + geom_bar(color=COLOR, fill=FILL)
# pie charts
ggplot(df, aes(x = "", y = anemia_level, fill = anemia_level)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() # Removes background grid and axes


#ggplot(data, aes(x = edu)) + geom_bar(fill = "steelblue", color = "white") + theme_minimal() + labs(title = "School attendance", x = "Educational level",y = "Count")



# Plots -------------------------------------------------------------------


pweight = ggplot(df, aes(x = weight)) +
  geom_histogram(bins = 30, colour = "white", fill=FILL ) +
  labs(title = "Distribution of Sampling Weights")

#pweight


pstrata = ggplot(df, aes(x = factor(strata))) +
  geom_bar(color="white", fill=FILL) + coord_flip() +
  labs(title = "Observations by Strata")
  
#pstrata


phousehold_size = ggplot(df, aes(x = household_size)) +
  geom_histogram(binwidth = 1, color=COLOR, fill=FILL) +
  labs(title = "Household Size Distribution")

#phousehold_size

pedu = ggplot(df, aes(x = edu)) +
  geom_bar(color=COLOR, fill=FILL) + coord_flip() +
  labs(title = "Education Level Distribution")

#pedu  
  

pwealthscoreshifted = ggplot(df, aes(x = wealthscoreshifted)) +
  geom_density(fill = FILL, alpha = 0.4) +
  labs(title = "Shifted Wealth Score Density (score + min(score))")

#pwealthscoreshifted

pcrowding = ggplot(df, aes(x = crowding)) +
  geom_histogram(bins = 30, binwidth = 1, color=COLOR, fill=FILL) +
  labs(title = "Crowding Index Distribution")

#pcrowding

pwealthscore = ggplot(df, aes(x = wealthscore)) +
  geom_density(fill = "skyblue", alpha = 0.4) +
  labs(title = "Wealth Score Density")

#pwealthscore


pwealth = ggplot(df, aes(x = wealth)) +
  geom_bar(color=COLOR, fill=FILL) + coord_flip() +
  labs(title = "Wealth Distribution")

#pwealth

pelec = ggplot(df, aes(x = factor(electricity))) +
  geom_bar(color=COLOR, fill=FILL) +
  labs(title = "Access to Electricity")

#pelec

pautomobile = ggplot(df, aes(x = factor(automobile))) +
  geom_bar(color=COLOR, fill=FILL) +
  labs(title = "Automobile Ownership")

#pautomobile



pcookfuel = ggplot(df, aes(x = cook_fuel)) +
  geom_bar(color=COLOR, fill=FILL) + coord_flip() +
  labs(title = "Cooking Fuel Type")

#pcookfuel


ptoilet = ggplot(df, aes(x = toilet)) +
  geom_bar(color=COLOR, fill=FILL) + coord_flip() +
  labs(title = "Toilet Type")

#ptoilet

pmat_floor = ggplot(df, aes(x = factor(mat_floor))) +
  geom_bar(color=COLOR, fill=FILL) + coord_flip() +
  labs(title = "Floor Material")

#pmat_floor


pwater_drink = ggplot(df, aes(x = water_drink)) +
  geom_bar(color=COLOR, fill=FILL) + coord_flip() +
  labs(title = "Drinking Water Source")

#pwater_drink

pnum_rooms = ggplot(df, aes(x = num_rooms)) +
  geom_histogram(bins = 1, binwidth = 1, color=COLOR, fill=FILL) +
  labs(title = "Number of Rooms")

#pnum_rooms


pmalaria = ggplot(df, aes(x = factor(malaria_rdt))) +
  geom_bar(color=COLOR, fill=FILL) + coord_flip() +
  labs(title = "Malaria RDT Result")

#pmalaria


panemia = ggplot(df, aes(x = anemia_level)) +
  geom_bar(color=COLOR, fill=FILL) +
  labs(title = "Anemia Level Distribution")

#panemia


pregion = ggplot(df, aes(x = region)) +
  geom_bar(color=COLOR, fill=FILL) + coord_flip() +
  labs(title = "Distribution by Region")

#pregion

pdivision = ggplot(df, aes(x = division)) +
  geom_bar(color=COLOR, fill=FILL) + coord_flip() +
  labs(title = "Distribution by Division")

#pdivision

psubdiv = ggplot(df, aes(x = subdivision)) +
  geom_bar(color=COLOR, fill=FILL) + coord_flip() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Distribution by Subdivision")

#psubdiv


ptyperesidence = ggplot(df, aes(x = typeresidence)) +
  geom_bar(color=COLOR, fill=FILL) +
  labs(title = "Urban vs Rural")

#ptyperesidence 

pregion_wealthscore = ggplot(df, aes(x = region, y = wealthscore)) +
  geom_boxplot(colour = COLOR, fill = FILL) + coord_flip() + 
  labs(title = "Wealth Score by Region")

#pregion_wealthscore

ptyperesi_wealthscore = ggplot(df, aes(x = typeresidence, y = wealthscore)) +
  geom_boxplot(colour = COLOR, fill = FILL) +
  labs(title ="Wealth score by settlement status")

#ptyperesi_wealthscore

phouse_wealthscore = ggplot(df, aes(x = household_size, y = wealthscore)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Wealth distribution by household size")

#phouse_wealthscore



# Plotting a them in a grid -----------------------------------------------

wealth_plots <- c(pwealth, pwealthscore,pwealthscoreshifted)
patchwork::wrap_plots(wealth_plots, nrow = 2, ncol = 2)

survey_attribs_plots <- c(pweight, pstrata)
patchwork::wrap_plots(survey_attribs_plots, nrow = 1, ncol = 2)

living_stds_plots <- c(pelec, pmat_floor, pwater_drink, pautomobile)
patchwork::wrap_plots(living_stds_plots, nrow = 2, ncol = 2)

household_plots <- c(pcookfuel, pnum_rooms, ptoilet,phousehold_size,pcrowding)
patchwork::wrap_plots(household_plots, nrow = 3, ncol = 2)

health_plots <- c(pmalaria, panemia)
patchwork::wrap_plots(health_plots, nrow = 1, ncol = 2)

edu_plots <- c(pedu)
patchwork::wrap_plots(edu_plots, nrow = 1, ncol = 1)

area_disto_plots <-c(pregion,ptyperesidence)
patchwork::wrap_plots(area_disto_plots, nrow = 2, ncol = 2)

pdivision

#psubdiv  #too condensed


bi_attrib_plots <- c(pregion_wealthscore, ptyperesi_wealthscore,phouse_wealthscore)
patchwork::wrap_plots(bi_attrib_plots, nrow = 2, ncol = 2)



