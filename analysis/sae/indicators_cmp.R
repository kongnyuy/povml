library(dplyr)
library(ggplot2)

root <- "F:/Workspaces/academics/masters_thesis/"
setwd(root)

#subdiv_aggs = read.csv("data/processed/out/subdiv_result.csv")
subdiv_aggs1 = readRDS("data/processed/subdiv_result.rds")

colnames(subdiv_aggs1)[2] = "WealthScoreMean"
#colnames(subdiv_aggs)[6] = "WealthQauntile"


# Charting theme ----------------------------------------------------------

# Global theme ------------------------------------------------------------
gtheme <- theme_minimal(base_size = 12) + theme(
  text = element_text(color = "black"),
  axis.text = element_text(color = "black"),
  axis.title = element_text(color = "black"),
  plot.title = element_text(color = "black", face = "bold"),
  #panel.grid = element_line(color = "gray", alpha = 0.1),
  axis.line = element_line(color = "orange"),
  legend.text = element_text(color = "black"),
  legend.title = element_text(color = "black"),
)

FILL = "#fdcb7c"
COLOR = "black"


theme_set(gtheme)


library(corrplot)

vars <- subdiv_aggs[, c("WealthScoreMean", "Head_Count", "Poverty_Gap", "Gini")]

cor_mat <- cor(vars, use = "complete.obs")

corrplot(cor_mat, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black")



# correlation plot 2 ------------------------------------------------------



library(corrplot)

# Calculate correlations (numeric columns only)
cor_data <- cor(subdiv_aggs1[, c("WealthScoreMean", "Head_Count", "Poverty_Gap", "Gini")], 
                use = "complete.obs")

# Plot
corrplot(cor_data, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black", # Show correlation numbers
         tl.col = "black", 
         diag = FALSE)




# Pairwise scatter plots --------------------------------------------------



# pair plots --------------------------------------------------------------

library(GGally)

# Assuming combined_df has columns: subdivision, WealthScoreMean, Head_Count, Poverty_Gap, Gini
ggpairs(subdiv_aggs1, 
        columns = c("WealthScoreMean", "Head_Count", "Poverty_Gap", "Gini"),
        title = "Correlation Matrix of Wealth score & Poverty indicator Metrics",
        lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.5))) 

# pair plot 2 -------------------------------------------------------------

library(GGally)

ggpairs(
  subdiv_aggs[, c("WealthScoreMean", "Head_Count", "Poverty_Gap", "Gini")],
  upper = list(continuous = "cor"),
  lower = list(continuous = "smooth"),
  diag = list(continuous = "densityDiag")
)


# Specific target binary plots --------------------------------------------

library(ggplot2)

ggplot(subdiv_aggs, aes(x = WealthScoreMean, y = Poverty_Gap)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal()


ggplot(subdiv_aggs, aes(x = WealthScoreMean, y = Poverty_Gap)) +
  geom_point(aes(size = Head_Count, color = Gini), alpha = 0.7) +
  scale_color_viridis_c() +
  theme_minimal()


# 3d plot of all variables ------------------------------------------------



library(plotly)

plot_ly(subdiv_aggs,
        x = ~WealthScoreMean,
        y = ~Poverty_Gap,
        z = ~Gini,
        size = ~Head_Count,
        type = "scatter3d",
        mode = "markers")



# Relationship heatmap ----------------------------------------------------

library(corrplot)

# Calculate correlations (numeric columns only)
cor_data <- cor(subdiv_aggs1[, c("WealthScoreMean", "Head_Count", "Poverty_Gap", "Gini")], 
                use = "complete.obs")

# Plot
corrplot(cor_data, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black", # Show correlation numbers
         tl.col = "black", 
         diag = FALSE)



# Correlation heatmap -----------------------------------------------------

library(ggplot2)
library(viridis)

ggplot(subdiv_aggs1, aes(x = WealthScoreMean, y = Gini, size = Head_Count, color = Poverty_Gap)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c(option = "magma") + # High contrast for Poverty Gap
  labs(title = "Wealth vs. Inequality by Subdivision",
       subtitle = "Size represents Headcount; Color represents Poverty Gap",
       x = "Mean Wealth Score",
       y = "Gini Coefficient (Inequality)") +
  theme_minimal() +
  geom_text(aes(label = subdivision), check_overlap = TRUE, size = 3, vjust = 1.5)


# Area effect of poverty indicator ----------------------------------------

# Compare Gini vs Wealth rank
subdiv_aggs1 %>%
  mutate(subdivision = reorder(subdivision, WealthScoreMean)) %>%
  ggplot(aes(x = WealthScoreMean, y = subdivision)) +
  geom_segment(aes(xend = 0, yend = subdivision), color = "grey") +
  geom_point(aes(color = Gini), size = 3) +
  scale_color_distiller(palette = "RdYlGn") +
  labs(title = "Subdivisions Ranked by Wealth",
       subtitle = "Color indicates Gini Coefficient (Red = High Inequality)") +
  theme_minimal()
