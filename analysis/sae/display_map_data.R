
setwd("F:\\Workspaces\\academics\\datasets\\cameroon\\gis\\geojson")


savepath = "F:\\Workspaces\\academics\\masters_thesis\\outputs\\maps\\genericPlots\\"

generic_plot_path <- function(fname) {
  return(paste0(savepath, fname))
}


library(ggplot2)
library(sf)
library(plotly)
library(dplyr)

#geo1 <- readOGR("data/dataset/cameroon/gis/geojson/gadm41_CMR_1.json")
# Import a geojson or shapefile
admin1 <- read_sf("gadm41_CMR_1.json")

admin1 <- admin1 %>%  mutate(
  region = VARNAME_1
)

head(admin1)

a1 <- ggplot(admin1) +
  geom_sf(color = "white", aes(fill = region)) +
  geom_sf_label(aes(label=region), size=2)
  theme(legend.position = "none")

#static map render
a1
ggsave(generic_plot_path("cm_admin1.png"), plot = a1)
  
# Make the map interactive
ggplotly(p)

#----- Admin 2
admin2 <- read_sf("gadm41_CMR_2.json")

admin2 <- admin2 %>%  mutate(
  division = VARNAME_2
)

head(admin2)

a2 <- ggplot(admin2) + 
      geom_sf(color = "white", aes(fill = division)) + 
      geom_sf_text(aes(label = division), size=1) +
      theme(legend.position = "left")
      
a2
ggsave(generic_plot_path("cm_admin2.svg"), plot = a2)
#interactive plot
ggplotly(a2, tooltip = c("division"))

#----- Admin 3
admin3 <- read_sf("gadm41_CMR_3.json")

admin3 <- admin3 %>%  mutate(
  subdivision = VARNAME_3
)

head(admin3)

a3 <- ggplot(admin3) + 
  geom_sf(color = "white", aes(fill = subdivision)) + 
  geom_sf_text(aes(label = subdivision), size=1) +
  theme(legend.position = "left")

a3

#ggsave(generic_plot_path("cm_admin3.svg"), plot = a3, width = 6, height = 4, units = "in")
ggsave(generic_plot_path("cm_admin3.svg"), plot = a3)

#interactive plot
ggplotly(a3)

#save as svg
#ggsave("cm_admin_3.svg", plot = a3, width = 40, height = 40)
