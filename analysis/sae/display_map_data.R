
setwd("F:\\Workspaces\\academics\\datasets\\cameroon\\gis\\geojson")

savepath = "F:\\Workspaces\\academics\\masters_thesis\\outputs\\maps\\genericPlots\\"

generic_plot_path <- function(fname) {
  return(paste0(savepath, fname))
}


library(ggplot2)
library(sf)
library(plotly)


# Import a geojson or shapefile
admin1 <- read_sf("gadm41_CMR_1.json")

head(admin1)
colnames(admin1)

a1 <- ggplot(admin1) +
  geom_sf(color = "white", aes(fill = VARNAME_1)) +
  geom_sf_label(aes(label=VARNAME_1), size=2)
  theme(legend.position = "none")

#static map render
a1
ggsave(generic_plot_path("cm_admin1.svg"), plot = a1)
  
# Make the map interactive
ggplotly(p)

#----- Admin 2
admin2 <- read_sf("gadm41_CMR_2.json")

head(admin2)

a2 <- ggplot(admin2) + 
      geom_sf(color = "white", aes(fill = VARNAME_2)) + 
      geom_sf_text(aes(label = VARNAME_2), size=1) +
      theme(legend.position = "none")
      
a2
ggsave(generic_plot_path("cm_admin2.svg"), plot = a2)
#interactive plot
ggplotly(a2, tooltip = c("VARNAME_2"))

#----- Admin 3
admin3 <- read_sf("gadm41_CMR_3.json")

head(admin3)

a3 <- ggplot(admin3) + 
  geom_sf(color = "white", aes(fill = NAME_3)) + 
  geom_sf_text(aes(label = NAME_3), size=1) +
  theme(legend.position = "none")

a3

#ggsave(generic_plot_path("cm_admin3.svg"), plot = a3, width = 6, height = 4, units = "in")
ggsave(generic_plot_path("cm_admin3.svg"), plot = a3)

#interactive plot
ggplotly(a3)

#save as svg
#ggsave("cm_admin_3.svg", plot = a3, width = 40, height = 40)
