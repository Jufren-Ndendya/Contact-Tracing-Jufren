rm(list = ls(all.names = TRUE))
setwd("C:/Users/JUZAN ENTERPRISES/Desktop/MAP Assessment")
# ============================
# Load libraries
# ============================
library(sf)
library(readr)
library(readxl)
library(ggplot2)
library(viridis)
# ============================
# Load data
# ============================
pfpr_with_sources <- read_csv("Data/pfpr_with_sources.csv")
pfpr_with_source  <- read_excel("Data/pfpr_with_sources.xlsx")
# Load shapefile (adjust path)

#shp<- st_read("/home/danielpetro/Dropbox/Desktop/RATemple_GRA/Juzan",
  #            layer = "261_Districts_matching2")
#shp<- st_read("C:/Users/JUZAN ENTERPRISES/Desktop/MAP Assessment",
  #            layer = "261_Districts_matching2")

shp<- st_read("Shapefile/261_Districts_matching2.shp")

# ============================
# Create sf points in WGS84
# ============================
points_sf <- st_as_sf(
  pfpr_with_source,
  coords = c("longitude", "latitude"),
  crs = 4326   # WGS84 (correct for lon/lat)
)
# ============================
# Transform points to match shapefile CRS
# ============================
points_sf <- st_transform(points_sf, st_crs(shp))
# ============================
# Check CRS (optional)
# ============================
print(st_crs(shp))
print(st_crs(points_sf))
# ============================
# Plot
# ============================
ggplot() +
  geom_sf(data = shp, fill = "gray90", color = "white", linewidth = 0.3) +
  geom_sf(data = points_sf, aes(color = pf_pr), alpha = 0.8, size = 0.7) +
  scale_color_viridis_c(option = "C") +
  facet_wrap(~year_end, ncol = 4)+
  theme_minimal() +
  labs(
    title = "Exploratory data analysis",
  )
color = "Prevalence"





