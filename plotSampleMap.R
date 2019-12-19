# Plot map of samples from each country
# Margaret Antonio

library(cowplot)
library(viridis)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(tidyr)

# Load the map
# Map object will have long, lat, region.
# At the world level region = country
world = map_data("world")

# Create random study data

study_regions = c("Mexico", "Brazil", "Algeria", "South Africa",
                  "Peru", "Spain",
                  "Finland", "India", "Japan")

getRandomData <- function(n = 200){
  rc <- study_regions[runif(n,1,length(study_regions) + 1)]
  rs <- paste0("Sample", seq(1,n,by = 1))
  ra <- rep(c("urban", "rural"), times = n)
  # can add other random characteristics
  return(data.frame(sample = rs, country = rc, age = ra))
}

# Data from https://developers.google.com/public-data/docs/canonical/countries_csv
country_dat <- read.csv("~/Documents/dylan/map/countries.csv",
                        header = TRUE, stringsAsFactors = F)

study_dat <- getRandomData(n = 100) %>%
  # Summarize by country, age, whatever
  group_by(country) %>%
  summarize(N = n()) %>%
  ungroup() %>%
  left_join(country_dat %>%
              select(-country), 
            by = c("country" = "name"))

# labels for all regions (countries) if want to label them on map
region.lab.data <- world %>%
  group_by(region) %>%
  dplyr::summarise(long = mean(long), lat = mean(lat)) %>%
  filter(!region %in% study_regions)


# Plot
ggplot() +
  # Create a polygon for every country
  geom_polygon(data = world,
               aes(x=long, y=lat, group=group),
               color = "white", fill = "lightgray") +
  # Fill country for study regions
  geom_polygon(data = world %>% 
                 filter(region %in% study_regions) %>%
                 left_join(study_dat, by = c("region" = "country")),
               aes(x = long, y = lat, group=group, fill = N),
               color = "darkgray") +
  # Create a point for each study region
  #geom_point(data = study_dat, 
  #           aes(x = longitude, y = latitude, size = log(adult+infant)),
  #           alpha = 0.8, color = "white") +
  # Create label for the study regions
  geom_label_repel(data = study_dat, 
                   aes(x = longitude, y = latitude, 
                       label = paste0(country, " (", N,")")),
                   fontface = "bold",
                   size = 5, point.padding = 0.001, box.padding = 1) +
  # All regions labels - messy
  #geom_text(data = region.lab.data, 
  #          aes(x = long, y = lat, 
  #              label = region),
  #          color = "darkgray", 
  #          size = 3) +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.1,0.2),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  scale_fill_viridis()


