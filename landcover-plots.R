
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------- LAND COVER PLOTS FOR SCHOOL GROUND STORY MAP------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Author: Sam Muir
# Last Updated: 10/09/2024


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                Load Packages                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(ggrepel)
library(sf)
library(patchwork)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          Read in and Define Data                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

threshold <- 10  # labels for pie chart slices with percentage less than 10% will go outside

# This data is from Eric's analysis of the school districts. This is over all of the schools in each district and includes the parking lots.
# Keeping this code, but not currently being used in the story map.

LA_landcover_area <- data.frame(
  landcover = c("Trees/Shrubs", "Grass", "Non-Building Impervious", "Buildings"),
  area_m2 = c(2233127.24, 537921.55, 69808855.85, 2054829.19),
  area_perc = c(18.91, 4.56, 59.13, 17.40)) %>%
  mutate(city = "Los Angeles") %>%
  mutate(label = paste0(landcover, "\n", round(area_perc, 1), "%")) %>%
  arrange(desc(landcover)) %>%
  mutate(pos = cumsum(area_perc) - area_perc / 2) %>%
  mutate(label_outside = ifelse(area_perc < threshold, TRUE, FALSE))

oakland_landcover_area <- data.frame(
  landcover = c("Trees/Shrubs", "Grass", "Non-Building Impervious", "Buildings"),
  area_m2 = c(191910.03, 110109.34, 496490.22, 170300.11),
  area_perc = c(19.81, 11.37, 51.25, 17.58)) %>%
  mutate(city = "Oakland") %>%
  mutate(label = paste0(landcover, "\n", round(area_perc, 1), "%")) %>%
  arrange(desc(landcover)) %>%
  mutate(pos = cumsum(area_perc) - area_perc / 2) %>%
  mutate(label_outside = ifelse(area_perc < threshold, TRUE, FALSE))

sac_landcover_area <- data.frame(
  landcover = c("Trees/Shrubs", "Grass", "Non-Building Impervious", "Buildings"),
  area_m2 = c(396741.08, 501352.93, 756905.12, 88384.65),
  area_perc = c(22.76, 28.76, 43.42, 5.07)) %>%
  mutate(city = "Sacramento") %>%
  mutate(label = paste0(landcover, "\n", round(area_perc, 1), "%")) %>%
  arrange(desc(landcover)) %>%
  mutate(pos = cumsum(area_perc) - area_perc / 2) %>%
  mutate(label_outside = ifelse(area_perc < threshold, TRUE, FALSE))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Pie Charts - Full School Boundary                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ All Cities Together  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

landcover_area <- bind_rows(LA_landcover_area, oakland_landcover_area, sac_landcover_area) %>%
  mutate(label = paste0(landcover, "\n", round(area_perc, 1),"%")) %>%
  group_by(city) %>%
  arrange(desc(landcover)) %>%
  mutate(pos = cumsum(area_perc) - area_perc / 2,
         label_outside = ifelse(area_perc < threshold, TRUE, FALSE))

pie <- ggplot(landcover_area, aes(x = "", y = area_perc, fill = landcover)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  facet_wrap(~city) +
  coord_polar("y", start = 0) +
  labs(x = "Landcover", y = "Percentage of Area") +
  theme_void() +
  scale_fill_manual(values = c("#b670b8", "#a8f0b3", "#d6d6d6", "#297549")) +
  geom_text(data = subset(landcover_area, !label_outside), 
            aes(y = pos, label = label), 
            color = "black", 
            size = 2.5) +
  geom_text_repel(data = subset(landcover_area, label_outside),
                  aes(y = pos, label = label), 
                  nudge_x = 0.8,             # Push the label outside the chart
                  nudge_y = 0,
                  show.legend = FALSE, 
                  size = 2.5, 
                  direction = "y", 
                  point.padding = 0, 
                  box.padding = 0, 
                  segment.size = 0.3) +
  theme(legend.position = "none",
        panel.background = element_rect(fill='transparent', color = NA), #transparent panel bg
        plot.background = element_rect(fill='transparent', color = NA))
pie

ggsave('plots/all_cities_lc_piecharts.png', pie, bg='transparent', width=16, height=4, units='in')

##~~~~~~~~~~~~~~~~~~~~~
##  ~ Los Angeles  ----
##~~~~~~~~~~~~~~~~~~~~~
la_pie <- ggplot(LA_landcover_area, aes(x = "", y = area_perc, fill = landcover)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#b670b8", "#a8f0b3", "#d6d6d6", "#297549")) +
  geom_text(data = subset(LA_landcover_area, !label_outside), 
            aes(y = pos, label = label), 
            color = "black", 
            size = 5) +
  geom_text_repel(data = subset(LA_landcover_area, label_outside),
                  aes(y = pos, label = label), 
                  nudge_x = 0.8,             # Push the label outside the chart
                  nudge_y = 0,
                  show.legend = FALSE, 
                  size = 5, 
                  direction = "y", 
                  point.padding = 0, 
                  box.padding = 0, 
                  segment.size = 0.3) +
  theme(legend.position = "none",
        panel.background = element_rect(fill='transparent', color = 'transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color = 'transparent'))
la_pie

# ggsave('~/Documents/git/USFS/plots/la_piechart.png', la_pie, bg='transparent', width=10, height=10, units='in')

##~~~~~~~~~~~~~~~~~
##  ~ Oakland  ----
##~~~~~~~~~~~~~~~~~
oak_pie <- ggplot(oakland_landcover_area, aes(x = "", y = area_perc, fill = landcover)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#b670b8", "#a8f0b3", "#d6d6d6", "#297549")) +
  geom_text(data = subset(oakland_landcover_area, !label_outside), 
            aes(y = pos, label = label), 
            color = "black", 
            size = 5) +
  geom_text_repel(data = subset(oakland_landcover_area, label_outside),
                  aes(y = pos, label = label), 
                  nudge_x = 0.8,             # Push the label outside the chart
                  nudge_y = 0,
                  show.legend = FALSE, 
                  size = 5, 
                  direction = "y", 
                  point.padding = 0, 
                  box.padding = 0, 
                  segment.size = 0.3) +
  theme(legend.position = "none",
        panel.background = element_rect(fill='transparent', color = 'transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color = 'transparent'))
oak_pie

# ggsave('~/Documents/git/USFS/plots/oak_piechart.png', oak_pie, bg='transparent', width=10, height=10, units='in')

##~~~~~~~~~~~~~~~~~~~~
##  ~ Sacramento  ----
##~~~~~~~~~~~~~~~~~~~~
sac_pie <- ggplot(sac_landcover_area, aes(x = "", y = area_perc, fill = landcover)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#b670b8", "#a8f0b3", "#d6d6d6", "#297549")) +
  geom_text(data = subset(sac_landcover_area, !label_outside), 
            aes(y = pos, label = label), 
            color = "black", 
            size = 5) +
  geom_text_repel(data = subset(sac_landcover_area, label_outside),
                  aes(y = pos, label = label), 
                  nudge_x = 0.8,             # Push the label outside the chart
                  nudge_y = 0,
                  show.legend = FALSE, 
                  size = 5, 
                  direction = "y", 
                  point.padding = 0, 
                  box.padding = 0, 
                  segment.size = 0.3) +
  theme(legend.position = "none",
        panel.background = element_rect(fill='transparent', color = 'transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color = 'transparent'))
sac_pie

# ggsave('~/Documents/git/USFS/plots/sac_piechart.png', sac_pie, bg='transparent', width=10, height=10, units='in')


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                     Pie Charts - Parking Lot and Non-Parking Lots        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Data Read in and Prep  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This data for all of the schools in each city
# Parking is just the parking lots, non-parking is everything else in the school boundary that is not a parking lot

LA_class_parking <- read_sf("data/LA_classification_parkinglot_polygons/LA_Classification_ParkingLot_Clip.shp") %>%
  st_drop_geometry() %>%
  janitor::clean_names() %>%
  mutate(lc_class = case_when(
    dn == "1" ~ "Trees/Shrubs",
    dn == "2" ~ "Grass",
    dn == "3" ~ "Non-Building Impervious",
    dn == "4" ~ "Buildings")) %>%
  group_by(lc_class) %>%
  summarise(area_km2_sum = sum(area_km2)) %>%
  drop_na() %>%
  mutate(area_perc = area_km2_sum / sum(area_km2_sum) * 100) %>%
  mutate(label = paste0(lc_class, "\n", round(area_perc, 1), "%")) %>%
  arrange(desc(lc_class)) %>%
  mutate(pos = cumsum(area_perc) - area_perc / 2) %>%
  mutate(label_outside = ifelse(area_perc < threshold, TRUE, FALSE),
         parking = "Parking Lot")

LA_class_nonparking <- read_sf("data/LA_classification_parkinglot_polygons/LA_Classification_NonParkingLot_Clip.shp") %>%
  st_drop_geometry() %>%
  janitor::clean_names() %>%
  mutate(lc_class = case_when(
    dn == "1" ~ "Trees/Shrubs",
    dn == "2" ~ "Grass",
    dn == "3" ~ "Non-Building Impervious",
    dn == "4" ~ "Buildings")) %>%
  group_by(lc_class) %>%
  rename(area_km2 = area) %>%
  summarise(area_km2_sum = sum(area_km2)) %>%
  drop_na() %>%
  mutate(area_perc = area_km2_sum / sum(area_km2_sum) * 100) %>%
  mutate(label = paste0(lc_class, "\n", round(area_perc, 1), "%")) %>%
  arrange(desc(lc_class)) %>%
  mutate(pos = cumsum(area_perc) - area_perc / 2) %>%
  mutate(label_outside = ifelse(area_perc < threshold, TRUE, FALSE),
         parking = "Non-Parking Lot")

LA_class_parking_join <- LA_class_parking %>%
  bind_rows(LA_class_nonparking) %>%
  filter(parking == "Non-Parking Lot") # filter since we decided to look at only non-parking lot areas
# to look at parking and non-parking lot pie charts together, remove this filter line and un-comment the facet_wrap() line in ggplot


oak_class_parking <- sf::read_sf("data/Oakland_classification_parkinglot_polygons/Oakland_Classification_ParkingLots_Clip.shp") %>%
  st_drop_geometry() %>%
  janitor::clean_names() %>%
  mutate(lc_class = case_when(
    dn == "1" ~ "Trees/Shrubs",
    dn == "2" ~ "Grass",
    dn == "3" ~ "Non-Building Impervious",
    dn == "4" ~ "Buildings")) %>%
  group_by(lc_class) %>%
  summarise(area_km2_sum = sum(area_km2)) %>%
  mutate(area_perc = area_km2_sum / sum(area_km2_sum) * 100) %>%
  mutate(label = paste0(lc_class, "\n", round(area_perc, 1), "%")) %>%
  arrange(desc(lc_class)) %>%
  mutate(pos = cumsum(area_perc) - area_perc / 2) %>%
  mutate(label_outside = ifelse(area_perc < 11, TRUE, FALSE),
         parking = "Parking Lot")

oak_class_nonparking <- read_sf("data/Oakland_classification_parkinglot_polygons/Oakland_Classification_NonParkingLots_Clip.shp") %>%
  st_drop_geometry() %>%
  janitor::clean_names() %>%
  mutate(lc_class = case_when(
    dn == "1" ~ "Trees/Shrubs",
    dn == "2" ~ "Grass",
    dn == "3" ~ "Non-Building Impervious",
    dn == "4" ~ "Buildings")) %>%
  group_by(lc_class) %>%
  rename(area_km2 = area) %>%
  summarise(area_km2_sum = sum(area_km2)) %>%
  drop_na() %>%
  mutate(area_perc = area_km2_sum / sum(area_km2_sum) * 100) %>%
  mutate(label = paste0(lc_class, "\n", round(area_perc, 1), "%")) %>%
  arrange(desc(lc_class)) %>%
  mutate(pos = cumsum(area_perc) - area_perc / 2) %>%
  mutate(label_outside = ifelse(area_perc < threshold, TRUE, FALSE),
         parking = "Non-Parking Lot")

oak_class_parking_join <- oak_class_parking %>%
  bind_rows(oak_class_nonparking) %>%
  filter(parking == "Non-Parking Lot")

sac_class_parking <- sf::read_sf("data/Sacramento_classification_parkinglot_polygons/Sacramento_Classification_ParkingLots_Clip.shp") %>%
  st_drop_geometry() %>%
  janitor::clean_names() %>%
  mutate(lc_class = case_when(
    dn == "1" ~ "Trees/Shrubs",
    dn == "2" ~ "Grass",
    dn == "3" ~ "Non-Building Impervious",
    dn == "4" ~ "Buildings")) %>%
  group_by(lc_class) %>%
  summarise(area_km2_sum = sum(area_km2)) %>%
  mutate(area_perc = area_km2_sum / sum(area_km2_sum) * 100) %>%
  mutate(label = paste0(lc_class, "\n", round(area_perc, 1), "%")) %>%
  arrange(desc(lc_class)) %>%
  mutate(pos = cumsum(area_perc) - area_perc / 2) %>%
  mutate(label_outside = ifelse(area_perc < 12, TRUE, FALSE),
         parking = "Parking Lot")

sac_class_nonparking <- read_sf("data/Sacramento_classification_parkinglot_polygons/Sacramento_Classification_NonParkingLot_Clip.shp") %>%
  st_drop_geometry() %>%
  janitor::clean_names() %>%
  mutate(lc_class = case_when(
    dn == "1" ~ "Trees/Shrubs",
    dn == "2" ~ "Grass",
    dn == "3" ~ "Non-Building Impervious",
    dn == "4" ~ "Buildings")) %>%
  group_by(lc_class) %>%
  rename(area_km2 = area) %>%
  summarise(area_km2_sum = sum(area_km2)) %>%
  drop_na() %>%
  mutate(area_perc = area_km2_sum / sum(area_km2_sum) * 100) %>%
  mutate(label = paste0(lc_class, "\n", round(area_perc, 1), "%")) %>%
  arrange(desc(lc_class)) %>%
  mutate(pos = cumsum(area_perc) - area_perc / 2) %>%
  mutate(label_outside = ifelse(area_perc < threshold, TRUE, FALSE),
         parking = "Non-Parking Lot")

sac_class_parking_join <- sac_class_parking %>%
  bind_rows(sac_class_nonparking) %>%
  filter(parking == "Non-Parking Lot")
  
##~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Make Pie Charts  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~
##  ~ Los Angeles  ----
##~~~~~~~~~~~~~~~~~~~~~
la_pie_park <- ggplot(LA_class_parking_join, aes(x = "", y = area_perc, fill = lc_class)) +
  geom_bar(stat = "identity", width = 2, color = "white") +
 # facet_wrap(~parking, strip.position = "bottom") + 
  coord_polar("y", start = 0) + # turn axis into circle - makes it a pie chart
  theme_void() +
  scale_fill_manual(values = c("#b670b8", "#a8f0b3", "#d6d6d6", "#297549")) +
  geom_text(data = subset(LA_class_parking_join, !label_outside), # add text labels in slices, only ones that fit inside
            aes(y = pos, label = label), 
            color = "black", 
            size = 6.5) + # text size
  geom_text_repel(data = subset(LA_class_parking_join, label_outside), # add text labels outside slices
                  aes(y = pos, label = label), 
                  nudge_x = 1.5, # how far to push the label outside the chart
                  nudge_y = 0,
                  show.legend = FALSE, 
                  size = 6.5, # text size
                  direction = "y", 
                  point.padding = 0.5, 
                  box.padding = 0.5, 
                  segment.size = 0.3) +
  labs(title = "Los Angeles") +
  theme(legend.position = "none",
        panel.background = element_rect(fill='transparent', color = 'transparent'), # transparent panel bg
        plot.background = element_rect(fill='transparent', color = 'transparent'), # transparent plot bg
        strip.text = element_text(size = 16, vjust = 20), # can keep, but not doing anything without facet_wrap()
        strip.clip = "off", # can keep, but not doing anything without facet_wrap()
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        panel.spacing = unit(-8, "lines")) # can keep, but not doing anything without facet_wrap()
la_pie_park

ggsave('plots/la_parking_piechart.png', la_pie_park, bg='transparent', width=15, height=10, units='in')

##~~~~~~~~~~~~~~~~~
##  ~ Oakland  ----
##~~~~~~~~~~~~~~~~~
oak_pie_park <- ggplot(oak_class_parking_join, aes(x = "", y = area_perc, fill = lc_class)) +
  geom_bar(stat = "identity", width = 2, color = "white") +
 # facet_wrap(~parking, strip.position = "bottom") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#b670b8", "#a8f0b3", "#d6d6d6", "#297549")) +
  geom_text(data = subset(oak_class_parking_join, !label_outside), 
            aes(y = pos, label = label), 
            color = "black", 
            size = 6.5) +
  geom_text_repel(data = subset(oak_class_parking_join, label_outside),
                  aes(y = pos, label = label), 
                  nudge_x = 1.5,             # Push the label outside the chart
                  nudge_y = 0,
                  show.legend = FALSE, 
                  size = 6.5, 
                  direction = "y", 
                  point.padding = 0.5, 
                  box.padding = 0.5, 
                  segment.size = 0.3) +
  labs(title = "Oakland") +
  theme(legend.position = "none",
        panel.background = element_rect(fill='transparent', color = 'transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color = 'transparent'),
        strip.text = element_text(size = 16, vjust = 20),
        strip.clip = "off",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        panel.spacing = unit(-12, "lines"))
oak_pie_park

ggsave('plots/oak_parking_piechart.png', oak_pie_park, bg='transparent', width=15, height=10, units='in')


##~~~~~~~~~~~~~~~~~~~~
##  ~ Sacramento  ----
##~~~~~~~~~~~~~~~~~~~~
sac_pie_park <- ggplot(sac_class_parking_join, aes(x = "", y = area_perc, fill = lc_class)) +
  geom_bar(stat = "identity", width = 2, color = "white") +
 # facet_wrap(~parking, strip.position = "bottom") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#b670b8", "#a8f0b3", "#d6d6d6", "#297549")) +
  geom_text(data = subset(sac_class_parking_join, !label_outside), 
            aes(y = pos, label = label), 
            color = "black", 
            size = 5.5) +
  geom_text_repel(data = subset(sac_class_parking_join, label_outside),
                  aes(y = pos, label = label), 
                  nudge_x = 1.5,             # Push the label outside the chart
                  nudge_y = 0,
                  show.legend = FALSE, 
                  size = 5.5, 
                  direction = "y", 
                  point.padding = 0.5, 
                  box.padding = 0.5, 
                  segment.size = 0.3) +
  labs(title = "Sacramento") +
  theme(legend.position = "none",
        panel.background = element_rect(fill='transparent', color = 'transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color = 'transparent'),
        strip.text = element_text(size = 16, vjust = 20),
        strip.clip = "off",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        panel.spacing = unit(-10, "lines"))
sac_pie_park

ggsave('plots/sac_parking_piechart.png', sac_pie_park, bg='transparent', width=15, height=10, units='in')


# if you want to see them all side by side
la_pie_park + oak_pie_park + sac_pie_park 


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        Individual School Pie Charts                      ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This data is just one school in each city and *excludes* the parking lots

##~~~~~~~~~~~~~~~~~~~
##  ~ Prep Data  ----
##~~~~~~~~~~~~~~~~~~~
LA_landcover_122nd <- read_sf("data/LA_Classification_NonParkingLot_Clip/LA_Classification_NonParkingLot_Clip.shp") %>%
  janitor::clean_names() %>%
  st_drop_geometry() %>%
  group_by(dn) %>%
  summarise(area_sum = sum(area), .groups = 'drop') %>%
  mutate(percent_area = area_sum / sum(area_sum) * 100) %>%
  mutate(lulc = case_when(dn == 3 ~ "Non-Building Impervious",
                              dn == 2 ~ "Grass",
                              dn == 1 ~ "Trees/Shrubs",
                              dn == 4 ~ "Buildings"),
         label = paste0(lulc, "\n", round(percent_area, 1), "%")) %>%
  arrange(desc(lulc)) %>%
  mutate(pos = cumsum(percent_area) - percent_area / 2) %>%
  mutate(label_outside = ifelse(percent_area < threshold, TRUE, FALSE))

oak_landcover_MLKJr <- read_sf("data/Oakland_Classification_NonParkingLots_Clip/Oakland_Classification_NonParkingLots_Clip.shp") %>%
  janitor::clean_names() %>%
  st_drop_geometry() %>%
  group_by(dn) %>%
  summarise(area_sum = sum(area), .groups = 'drop') %>%
  mutate(percent_area = area_sum / sum(area_sum) * 100) %>%
  mutate(lulc = case_when(dn == 3 ~ "Non-Building Impervious",
                          dn == 2 ~ "Grass",
                          dn == 1 ~ "Trees/Shrubs",
                          dn == 4 ~ "Buildings"),
         label = paste0(lulc, "\n", round(percent_area, 1), "%")) %>%
  arrange(desc(lulc)) %>%
  mutate(pos = cumsum(percent_area) - percent_area / 2) %>%
  mutate(label_outside = ifelse(percent_area < threshold, TRUE, FALSE))

sac_landcover_Camellia <- read_sf("data/Sacramento_Classification_NonParkingLot_Clip/Sacramento_Classification_NonParkingLot_Clip.shp") %>%
  janitor::clean_names() %>%
  st_drop_geometry() %>%
  group_by(dn) %>%
  summarise(area_sum = sum(area), .groups = 'drop') %>%
  mutate(percent_area = area_sum / sum(area_sum) * 100) %>%
  mutate(lulc = case_when(dn == 3 ~ "Non-Building Impervious",
                          dn == 2 ~ "Grass",
                          dn == 1 ~ "Trees/Shrubs",
                          dn == 4 ~ "Buildings"),
         label = paste0(lulc, "\n", round(percent_area, 1), "%")) %>%
  arrange(desc(lulc)) %>%
  mutate(pos = cumsum(percent_area) - percent_area / 2) %>%
  mutate(label_outside = ifelse(percent_area < threshold, TRUE, FALSE))

##~~~~~~~~~~~~~~~~~~~~
##  ~ Pie Charts  ----
##~~~~~~~~~~~~~~~~~~~~
LA_122nd_pie <- ggplot(LA_landcover_122nd, aes(x = "", y = percent_area, fill = lulc)) +
  geom_bar(stat = "identity", width = 2, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#b670b8", "#a8f0b3", "#d6d6d6", "#297549")) +
  geom_text(data = subset(LA_landcover_122nd, !label_outside), 
            aes(y = pos, label = label), 
            color = "black", 
            size = 6) +
  geom_text_repel(data = subset(LA_landcover_122nd, label_outside),
                  aes(y = pos, label = label), 
                  nudge_x = 1.5,             # Push the label outside the chart
                  nudge_y = 0,
                  show.legend = FALSE, 
                  size = 6, 
                  direction = "y", 
                  point.padding = 0.5, 
                  box.padding = 0.5, 
                  segment.size = 0.3) +
  theme(legend.position = "none",
        panel.background = element_rect(fill='transparent', color = 'transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color = 'transparent'),
        strip.text = element_text(size = 16, vjust = 20),
        strip.clip = "off",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
LA_122nd_pie

ggsave('plots/122nd_piechart.png', LA_122nd_pie, bg='transparent', width=10, height=10, units='in')


oak_MLKJr_pie <- ggplot(oak_landcover_MLKJr, aes(x = "", y = percent_area, fill = lulc)) +
  geom_bar(stat = "identity", width = 2, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#b670b8", "#a8f0b3", "#d6d6d6", "#297549")) +
  geom_text(data = subset(oak_landcover_MLKJr, !label_outside), 
            aes(y = pos, label = label), 
            color = "black", 
            size = 6) +
  geom_text_repel(data = subset(oak_landcover_MLKJr, label_outside),
                  aes(y = pos, label = label), 
                  nudge_x = 1.5,             # Push the label outside the chart
                  nudge_y = 0,
                  show.legend = FALSE, 
                  size = 6, 
                  direction = "y", 
                  point.padding = 0.5, 
                  box.padding = 0.5, 
                  segment.size = 0.3) +
  theme(legend.position = "none",
        panel.background = element_rect(fill='transparent', color = 'transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color = 'transparent'),
        strip.text = element_text(size = 16, vjust = 20),
        strip.clip = "off",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
oak_MLKJr_pie

ggsave('plots/MLKJr_piechart.png', oak_MLKJr_pie, bg='transparent', width=10, height=10, units='in')


sac_camellia_pie <- ggplot(sac_landcover_Camellia, aes(x = "", y = percent_area, fill = lulc)) +
  geom_bar(stat = "identity", width = 2, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#b670b8", "#a8f0b3", "#d6d6d6", "#297549")) +
  geom_text(data = subset(sac_landcover_Camellia, !label_outside), 
            aes(y = pos, label = label), 
            color = "black", 
            size = 6) +
  geom_text_repel(data = subset(sac_landcover_Camellia, label_outside),
                  aes(y = pos, label = label), 
                  nudge_x = 1.5,             # Push the label outside the chart
                  nudge_y = 0,
                  show.legend = FALSE, 
                  size = 6, 
                  direction = "y", 
                  point.padding = 0.5, 
                  box.padding = 0.5, 
                  segment.size = 0.3) +
  theme(legend.position = "none",
        panel.background = element_rect(fill='transparent', color = 'transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color = 'transparent'),
        strip.text = element_text(size = 16, vjust = 20),
        strip.clip = "off",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
sac_camellia_pie

ggsave('plots/camellia_piechart.png', sac_camellia_pie, bg='transparent', width=10, height=10, units='in')


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  Box Plots                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This data is for all schools in each city, joined so that school name is included
# This data also *excludes* the parking lots

##~~~~~~~~~~~~~~~~~~~~~~
##  ~ Read in Data  ----
##~~~~~~~~~~~~~~~~~~~~~~
LA_landcover_area_full <- read_sf("data/LA_District_Classification_NonParking_Join/LA_District_Classification_NonParking_Join.shp") %>%
  janitor::clean_names() %>%
  st_drop_geometry() %>%
  group_by(school, dn) %>%
  summarise(area_km2 = sum(area_km2)) %>%
  mutate(area_perc = area_km2 / sum(area_km2) * 100) %>%
  mutate(city = "Los Angeles")

oak_landcover_area_full <- read_sf("data/Oakland_District_Classification_NonParking_Join/Oakland_District_Classification_NonParking_join.shp") %>%
  janitor::clean_names() %>%
  st_drop_geometry() %>%
  group_by(school, dn) %>%
  summarise(area_km2 = sum(area_km2)) %>%
  mutate(area_perc = area_km2 / sum(area_km2) * 100) %>%
  mutate(city = "Oakland")

sac_landcover_area_full <- read_sf("data/Sacramento_District_Classification_NonParking_Join/Sacramento_District_Classification_NonParking_Join.shp") %>%
  janitor::clean_names() %>%
  st_drop_geometry() %>%
  group_by(school, dn) %>%
  summarise(area_km2 = sum(area_km2)) %>%
  mutate(area_perc = area_km2 / sum(area_km2) * 100) %>%
  mutate(city = "Sacramento")

# combine data
landcover_area_full <- bind_rows(LA_landcover_area_full, oak_landcover_area_full, sac_landcover_area_full) %>%
  group_by(city) %>%
  mutate(lulc = case_when(dn == 3 ~ "Non-Building Impervious",
                          dn == 2 ~ "Grass",
                          dn == 1 ~ "Trees/Shrubs",
                          dn == 4 ~ "Buildings"))

##~~~~~~~~~~~~~~~~~~~~~~
##  ~ Create Plot   ----
##~~~~~~~~~~~~~~~~~~~~~~
impervious_boxplot <- landcover_area_full %>%
  filter(lulc == "Non-Building Impervious") %>%
  ggplot(., aes(x = area_perc, y = lulc)) +
  geom_jitter(alpha = 0.4, fill = "#d6d6d6", size = 0.5) +
  geom_boxplot(fill = NA, size = 1) +
  facet_wrap(~city, ncol = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  labs(x = "Area (%)", title = "Non-Building Impervious Cover Area on School Grounds") +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        strip.text = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2),
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color = NA),
        rect = element_rect(fill = "transparent"))
impervious_boxplot
  
ggsave('plots/impervious_boxplots.png', impervious_boxplot, bg='transparent', width=16, height=4, units='in')

  
  