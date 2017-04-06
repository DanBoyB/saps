library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(purrr)
library(sf)


themes <- read_csv("2011/Theme_breakdown.csv")

# Create sf dataframe of small areas shapefile, calculate polygon areas and popultation densities
smallAreas <- st_read("2011/Census2011_Small_Areas_generalised20m.shp", stringsAsFactors = FALSE) %>% 
    mutate(area = st_area(geometry),
           popDensity = Total2011 / (as.numeric(area) * 1e-6))

# use dev version of ggplot2 to plot sf features
ggplot() + 
    geom_sf(data = smallAreas, 
            aes(fill = (cut_number(popDensity, 5))), 
            colour = NA) +
    scale_fill_brewer("Persons per sq. km", palette = "YlGnBu") +
    ggtitle("Population Density 2011") +                      
    theme(line = element_blank(),                             
          axis.text=element_blank(),                          
          axis.title=element_blank(),                         
          panel.background = element_blank())

smallAreaData <- read_csv("2011/AllThemesTablesSA.csv") %>% 
  gather(theme, value, 4:767) %>% 
  left_join(themes, by = "theme") %>% 
  rename(smallArea = GEOGDESC) %>% 
  arrange(smallArea) %>% 
  select(smallArea, theme, desc, value)

pop2011 <- smallAreaData %>% 
  filter(theme == "T1_1AGETT")



smallAreaData %>% filter(theme == "T1_1AGETT") %>% arrange(desc(value))

