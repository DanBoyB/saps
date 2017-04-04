library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(sf)

themes <- read_csv("2011/Theme_breakdown.csv")

smallAreaShape <- st_read("2011/Census2011_Small_Areas_generalised20m.shp", stringsAsFactors = FALSE)

smallAreaData <- read_csv("2011/AllThemesTablesSA.csv") %>% 
  gather(theme, value, 4:767) %>% 
  left_join(themes, by = "theme") %>% 
  rename(smallArea = GEOGDESC) %>% 
  arrange(smallArea) %>% 
  select(smallArea, theme, desc, value)

pop2011 <- smallAreaData %>% 
  filter(theme == "T1_1AGETT")

ggplot(smallAreaShape) +
  geom_sf(aes(fill = Total2011)) +
  scale_fill_viridis("Total2011") +
  theme_bw()