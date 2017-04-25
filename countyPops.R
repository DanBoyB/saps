library(tidyverse)
library(stringr)
library(sf)
library(animation)

Dublin <- c("Dublin City", "Fingal", "South Dublin", "DU+00FAn Laoghaire-Rathdown", "D�n Laoghaire-Rathdown")
Tipperary <- c("North Tipperary", "South Tipperary")

countyPop <- read_csv("output/Population at Each Census from 1841 to 2016 by  County, Sex and CensusYear.csv") %>% 
    filter(Sex == "Both sexes", County != "State") %>% 
    select(County, `Census Year`, value) %>% 
    rename(population = value, censusYear = `Census Year`)

countyShape <- st_read("2011/Census2011_Admin_Counties_generalised20m.shp", stringsAsFactors = FALSE) %>% 
    rename(County = COUNTYNAME, areaKm = TOTAL_AREA) %>% 
    mutate(County = str_replace_all(County, " County", ""),
           County = str_replace_all(County, " City", ""),
           County = ifelse(County %in% Dublin, "Dublin", County),
           County = ifelse(County %in% Tipperary, "Tipperary", County)) %>% 
    left_join(countyPop, by = "County") %>% 
    group_by(County, censusYear) %>% 
    summarise(population = sum(population),
              areaKm = sum(areaKm)) %>% 
    mutate(popDensity = population / areaKm,
           popProp = population / sum(population))

years <- unique(countyShape$censusYear)

saveHTML({
    for (i in years) 
    {
        p <- countyShape %>% 
            filter(censusYear == i) %>% 
            ggplot() +
            geom_sf(aes(fill = popProp), 
                    colour = NA) +
            scale_fill_gradient(low = "light blue", high = "dark blue", limit = c(0, 0.2)) +
            ggtitle("Population Density 2011") +                      
            theme(line = element_blank(),                             
                  axis.text=element_blank(),                          
                  axis.title=element_blank(),                         
                  panel.background = element_blank()) +
            ggtitle(i)
        print(p)
        
    }},
    
    img.name = "anim_plot2", imgdir = "anim_dir", htmlfile = "anim2.html", 
    autobrowse = FALSE, title = "County Population Changes", verbose = FALSE, 
    interval = 1, ani.width = 800, ani.height = 600
)

graphics.off()

