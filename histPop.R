library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(animation)
library(scales)

popData <- read_csv("pop/censusPop1926_2016.csv")

national <- tidyPopData <- read_csv("pop/censusPop1926_2016.csv") %>% 
    gather(year, population, 2:18) %>%
    group_by(year) %>% 
    summarise(natPop = sum(population))
    
tidyPopData <- read_csv("pop/censusPop1926_2016.csv") %>% 
    gather(year, population, 2:18) %>%
    mutate(county = ifelse(county %in% c("North Tipperary", "South Tipperary"), 
                           "Tipperary",
                           county),
           county = as.factor(county),
           year = as.factor(year)) %>% 
    group_by(county, year) %>% 
    summarise(population = sum(population)) %>% 
    left_join(national, by = "year") %>% 
    mutate(popProp = population / natPop)

countyColours <- read_csv("pop/countyColours.csv")

fill <- setNames(as.character(countyColours$fill), countyColours$county)
line <- setNames(as.character(countyColours$line), countyColours$county)

years <- unique(tidyPopData$year)

saveHTML({
    for (i in years) 
    {
        p <- tidyPopData %>% 
            filter(year == i) %>% 
            transform(county = reorder(county, -popProp)) %>% 
            ggplot(aes(county, popProp, fill = county, colour = county, frame = year)) +
            geom_bar(stat = "identity", size = 0.2) +
            scale_fill_manual(name = "county", values = fill) +
            scale_colour_manual(name = "county", values = line) +
            scale_y_continuous(limits = c(0, 0.3), labels = percent) +
            labs(x = "", y = "Proportion of National Population") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                  legend.position="none",
                  panel.background = element_blank(),
                  panel.grid = element_blank(),
                  plot.title = element_text(size = 20)) +
            ggtitle(i) 
        print(p)
        
    }},
    
    img.name = "anim_plot", imgdir = "anim_dir", htmlfile = "anim.html", 
    autobrowse = FALSE, title = "County Population Changes", verbose = FALSE, 
    interval = 1, ani.width = 800, ani.height = 600
)

graphics.off()
    
