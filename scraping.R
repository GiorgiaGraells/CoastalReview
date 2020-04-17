#Extraccion de informacion web para mapa poblacion mundial
#Uso de SelctorGadget, ap junto a la barra de navegacion en chrome


library(rvest)
library(dplyr)
library(tidyverse)

# scrape html x ciudad
html <- read_html("https://worldpopulationreview.com/world-cities/")
pop2020 <- html_nodes(html, "td")  #.cpage tambien?


# Convert world population to text and view the summary data
pop2020_text <- html_text(pop2020) %>% str_remove_all(pattern = ",")  %>% str_remove_all(pattern = "%")
pop2020 <- as.data.frame(matrix(pop2020_text, ncol = 5, byrow = T)) %>% mutate_all(as.character) %>% mutate(V3= as.numeric(V3),V4= as.numeric(V4), V5 = as.numeric(V5)) %>% rename(Population2020=V3, Population2019=V4, Growth_rate = V5) %>% mutate(V2 = str_to_lower(as.character(V2)))

write_csv(pop2020, "Pop2020.csv")



#####################################
###########################################
#scrape por pais
html <- read_html("http://world.bymap.org/Population.html")
WorldPop <- html_nodes(html, "td")  
WorldPop_text <- html_text(WorldPop) %>% str_remove_all(pattern = ",")
world <- as.data.frame(matrix(WorldPop_text, ncol = 5, byrow = T)) %>% mutate_all(as.character) %>% mutate(V3 = as.numeric(V3)) %>% rename(Population = V3)
world <- world %>% rename(region=V2, value=Population) %>% mutate(region = str_to_lower(region))

write.csv(world, "WorldPop.csv")

###ver mapa
WorldPop <- read_csv("WorldPop.csv")
map1<- country_choropleth(WorldPop,  num_colors = 1)
map1


#verificacion de nombres de países

#En_World_no_en_country <- world$region[!(world$region %in% country.map$region)]
#En_Country_no_en_world <- country.map$region[!(country.map$region %in% world$region)]

#MAPA POBLACION MUNDIAL
library(choroplethr)
library(choroplethrMaps)
library(readr)

population <- read_csv("WorldPop.csv")
map<- country_choropleth(population,  num_colors = 1)
map


#Scraping por crecimiento paises
html <- read_html("https://www.worldometers.info/world-population/population-by-country/")
Worldgrow <- html_nodes(html, "td")  
Worldgrow_text <- html_text(Worldgrow) %>% str_remove_all(pattern = ",")%>% str_remove_all(pattern = "%")
Worldgrow <- as.data.frame(matrix(Worldgrow_text, ncol = 12, byrow = T)) %>% mutate_all(as.character) %>% mutate(V4 = as.numeric(V4)) %>% rename(yearly_change = V4)


WorldGrow <- Worldgrow %>% rename(region=V2, value=yearly_change) %>% mutate(region = str_to_lower(region))
write.csv(WorldGrow, "WorldGrow.csv")

growth <- read_csv("WorldGrow.csv")
map2<- country_choropleth(growth,  num_colors = 1)
map2

