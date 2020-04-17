##Maps

library(tidyverse)
library(sf)
library(rworldxtra)
library(gridExtra)

######################################################
#poblacion por ciudad

pop2020 <- read_csv("Pop2020.csv")

pop2020 <- pop2020 %>%  
  mutate(V2= str_replace(V2, "port-au-prince", "port au prince"))%>%
  mutate(V2= str_replace(V2, "st. petersburg", "st petersburg"))%>%
  mutate(V2= str_replace(V2, "tel aviv-yafo", "tel aviv"))%>%
  mutate(V2= str_replace(V2, "washington, d.c.", "washington"))


##revisar nombres que coincidesn entre articulos final y pop2020 y no con citypopo

Articulos_final <- read_csv("Articulos_al2020.csv")%>% mutate(study_country = str_to_lower(study_country))%>% 
  mutate(study_city = str_to_lower(study_city))  %>% 
  mutate(study_country= str_replace(study_country, "usa", "United States of America"))%>%
  mutate(study_country= str_replace(study_country, "emirates", "United Arab Emirates"))%>%
  mutate(study_country= str_replace(study_country, "the netherlands", "Netherlands"))%>%
  mutate(study_country= str_replace(study_country, "uk", "United Kingdom")) %>% 
  mutate(study_city= str_replace(study_city, "limassol", "lemosos")) %>% 
  mutate(study_city= str_replace(study_city, "bruges", "brugge")) %>%
  mutate(study_city= str_replace(study_city, "tel aviv", "tel aviv-yafo")) %>%
  mutate(study_city= str_replace(study_city, "herzliya", "tel aviv-yafo")) %>%
  mutate(study_city= str_replace(study_city, "san sebastian", "santos")) %>%
  mutate(study_city= str_replace(study_city, "mehuin", "valdivia")) %>%
  mutate(study_city= str_replace(study_city, "tome", "talcahuano")) %>%
  mutate(study_city= str_replace(study_city, "falcon", "coro")) %>%
  mutate(study_city= str_replace(study_city, "tasmania", "hobart")) %>%
  mutate(study_city= str_replace(study_city, "syngapore", "singapore")) %>%
  mutate(study_country= str_replace(study_country, "malaysia", "Singapore")) %>% 
  mutate(study_city= str_replace(study_city, "hambourg", "hamburg")) %>%
  mutate(study_city= str_replace(study_city, "newport beach", "newport")) %>%
  mutate(study_city= str_replace(study_city, "washington", "washington, d.c.")) %>%
  mutate(study_city= str_replace(study_city, "castellon", "castello")) %>%
  mutate(study_city= str_replace(study_city, "sevilla", "seville")) %>%
  mutate(study_city= str_replace(study_city, "kwazulu-natal", "durban")) %>%
  mutate(study_city= str_replace(study_city, "yakarta", "yogyakarta")) %>%
  mutate(study_city= str_replace(study_city, "akko", "haifa")) %>%
  mutate(study_city= str_replace(study_city, "stone harbor", "atlantic city")) %>%
  mutate(study_city= str_replace(study_city, "jersey city", "new york")) %>%
  mutate(study_city= str_replace(study_city, "isle of palms", "charleston")) %>%
  mutate(study_city= str_replace(study_city, "milford", "new haven")) %>%
  mutate(study_city= str_replace(study_city, "hoboken", "new york")) %>%
  mutate(study_city= str_replace(study_city, "sussex", "brighton")) %>%
  mutate(study_city= str_replace(study_city, "kesennuma", "sendai")) %>%
  mutate(study_city= str_replace(study_city, "okkaido", "sapporo")) %>%
  mutate(study_city= str_replace(study_city, "tohoku", "sendai")) %>% 
  mutate(study_city= str_replace(study_city, "macao", "zhuhai")) %>% 
  mutate(study_city= str_replace(study_city, "mongkok", "hong kong")) %>% 
  mutate(study_city= str_replace(study_city, "brittany", "nantes")) %>% 
  mutate(study_city= str_replace(study_city, "mandelieu", "nice")) %>% 
  mutate(study_city= str_replace(study_city, "penestin", "nantes")) %>% 
  mutate(study_city= str_replace(study_city, "casabalate", "lecce")) %>% 
  mutate(study_city= str_replace(study_city, "otranto", "lecce")) %>% 
  mutate(study_city= str_replace(study_city, "senigallia", "ancona")) %>% 
  mutate(study_city= str_replace(study_city, "volturno", "naples")) %>% 
  mutate(study_city= str_replace(study_city, "senigallia", "ancona")) %>% 
  mutate(study_city= str_replace(study_city, "slupsk", "gdynia")) %>% 
  mutate(study_city= str_replace(study_city, "st petersboug", "st. petersburg")) %>% 
  mutate(study_city= str_replace(study_city, "mersin", "adana")) %>% 
  mutate(study_city= str_replace(study_city, "swansea city", "swansea")) %>% 
mutate(study_city= str_replace(study_city, "tywyn", "swansea")) 

Articulos_final <- Articulos_final %>% mutate(study_country = str_to_lower(study_country)) %>% mutate(study_city = str_to_lower(study_city)) %>% 
  group_by(study_country, study_city) %>% summarise(N=n())
 
  #juntado df
DF_by_city <-  full_join(city_pop, Articulos_final) %>% dplyr::filter(!(study_city %in% c("other", "review", "many", NA))) %>% 
  dplyr::filter(!is.na(N)) %>% dplyr::filter(!is.na(lat))

Missing <-sort(DF_by_city$study_city[!(DF_by_city$study_city %in% pop2020$V2)])

DF_by_citySF <- DF_by_city %>% st_as_sf(coords=c(1,2), crs=4326)

saveRDS(DF_by_citySF, "DF_by_citySF.rds")

###

data("countriesHigh")
DF <- st_as_sf(countriesHigh)  %>% dplyr::filter(ADMIN != "Antarctica")
DF_by_citySF <- read_rds("DF_by_citySF.rds")

DF_by_citySF <- DF_by_citySF %>% 
  mutate(city_categ=case_when(pop <100000~"Towns", pop >=100000 & pop <500000~"Small cities", pop >=500000 & pop <1000000~"Medium cities" , pop >=1000000 & pop <5000000~"Large cities" , pop >=5000000 & pop <10000000~"Very large cities" , pop >=10000000~"Megacities" )) %>% 
  mutate(city_categ=fct_relevel(city_categ, "Towns", "Small cities", "Medium cities", "Large cities", "Very large cities", "Megacities" ))


ggplot()+ geom_sf(data=DF,  size=0.2, color="#585858", fill="#666666") +theme_bw() + geom_sf(data=DF_by_citySF, aes(size=N, color=city_categ), show.legend = "point") + 
  scale_color_manual(values = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027")) + theme(legend.position = "bottom")

ggplot() + geom_sf(data = DF2, aes(fill = Articles), lwd=0.05) + facet_grid(Paradigm~.) + theme_bw()+ 
  scale_fill_gradient(low="#56B1F7", high="#132B43", name="Number of articles")


cor(DF_by_citySF$pop, DF_by_citySF$N, method = "spearman")

#ggplot()+ geom_sf(data=DF) + theme_classic() + geom_sf(data=DF_by_citySF, aes(size=N, color=pop), show.legend = "point") + 
#  scale_color_manual(low = "green", mid="yellow", high = "red", midpoint =5000000, labels = scales::comma)


#ggplot()+ geom_sf(data=DF) + geom_sf(data=DF_by_citySF, aes(size=N), show.legend = "point")
#ggplot()+ geom_sf(data=DF) + geom_sf(data=DF_by_citySF, aes(size=pop)) 

#######################################################
#revision de PAISES de afiliacion:                                                            40 PAISES DE AFILIACION
#resumen_afi <- Articulos_final %>% mutate(affi_country = str_to_lower(affi_country))%>%
#    group_by(affi_country) %>% summarise(N=n()) %>% dplyr::filter(affi_country != c("other", "review"), !is.na(affi_country))

#revision de CIUDADES de afiliacion:                                                           CIUDADES DE AFILIACION


#####################################################################
# mapa mundial poblacion por ciudad


#MAPAS ANTIGUOS CON INFO POR PAIS

population <- read_csv("WorldPop.csv") %>% rename("ADMIN" = "region") %>% select("ADMIN", "value")
growth <- read_csv("WorldGrow.csv")%>% rename("ADMIN" = "region") %>% select("ADMIN", "growth")

Articulos_final <- read_csv("Articulos_al2020.csv")

resumen_country <- Articulos_final %>% mutate(study_country = str_to_lower(study_country))%>%
  mutate(study_country= str_replace(study_country, "USA", "United States of America"))%>%
  mutate(study_country= str_replace(study_country, "Emirates", "United Arab Emirates"))%>%
  mutate(study_country= str_replace(study_country, "Tanzania", "United Republic of Tanzania")) %>%
  mutate(study_country= str_replace(study_country, "The Netherlands", "Netherlands"))%>%
  mutate(study_country= str_replace(study_country, "UK", "United Kingdom")) %>%
  dplyr::filter(!(study_country %in% c("caribe",  "many", "other",  "review"))) %>%
    group_by(study_country) %>% summarise(N=n())

resumen_country<- resumen_country %>% rename("ADMIN" = "study_country", articles=N)%>%
                  select("ADMIN", "articles") %>%  dplyr::filter(ADMIN != c("other", "review"), !is.na(ADMIN))


DF <- st_as_sf(countriesHigh) %>% mutate(ADMIN = str_to_lower(ADMIN)) %>% full_join(population) %>%
  full_join(growth)  %>% full_join(resumen_country) %>% dplyr::filter(ADMIN != "antarctica")


saveRDS(DF, "DF_mapas2.rds")
##################


DF <-read_rds("DF_mapas2.rds")

######### population map
a <-ggplot() + geom_sf(data = DF, aes(fill = value), lwd=0.05) + theme_classic() + scale_fill_gradient(low="white", high="#06115e", name="Population", breaks= c(100000000,500000000,900000000,1300000000), labels=function(x) format(x,big.mark = ",", scientific = FALSE)) + labs(title="Population by country")

#ggplot() + geom_sf(data = DF, aes(fill = value), size=0.01) + theme_classic() + scale_fill_gradient(low="#d0d8f7", high = "#132B43",name="Population",labels=function(x) format(x,big.mark = ",", scientific = FALSE))

######### growth map
b <-ggplot() + geom_sf(data = DF, aes(fill = growth), lwd=0.05) + theme_classic() + scale_fill_gradient2(low="red",  high="#06115e", name="Growth",labels=function(x) format(x,big.mark = ",", scientific = FALSE)) + labs(title="Population growth by country")

#########resumen country map
c <-ggplot() + geom_sf(data = DF, aes(fill = articles), lwd=0.05) + theme_classic() + scale_fill_gradient(low="#f2f4f7", high="#06115e",  na.value = "white",  name="Number of articles",labels=function(x) format(x,big.mark = ",", scientific = FALSE))+ labs(title="Publications in Coastal Urban Ecology by country")

grid.arrange(a,b,c, ncol=1)


##########################
#mapa paradigmas anterior choropleth

library(tidyverse)
library(knitr)
library(stringr)
library(gridExtra)
library(sf)

DF <-read_rds("DF_mapas2.rds")
Articulos_final<- read_csv("Articulos_al2020.csv")

Articulos_final2<- Articulos_final %>%  mutate(ADMIN = str_to_lower(study_country)) %>% 
  mutate(ADMIN= str_replace(ADMIN, "usa", "united states of america"))%>% 
  mutate(ADMIN= str_replace(ADMIN, "emirates", "united arab emirates"))%>% 
  mutate(ADMIN= str_replace(ADMIN, "tanzania", "united republic of tanzania")) %>% 
  mutate(ADMIN= str_replace(ADMIN, "the netherlands", "netherlands"))%>% 
  mutate(ADMIN= str_replace(ADMIN, "uk", "united kingdom")) %>% 
  mutate(ADMIN= str_replace(ADMIN, "cyprus", "cyprus no mans area")) %>% 
  dplyr::filter(ADMIN != c("other", "review"), !is.na(ADMIN))



Articulos_final2$paradigm <- paste0(Articulos_final2$paradigm, "_the_city")
resumen <- Articulos_final2 %>%  group_by(ADMIN, paradigm) %>% summarise(N=n())

resumen<- resumen %>% spread(key= paradigm, value=N) 

DF2 <- DF%>% full_join(resumen) %>%   
gather(contains("city") ,key = "Paradigm", value = "Articles") %>% mutate(Paradigm = case_when(Paradigm == "for_the_city" ~"For the city",Paradigm == "in_the_city" ~"In the city",Paradigm == "of_the_city" ~"Of the city")) %>% 
  mutate(Paradigm = fct_relevel(Paradigm, "In the city", "Of the city"))


ggplot() + geom_sf(data = DF2, aes(fill = Articles), lwd=0.05) + facet_grid(Paradigm~.) + theme_bw()+ 
  scale_fill_gradient(low="#56B1F7", high="#132B43", name="Number of articles")


##############antiguo
#graficos paradigmas por separado
a <-ggplot() + geom_sf(data = DF2, aes(fill = in_the_city), lwd=0.05) + theme_classic() + 
  scale_fill_gradient(low="#f2f4f7", high="#06115e", na.value = "white", name="Number of articles",labels=function(x) format(x,big.mark = ",", scientific = FALSE)) + labs(title = "In the city")
b <-ggplot() + geom_sf(data = DF2, aes(fill = of_the_city), lwd=0.05) + theme_classic() + 
  scale_fill_gradient(low="#f2f4f7", high="#06115e", na.value = "white", name="Number of articles",labels=function(x) format(x,big.mark = ",", scientific = FALSE))+ labs(title = "Of the city")
c <-ggplot() + geom_sf(data = DF2, aes(fill = for_the_city), lwd=0.05) + theme_classic() + 
  scale_fill_gradient(low="#f2f4f7", high="#06115e", na.value = "white", name="Number of articles",labels=function(x) format(x,big.mark = ",", scientific = FALSE))+ labs(title = "For the city")

grid.arrange(a,b,c, ncol=1)


