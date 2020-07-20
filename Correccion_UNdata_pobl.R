

## A partir de los datos de UNdata.csv se filtra y modifica para dejar lo que se quiere utilizar 
# (ultimos datos poblacionales por ciudad), grandes ciudades con poblacion de areas metropolitanas, ambos sexos


## Leemos la base de datos
UNdata <- read_csv("UNdata.csv") %>%
  # solo la info de 2 sexos
  dplyr::filter(Sex == "Both Sexes") %>% 
  group_by(City) %>% 
  # el año de fuente mas reciente
  dplyr::filter(Year == max(`Source Year`))


# Algunas ciudades tienen estimado para la ciudad y para area metropolitna
Nummber_Of <- UNdata %>% group_by(City) %>% summarise(n = n()) %>% arrange(desc(n))

Metropolitanas <- Nummber_Of %>% 
  ungroup() %>% 
  dplyr::filter(n == 2) %>% 
  left_join(UNdata) %>% 
  dplyr::filter(`City type` == "Urban agglomeration")

Ciudades <- Nummber_Of %>% 
  ungroup() %>% 
  dplyr::filter(n == 1) %>% 
  left_join(UNdata) 

UNdata_corregido <- bind_rows(Metropolitanas, Ciudades) %>% 
  dplyr::select(-n) %>%
  rename(pop = Value) %>% 
  mutate(city_categ=case_when(pop <100000~"Non-urban areas", pop >=100000 & pop <500000~"Small cities", pop >=500000 & pop <1000000~"Medium cities" , pop >=1000000 & pop <5000000~"Large cities" , pop >=5000000 & pop <10000000~"Very large cities" , pop >=10000000~"Megacities" )) %>% 
  mutate(city_categ=fct_relevel(city_categ, "Non-urban areas", "Small cities", "Medium cities", "Large cities", "Very large cities", "Megacities" ))


saveRDS(UNdata_corregido, "UNdata_corregido.rds")
