library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(Matrix)
library(RColorBrewer)
library(bibliometrix)
library(readr)

clusteringNetwork <- function(bsk.network,cluster){
  colorlist= c(brewer.pal(9, 'Set1'), brewer.pal(8, 'Set2'),brewer.pal(12, 'Set3'),brewer.pal(12, 'Paired'))
  
  switch(cluster,
         none={
           
           net_groups=list(membership=rep(1,vcount(bsk.network)))},
         optimal={
           net_groups <- cluster_optimal(bsk.network)},
         louvain={
           net_groups <- cluster_louvain(bsk.network)},
         infomap={
           net_groups <- cluster_infomap(bsk.network)},
         edge_betweenness={
           net_groups <- cluster_edge_betweenness(bsk.network)},
         walktrap={
           net_groups <- cluster_walktrap(bsk.network)},
         
         ## default statement
         {cat("\nUnknown cluster argument. Using default algorithm\n")
           net_groups <- cluster_walktrap(bsk.network)}
  )
  
  V(bsk.network)$color <- colorlist[net_groups$membership]
  ### set egde intra-class colors
  V(bsk.network)$community <- net_groups$membership
  El=as.data.frame(get.edgelist(bsk.network,names=F))
  
  
  E(bsk.network)$color <- apply(El, 1, function(x){
    colorlist= c(brewer.pal(9, 'Set1'), brewer.pal(8, 'Set2'),brewer.pal(12, 'Set3'),brewer.pal(12, 'Paired'))
    if (V(bsk.network)$community[x[1]] == V(bsk.network)$community[x[2]]){
      C=colorlist[V(bsk.network)$community[x[1]]]
    }else{C='#E8E8E8'}
    return(C)
  })
  ### end
  
  cl=list()
  cl$bsk.network=bsk.network
  cl$net_groups=net_groups
  return(cl)
}


###############


DF <-read_rds("DFbib.rds") %>% mutate(AU = str_replace_all(string = AU, pattern = "[\U4E00-\U9FFF\U3000-\U303F]", replacement = ""), AU = str_replace_all(string = AU, pattern = "\\[|\\]", replacement = "")) %>% 
  mutate(AU = str_trim(AU)) %>% mutate(AU = str_split(AU, " ", simplify = T)[,1]) %>%  
  mutate(AU = str_replace_all(string =AU, pattern = "[^[:alnum:]]", replacement = "")) %>% 
  dplyr::filter(TI!= "CITIZEN SCIENCE DATASETS REVEAL DRIVERS OF SPATIAL AND TEMPORAL VARIATION FOR ANTHROPOGENIC LITTER ON GREAT LAKES BEACHES") %>% mutate(CR = str_replace_all(string = CR, pattern = "[\U4E00-\U9FFF\U3000-\U303F]", replacement = ""), CR = str_replace_all(string = CR, pattern = "\\[", replacement = ""), CR = str_replace_all(string = CR, pattern = "\\]", replacement = ""))

NewDF <- list()

for(i in 1:nrow(DF)){
  if(is.na(DF$CR[i])){next}else{
    NewDF[[i]] <- data.frame(from = DF$TI[i], to = as.character(str_split(DF$CR[i], pattern = ";", simplify = T)), DOI_From = DF$DI[i], paradigm_from = DF$paradigm[i]) %>% dplyr::filter(to != "") %>% 
      mutate(to = str_trim(to, side = "left"))%>% mutate(AU_to = str_split(to, ",", simplify = T)[,1], AU_to = str_split(AU_to, " ", simplify = T)[,1]) %>% 
      mutate(AU_to = str_remove_all(AU_to, "\\*"))%>% 
      mutate(PY_to = str_split(to, ",", simplify = T)[,2], PY_to = str_trim(PY_to))  %>% 
      mutate(SO_to = str_split(to, ",", simplify = T)[,3], SO_to = str_trim(SO_to))
    message(paste(i, "of", nrow(DF))) 
  }
}


  NewDF <- bind_rows(NewDF)

NewDF <- bind_rows(NewDF) %>% mutate(DOI_to = str_trim(str_split(to, "DOI", simplify = T)[,2])) %>% mutate(DOI_to = str_remove(DOI_to, "\\.$")) %>% mutate(SO_to = ifelse(!is.na(as.numeric(AU_to)), PY_to, SO_to), PY_to = ifelse(!is.na(as.numeric(AU_to)), AU_to, PY_to), AU_to = ifelse(!is.na(as.numeric(AU_to)), NA, AU_to)) %>% 
         mutate(PY_to = as.numeric(PY_to))

### HAsta aqui conservamos todo


NewDF <- NewDF %>% dplyr::filter(PY_to %in% unique(DF$PY))

NewDF$ID <- 1:nrow(NewDF)


DF1 <- DF %>% dplyr::select(TI, AU, PY, SO, DI)

DF2 <- NewDF %>% dplyr::select(AU_to, PY_to, SO_to, DOI_to)


Level1 <- DF2 %>% rename(DI  = DOI_to) %>% right_join(DF1) %>% distinct() %>% dplyr::filter(!is.na(SO_to))

Level2 <- DF2 %>% dplyr::filter(!(DOI_to %in% Level1$DI)) %>% rename(AU = AU_to, PY = PY_to) %>% right_join(DF1) %>% distinct() %>% dplyr::filter(!is.na(SO_to)) %>% dplyr::filter(str_detect(string = SO, pattern = str_split(SO_to, " ", simplify = T)[,1]))


NewDF <- NewDF %>% dplyr::filter(DOI_to %in% Level1$DI)


Edgelist <- NewDF %>% dplyr::select(DOI_From, DOI_to)

Diccionario <- DF %>% dplyr::filter(DI %in% unique(c(NewDF$DOI_From, NewDF$DOI_to))) %>% rename(name = DI)


Red <- graph_from_data_frame(Edgelist) %>% as_tbl_graph() %>% activate(nodes) %>% full_join(Diccionario)




red <-ggraph(Red, layout="graphopt")  + geom_edge_link(arrow = arrow(length = unit(2, 'mm'))) + geom_node_point(aes(color = paradigm), alpha = 0.6,  size = 4) + theme_graph()



saveRDS(red, "red.RDS")


D1 <- Diccionario %>% dplyr::select(name, paradigm) %>% rename(DOI_From = name) %>% right_join(Edgelist) %>% rename(paradigm_from = paradigm)
D2 <- Diccionario %>% dplyr::select(name, paradigm) %>% rename(DOI_to = name) %>% right_join(Edgelist) %>% rename(paradigm_to = paradigm) %>% full_join(D1)


Resumen <- D2 %>% group_by(paradigm_from, paradigm_to) %>% summarise(n = n()) %>% arrange(desc(n))

Red2 <- graph_from_data_frame(Resumen) %>% as_tbl_graph()




red <-ggraph(Red2, layout="graphopt")  + geom_edge_link(arrow = arrow(length = unit(2, 'mm')), aes(width = n)) + geom_node_point(alpha = 0.6,  size = 4) + theme_graph()
