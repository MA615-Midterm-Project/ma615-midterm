library(magrittr)
library(tidyselect)
library(readxl)
library(tidyr)
library(base)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(maps)
library(sf)
library(sp)
library(cowplot)
library(usmap)
library(plotly)
library(leaflet)
library(GGally)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)

source(file = "strawbData.R", echo = FALSE)


mo_map <- function(para1, para2){
  
  #input the measurement and the chemical type
  state <- cbind("CALIFORNIA","FLORIDA","NEW YORK","NORTH CAROLINA","WASHINGTON","OREGON")
  dataset_sub <- rep(NA, 6)
  color <- rep(NA, 6)
  
  for(i in 1:6){
    
    dataset_sub = 0
    STATE <- state[i]
    dataset_sub <- strawb1 %>% 
      filter(measurement.s. == para1) %>%
      filter(chemical.type %in% para2) %>%
      filter(state == STATE) 
    
    dataset <- strawb1 %>% 
      filter(measurement.s. == para1) %>%
      filter(chemical.type %in% para2)
    
    #chemical.type percentage on different state
    percentage <- sum(dataset_sub$value)/sum(dataset$value)
    color[i] <- percentage
  }
  
  color1 <- rank(color)
  
  for(i in 1:6){
    color1[i] <- ifelse(color[i] == 0, 1, color1[i])
  }
  
  #color for different chemical.type
  if(para2 == "ORGANIC STATUS")
  {a = brewer.pal(6,"Purples")}
  else if(para2 == "FUNGICIDE")
  {a = brewer.pal(6,"Blues")}
  else if(para2 == "HERBICIDE")
  {a = brewer.pal(6,"Greys")}
  else if(para2 == "INSECTICIDE")
  {a = brewer.pal(6,"Reds")}
  else if(para2 == "OTHER")
  {a = brewer.pal(6,"RdPu")}
  else if(para2 == "FERTILIZER")
  {a = brewer.pal(6,"RdPu")}
  
  data(state.fips)
  mapstate=st_as_sf(maps::map('state',plot = F,fill = T))
  label<- paste("<p><b>",toupper(mapstate$ID), "</p></b>",
                "measurement.s.:", para1, "<br>", 
                "chemical.type:", para2)
  
  
  mymap<-leaflet(mapstate)%>%
    
    addTiles()%>%
    
    addPolygons(data = mapstate %>% filter(ID %in% c("california")),
                stroke = T, fillOpacity = 1, weight = 1, 
                color = a[color1[1]]) %>%
    addPolygons(data = mapstate %>% filter(ID %in% c("florida")),
                stroke = T, fillOpacity = 1, weight = 1,
                color = a[color1[2]]) %>%
    addPolygons(data = mapstate %>% filter(ID %in% c("new york")),
                stroke = T, fillOpacity = 1, weight = 1,
                color = a[color1[3]]) %>%
    addPolygons(data = mapstate %>% filter(ID %in% c("north carolina")),
                stroke = T, fillOpacity = 1, weight = 1,
                color = a[color1[4]]) %>%
    addPolygons(data = mapstate %>% filter(ID %in% c("washington")),
                stroke = T, fillOpacity = 1, weight = 1,
                color = a[color1[5]]) %>%
    addPolygons(data = mapstate %>% filter(ID %in% c("oregon")),
                stroke = T, fillOpacity = 1, weight = 1,
                color = a[color1[6]]) %>%
    
    addPolygons(data=mapstate,label=lapply(label, HTML),color='white',
                stroke = T,fillOpacity = 0,weight = 2)  
  
  mymap %>% addProviderTiles(providers$OpenTopoMap) 
  
}

#map(years = "2019", chemical = "NOP USDA CERTIFIED", measurement = "MEASURED IN LB")

mo_map("MEASURED IN LB", "FERTILIZER")
