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

# map

map <-function(years = "2019", 
               chemical = "NOP USDA CERTIFIED", measurement = "MEASURED IN LB"){
  
  usmap::plot_usmap(data = strawb1[strawb1$year==years & 
                                     strawb1$chemical == chemical & 
                                     strawb1$`measurement.s.` == measurement,], 
                    
                    values = "value", 
                    color = "pink",  size = 1,
                    labels = T, label_color = "grey") +
   
    ggplot2::scale_fill_continuous(low = "white", high = "red", name = "log(value)", label = scales::comma)+ 
    ggplot2::theme(legend.position = "right") +
    labs(title="U.S. State Map of Strawberry Value")
}

#map(years = "2019", chemical = "NOP USDA CERTIFIED", measurement = "MEASURED IN LB")



# plot1

# Annual value of strawberries in each state

plot1 <-function(para1){
  dataset <- strawb1 %>% 
    filter(measurement.s. == para1)
  ggplot(data=dataset) +
    geom_bar(mapping = aes(x=factor(chemical.type), y=log(value), fill=state), 
             stat="identity", position= 'dodge', alpha=0.8) +
    labs(x="year", y=paste("log of ", para1, sep = ""))+
    coord_polar()+
    facet_wrap(~factor(year)) +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(title="Annual State Value of Strawberries") 
}


plot1("MEASURED IN LB")



# plot2

#chemical type related value in each state in LB

# Annual value of strawberries in each state
plot2 <-function(para1, para2, para3){
  dataset <- strawb1 %>% 
    filter(measurement.s. == para1) %>%
    filter(chemical.type %in% para2) %>%
    filter(state == para3)
  ggplot(data = dataset)+ 
    geom_bar(mapping= aes(x = chemical, y= log(value), fill=chemical.type), 
             stat='identity', alpha= 0.8)+
    labs(title="") +
    coord_polar()+
    theme_bw()
}

#plot2("MEASURED IN LB","ORGANIC STATUS", "CALIFORNIA")

#plot3

plot3 <-function(para1){
  dataset <- toxin_st %>% 
      filter(measurement.s. == para1)
ggplot(data=dataset)+
  geom_bar(mapping = aes(x=factor(toxin), y=log(value), fill=level), 
           stat="identity", alpha=0.8) +
  labs(x = "toxin", y="value", title = "Toxin level versus Value")+
  facet_wrap(~factor(year)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=0.5), plot.title = element_text(hjust = 0.5))
}

#p3 <- plot3("MEASURED IN LB")
#ggplotly(p3)

# Total toxin levels
#plot4 
plot4 <-function(para1, para2){
  dataset <- toxin_st %>%
    filter(measurement.s. == para1)%>%
    filter(state == para2)
  ggplot()+
    geom_bar(data=dataset,mapping = aes(x=factor(year), y=log(value),fill = chemical.type), 
             stat="identity",alpha=0.8) +
    labs(x="year", y="log(value)", title ="Toxin Changes Over Years")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme_bw()
}

plot4("MEASURED IN LB","CALIFORNIA")

#plot5

plot5 <-function(para1, para2,para3,para4){
  dataset <- toxin_st %>%
    filter(toxin == para2)%>% 
    filter(measurement.s. == para1)%>%
    filter(state == para3)
  ggplot()+
    geom_bar(data=dataset,mapping = aes(x=factor(year), y=log(value),fill = chemical.type), 
             stat="identity",alpha=0.8) +
    labs(x="year", y="log(value)", title ="Bee Toxins in Slight Level")+
    theme_bw()
} 

plot5("MEASURED IN LB","Bee.Toxins","CALIFORNIA")

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
  {a = brewer.pal(6,"Oranges")}
  else if(para2 == "OTHER")
  {a = brewer.pal(6,"RdPu")}
  else if(para2 == "FERTILIZER")
  {a = brewer.pal(6,"Greens")}
  
  data(state.fips)
  mapstate=st_as_sf(map('state',plot = F,fill = T))
  label<- paste("<p><b>",toupper(mapstate$ID), "</p></b>",
                "measurement.s.:", para1, "<br>", 
                "chemical.type:", para2)
  
  
  mymap<-leaflet(mapstate)%>%
    
    addTiles()%>%
  
  addPolygons(data = mapstate %>% filter(ID %in% c("california")),
              stroke = T, fillOpacity = 0.8, weight = 1, 
              color = a[color1[1]]) %>%
    addPolygons(data = mapstate %>% filter(ID %in% c("florida")),
                stroke = T, fillOpacity = 0.8, weight = 1,
                color = a[color1[2]]) %>%
    addPolygons(data = mapstate %>% filter(ID %in% c("new york")),
                stroke = T, fillOpacity = 0.8, weight = 1,
                color = a[color1[3]]) %>%
    addPolygons(data = mapstate %>% filter(ID %in% c("north carolina")),
                stroke = T, fillOpacity = 0.8, weight = 1,
                color = a[color1[4]]) %>%
    addPolygons(data = mapstate %>% filter(ID %in% c("washington")),
                stroke = T, fillOpacity = 0.8, weight = 1,
                color = a[color1[5]]) %>%
    addPolygons(data = mapstate %>% filter(ID %in% c("oregon")),
                stroke = T, fillOpacity = 0.8, weight = 1,
                color = a[color1[6]]) %>%
    
    addPolygons(data=mapstate,label=lapply(label, HTML),color='white',
                stroke = T,fillOpacity = 0,weight = 2)  

  mymap %>% addProviderTiles(providers$OpenTopoMap) 

}


#mo_map("MEASURED IN LB", "FERTILIZER")


