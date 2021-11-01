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
library(cowplot)
library(usmap)
library(plotly)

source(file = "strawbData.R", echo = FALSE)


# plot1

# yearly value of strawberries in each state

plot1 <-function(para1){
  dataset <- strawb1 %>% 
    filter(measurement.s. == para1)
  ggplot(data=dataset) +
    geom_bar(mapping = aes(x=factor(chemical.type), y=log(value), fill=state), 
             stat="identity", position= 'dodge', alpha=0.8) +
    labs(x="year", y=paste("log of", para1, sep = ""))+
    coord_polar()+
    facet_wrap(~factor(year)) +
    theme_bw()
}

p1 <- plot1("MEASURED IN LB")
ggplotly(p1)

# plot2

#chemical type related value in each state in LB

# yearly value of strawberries in each state
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
  labs(x="toxin", y="value")+
  facet_wrap(~factor(year)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=0.5))
}

plot3("MEASURED IN LB")

# map

map <-function(years = "2019",state = "CALIFORNIA", 
               chemical = "NOP USDA CERTIFIED", measurement = "MEASURED IN LB"){
  
  usmap::plot_usmap(data = strawb1[strawb1$year==years & 
                                     strawb1$state == state & 
                                     strawb1$chemical == chemical & 
                                     strawb1$`measurement.s.` == measurement,], 
                    
                    values = "value", 
                    color = "pink",  size = 1,
                    labels = T, label_color = "grey") +
    
    
    # geom_point(data = strawb1[strawb1$year==years & 
    #                                 strawb1$state == state & 
    #                                strawb1$chemical == chemical & 
    #                                strawb1$`measurement.s.` == measurement,])+
    
    ggplot2::scale_fill_continuous(low = "white", high = "red", name = "", label = scales::comma)+ 
    ggplot2::theme(legend.position = "right")
}

#map()

