library(magrittr)
library(tidyselect)
library(readxl)
library(tidyr)
library(base)
library(dplyr)
library(stringr)
Pest <- read.csv("Pesticides.csv")
Pest <- Pest[Pest$Pesticide != "",]

strawb <- read.csv("Strawberries.csv",fileEncoding= "latin1")

drop_no_info_cols <- function(df){
  cnames = colnames(strawb)
  T = NULL
  for(i in 1:ncol(df)){ 
    T <- c(T, nrow(unique(df[i])))
  }
  drop_cols <- cnames[which(T == 1)]
  return(df[, !names(df) %in% all_of(drop_cols)])
}

strawb <- drop_no_info_cols(strawb)

strawb %<>% separate(col=Data.Item,
                     into = c("Strawberries", "items", "discription", "units"),
                     sep = ",",
                     fill = "right")

strawb %<>%  separate(col=Domain,
                      into = c("dname", "type" ), 
                      sep = ",", 
                      fill = "right")

strawb %<>% 
  mutate(Chemicals = Domain.Category) %>% 
  relocate(Chemicals, .after = Domain.Category) 

strawb %<>% separate(col = Chemicals,
                     into = c("title", "details"),
                     sep = ":",
                     fill = "right")

strawb %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )

strawb %<>% mutate(type = str_trim(type))


for (i in 1:nrow(strawb[is.na(strawb$details),])) {
  strawb[is.na(strawb$details),]$details <- rep("NOT SPECIFIED", nrow(strawb[is.na(strawb$details),]))
}

for (i in 1:nrow(strawb[is.na(strawb$type),])) {
  strawb[is.na(strawb$type),]$type <- strawb[is.na(strawb$type),]$dname
}

bb <- strawb$discription %>% str_detect("MEASURED")
bb[is.na(bb)] <- FALSE
## index 
ind_C <- (!bb)*(1:dim(strawb)[1])
## 
r1 <- ind_C[ind_C > 0]
## set entries in Chemicals column to " " if they don't start with CHEM
strawb$discription[r1] <- strawb$units[r1]

strawb %<>%  separate(col=details,
                      into = c("details", "No." ), 
                      sep = "=", 
                      fill = "right")
strawb %<>% mutate(Value = str_extract(str_trim(Value) ,"[^(].*[^)]") )
strawb$Value[strawb$Value == "NA"] <- NA
strawb <- strawb[!is.na(strawb$discription) & !is.na(strawb$Value),]

strawb$Value  <-  as.numeric(gsub( ",","",strawb$Value))
strawb %<>% mutate(discription = str_trim(discription))

strawb$Value[strawb$discription == "MEASURED IN CWT" ] <- strawb$Value[strawb$discription == "MEASURED IN CWT" ]*100
strawb$discription[strawb$discription == "MEASURED IN CWT"] <- rep("MEASURED IN LB", length(strawb$discription[strawb$discription == "MEASURED IN CWT" ]))
strawb1 <- data.frame("year" = strawb$Year, "state" = strawb$State, "measurement(s)" = strawb$discription, "chemical" = strawb$details, "chemical.type" = strawb$type, "value" = strawb$Value)

# strawb_fungi <- strawb1 %>% filter((chemical.type=="FUNGICIDE"))
# strawb_herb <- strawb1 %>% filter((chemical.type=="HERBICIDE"))
# strawb_insect <- strawb1 %>% filter((chemical.type=="INSECTICIDE"))
a <- unique(strawb1$ `measurement.s.`)
measure_dollar <- strawb1[strawb1$measurement.s. == a[1],]
measure_LB <- strawb1[strawb1$measurement.s. == a[2],]
measure_LAAP <- strawb1[strawb1$measurement.s. == a[3],]
measure_LAyear <- strawb1[strawb1$measurement.s. == a[4],]
measure_number <- strawb1[strawb1$measurement.s. == a[5],]
measure_AB <- strawb1[strawb1$measurement.s. == a[6],]


#strawberry and toxin

Pest <- read.csv("Pesticides.csv")



Pest <- Pest[Pest$Pesticide != "",]
strawb1%<>% mutate(chemical= str_trim(strawb1$chemical))
Pest %<>% mutate(Pesticide = str_trim(Pest$Pesticide))
Pest$chemical <- toupper(Pest$Pesticide)
Pest<- Pest[, c(7,2,3,4,5,6)]
straw_health<- strawb1 %>% inner_join(Pest,by="chemical")


Pest1<- Pest%>%
  pivot_longer(!chemical, names_to = "toxin", values_to = "level") 
toxin_st<- strawb1 %>% inner_join(Pest1,by="chemical")
toxin_st$level[toxin_st$level==""] <- NA
toxin_st<- na.omit(toxin_st)


