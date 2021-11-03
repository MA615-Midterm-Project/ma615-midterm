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

#drop empty columns

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

# separate one column into more detailed columns
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

#remove white space and unnecessary signs in certain variables
strawb %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )

strawb %<>% mutate(type = str_trim(type))

# data cleanning and extracting need data 
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
strawb1 <- data.frame("year" = strawb$Year, "state" = strawb$State, "measurement(s)" = strawb$discription, 
                      "chemical" = strawb$details, "chemical.type" = strawb$type, "value" = log(strawb$Value))

#strawberry and toxin

Pest <- read.csv("Pesticides.csv")

Pest <- Pest[Pest$Pesticide != "",]
strawb1%<>% mutate(chemical= str_trim(strawb1$chemical))
Pest %<>% mutate(Pesticide = str_trim(Pest$Pesticide))
Pest$chemical <- toupper(Pest$Pesticide)
Pest<- Pest[, c(7,2,3,4,5,6)]
Pest1<- Pest%>%
  pivot_longer(!chemical, names_to = "toxin", values_to = "level") 
toxin_st<- strawb1 %>% inner_join(Pest1,by="chemical")
toxin_st$level[toxin_st$level==""] <- NA
toxin_st<- na.omit(toxin_st)


