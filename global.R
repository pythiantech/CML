library(tidyverse)
library(RMySQL)
library(DBI)
library(dbplyr)
library(highcharter)
library(visNetwork)
library(DT)
library(igraph)
library(shinyjs)
library(lubridate)
library(gridExtra)
library(data.table)
library(shiny)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(ggmap)
library(leaflet)
library(leaflet.extras)
library(ggthemes)

##########################################
##############  data table  ##############
##########################################
options(DT.options  = list(lengthMenu = list(c(20, 50, 100, -1), c('20','50','100','All')), 
                           serverSide = FALSE,
                           pagingType = "full",
                           dom = 'lfBrtip',
                           buttons = list('copy', 'print', list(
                             extend = 'collection',
                             buttons = c('csv', 'excel', 'pdf'),
                             text = 'Download'
                           ))
)
)

####################################
##############  busy  ##############
####################################
busy <- function (text = "") {
  div(class = "busy",
      p("Please be patient, this can take time!"),
      img(src="loader.gif"),
      hr(),
      p(text)
  )
}
#Kill db connections
killDbConnections <- function () {

  all_cons <- dbListConnections(MySQL())

  print(all_cons)

  for(con in all_cons)
    +  dbDisconnect(con)

  print(paste(length(all_cons), " connections killed."))

}
#Connect to the database
# con <- dbConnect(RMySQL::MySQL(),
#                  dbname = 'ebdb',
#                  host= "healthcare.cngpocha5xz0.ap-south-1.rds.amazonaws.com",
#                  port=3306,
#                  user='admin',
#                  password='password')
# 
# #List the tables in the database
# dbListTables(con)
# 
# 
# patients <- tbl(con, "RegisterPatient") %>% collect()
# patients$gender <- as.factor(patients$gender)
# patients$registerdate <- ymd(patients$registerdate)
# patients <- patients %>% filter(patientId!="Hospital1_1") #Remove Amit Kumar Gupta (that's our developer)
# colnames(patients)[1] <- "PatientId"
# patients$registerdate <- ymd(patients$registerdate)
# 
# #Filter for hospital 1 only
# patients <- patients %>% filter(grepl('Hospital1_',PatientId))
# 
# initDetail <- tbl(con, "InitialDetail") %>% collect()
# diseasePhase <- tbl(con, "DiseasePhase") %>% collect()
# diagDetail <- tbl(con, "DiagnosisDetail") %>% collect()
# patientDisease <- tbl(con, "PatientDisease") %>% collect()
# treatment <- data.table(tbl(con, "Treatment") %>% collect())
# treatment[681,3] <- "2001-03-09"
# pbd <- tbl(con, "PeripheralBloodDetails") %>% collect()
# CCR <- tbl(con,"CCR") %>% collect()
# bct <- tbl(con, "BiochemicalTests") %>% collect()
# status <- tbl(con, "PatientAliveStatus") %>% collect()
# PCR <- tbl(con, "PCR") %>% collect()
# 
# killDbConnections()
# 
# initDetail <- initDetail %>% filter(PatientId %in% unique(patients$PatientId))
# 
# treatment <- treatment %>% filter(PatientId %in% unique(patients$PatientId))
# treatment$dateofStarting <- ymd(treatment$dateofStarting)
# treatment$submittedDate <- ymd(treatment$submittedDate)

#Filtering for consecutive rows with difference
#https://stackoverflow.com/questions/38035323/comparing-consecutive-rows-and-select-rows-where-are-subsequent-is-a-specific-va
#https://stackoverflow.com/questions/14846547/calculate-difference-between-values-in-consecutive-rows-by-group
# x <- treatment %>% group_by(PatientId) %>% arrange(dateofStarting) %>%
#   mutate(ind = nameTKI=="Imatinib" & lead(nameTKI)!="Imatinib") %>%
#   slice(sort(c(which(ind),which(ind)+1))) %>%
#   select(-ind) %>%
#   select(PatientId,nameTKI,dateofStarting) %>%
#   mutate(Diff=c(NA,as.numeric(diff(dateofStarting))))
# mean(x$Diff,na.rm = TRUE)
# plot(x$Diff)

#PCR analysis
PCR1 <- PCR %>% filter(PatientId %in% unique(patients$PatientId))
PCR1 <- PCR1 %>% left_join(patients,by='PatientId') %>%
  select(PatientId,age,gender,submittedDate,labname, BCR_ABL) %>%
  left_join(treatment,by='PatientId') %>%
  left_join(initDetail, by='PatientId') %>%
  select(PatientId,age,gender,submittedDate.x,submittedDate.y,dateofStarting.x,BCR_ABL.x,nameTKI.x,brandTKI.x,
         dailydose.x,labname,sokel,hosford,eutos) %>%
  arrange(dateofStarting.x)
PCR1 <- PCR1 %>% distinct(dateofStarting,BCR_ABL.x,.keep_all = TRUE)
PCR1$submittedDate.x <- ymd(PCR1$submittedDate.x)
PCR1$dailydose.x <- as.numeric(PCR1$dailydose.x)
PCR1$DT <- paste(month(PCR1$dateofStarting.x),year(PCR1$dateofStarting.x),sep = '-')

# data(gapminder, package = "gapminder")
# gg <- ggplot(PCR1, aes(sokel, hosford, color = brandTKI.x)) +
#   geom_point(aes(size=1/log(BCR_ABL.x+1),frame = Year, ids = PatientId))+
#   scale_x_log10()
# ggplotly(gg)

total <- nrow(patients)
genderCount <- patients %>% count(gender)

#Get coordinates from cities
coords <- readRDS("coords.Rds")
`%nin%` <- Negate(`%in%`)
newrecords <- patients %>% filter(city %nin% coords$City)
if(nrow(newrecords)>0){
  coords <- bind_rows(coords, bind_cols(geocode(newrecords$city),City=newrecords$city))
}
saveRDS(coords,"coords.Rds")

#Top symptoms
#First combine gender too
Symptoms <- initDetail %>% left_join(patients, by="PatientId") %>% 
  select(gender,symptoms)
Symptoms <- separate(Symptoms, 'symptoms', paste("Symptom",1:5), sep=",", extra = "drop") %>% 
  gather(key="symptom",value="value",-gender) %>% 
  select(gender, value) %>% group_by(value, gender) %>% filter(value!="") %>% 
  summarize(Count=n()) %>% filter(value!=" " & Count>10) %>% arrange(desc(Count))

#Adding risk levels to eutos, sokal and hasford
initDetail$SokelRisk <- ifelse(initDetail$sokel<0.8, "Low",
                               ifelse(initDetail$sokel>0.8 & initDetail$sokel<=1.2, "Intermediate","High"))
initDetail$HosfordRisk <- ifelse(initDetail$hosford<=780, "Low",
                               ifelse(initDetail$hosford>780 & initDetail$hosford<=1480, "Intermediate","High"))
initDetail$EutosRisk <- ifelse(initDetail$eutos<87, "Low","High")

#BCR_ABL can't be greater than 100?
initDetail$BCR_ABL[initDetail$BCR_ABL>100] <- initDetail$BCR_ABL[initDetail$BCR_ABL>100]/100

#
