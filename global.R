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
library(shinyjs)
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
# # # 
# # # 
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
# treatment[691,3] <- "2016-03-09"
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
initDetail$BCR_ABL[initDetail$BCR_ABL>100] <- 100

########################################################################
#Change disease phase
initDetail$DiseasePhase[initDetail$DiseasePhase==""] <- "Chronic Phase"
initDetail$DiseasePhase[initDetail$DiseasePhase=="Myeloid"|initDetail$DiseasePhase=="Lymphoid"] <- "Blast Crisis"

#Change PLT
initDetail$PLT[initDetail$PLT<10] <- 10
initDetail$PLT[initDetail$PLT>1800] <- 1800

#Change eosinophils, basophils and Blast
initDetail$Eosinophils <- ifelse(initDetail$Eosinophils%%1>0, initDetail$Eosinophils*10,
                                 initDetail$Eosinophils)
initDetail$Basophils <- ifelse(initDetail$Basophils%%1>0, initDetail$Basophils*10,
                                 initDetail$Basophils)
initDetail$Blast <- ifelse(initDetail$Blast%%1>0, initDetail$Blast*10,
                                 initDetail$Blast)

#New Calculations for Sokal, Hasford & Eutos
NewInitDetails <- initDetail %>% left_join(patients, by="PatientId") %>% 
  select(colnames(initDetail), age)

NewInitDetails$Sokal <- exp(0.0116*(NewInitDetails$age - 43.4)) + 
  (0.0345*(NewInitDetails$spleenSize - 7.51)) + (0.188*((NewInitDetails$PLT/700)^2 - 0.563)) + 
     (0.0887*(NewInitDetails$Blast - 2.10))
NewInitDetails$Hasford <- ((0.6666*ifelse(NewInitDetails$age<50,0,1)) + 
  (0.0420*NewInitDetails$spleenSize) + (0.0584*NewInitDetails$Blast) + 
  (0.0413*NewInitDetails$Eosinophils) + (0.2039*ifelse(NewInitDetails$Basophils<3,0,1)) + 
  (1.0956*ifelse(NewInitDetails$PLT<1500,0,1)))*1000
NewInitDetails$EUTOS <- (7*NewInitDetails$Basophils)+(4*NewInitDetails$spleenSize)

#New risk scores
NewInitDetails$SokalRisk <- ifelse(NewInitDetails$Sokal<0.8, "Low",
                               ifelse(NewInitDetails$Sokal>0.8 & NewInitDetails$Sokal<=1.2, "Intermediate","High"))
NewInitDetails$HasfordRisk <- ifelse(NewInitDetails$Hasford<=780, "Low",
                                 ifelse(NewInitDetails$Hasford>780 & NewInitDetails$Hasford<=1480, "Intermediate","High"))
NewInitDetails$EUTOSRisk <- ifelse(NewInitDetails$EUTOS<87, "Low","High")

#############################################
#PCR analysis
PCR1 <- PCR %>% filter(PatientId %in% unique(patients$PatientId))
PCR1 <- PCR1 %>% left_join(patients,by='PatientId') %>%
  select(PatientId,age,gender,submittedDate,labname, BCR_ABL) %>%
  left_join(treatment,by='PatientId') %>%
  left_join(NewInitDetails, by='PatientId') %>%
  select(PatientId,age.x,gender,submittedDate.x,submittedDate.y,dateofStarting.x,BCR_ABL.x,nameTKI.x,brandTKI.x,
         dailydose.x,labname) %>%
  arrange(dateofStarting.x)
PCR1 <- PCR1 %>% distinct(dateofStarting,BCR_ABL.x,.keep_all = TRUE)
PCR1$submittedDate.x <- ymd(PCR1$submittedDate.x)
PCR1$dailydose.x <- as.numeric(PCR1$dailydose.x)
PCR1$DT <- paste(month(PCR1$dateofStarting.x),year(PCR1$dateofStarting.x),sep = '-')

############################################################
#PCA Factor analysis
library(corrplot)
library(FactoMineR)
library(factoextra)

#Create a new dataset with relevant variables
PCAset <- NewInitDetails %>% select(spleenSize, liver, Hb:Creat,aspirate,BCR_ABL,age:EUTOSRisk )
PCAset$SokalRisk <- as.factor(PCAset$SokalRisk)
PCAset$HasfordRisk <- as.factor(PCAset$HasfordRisk)
PCAset$EUTOSRisk <- as.factor(PCAset$EUTOSRisk)
#Correlation matrix
cor.mat <- round(cor(PCAset[,1:27]),2)


#Scatter Plot Matrix
library(PerformanceAnalytics)
# chart.Correlation(PCAset[,1:27],histogram = TRUE, pch=19)

#PCA
res.pca <- PCA(PCAset[,1:27],scale.unit = T,graph = FALSE)
#Variances of the eigen values
eigenvalues <- res.pca$eig
head(eigenvalues[, 1:2])
#Scree plot
fviz_screeplot(res.pca, ncp=10)
#Graph of variables
fviz_pca_var(res.pca, col.var="contrib")#+
  # scale_color_gradient2(low="white", mid="blue", 
  #                       high="red", midpoint=55)+theme_bw()

#Graph of individuals
fviz_pca_ind(res.pca)
fviz_pca_ind(res.pca, geom="text")
#Make a biplot of individuals and variables :
fviz_pca_biplot(res.pca,  geom = "text")

fviz_pca_ind(res.pca,geom.ind = "point", # show points only (nbut not "text")
             col.ind = PCAset$SokalRisk, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             repel=TRUE,
             # ellipse.type="confidence",# Concentration ellipses
             legend.title = "Groups")#, habillage=PCAset$SokalRisk)
