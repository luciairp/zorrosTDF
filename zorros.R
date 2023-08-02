#librarys
library(tidyverse)
library(lubridate)
library(camtrapR)
library(readr)

# CARGAR CSV CON DATOS generales

# creo objeto con dirección a la ruta donde están las imágenes
f_pandemia <- file.path("E:/cámaras pandemia caminos 2020/etiquetadas Leo")

# pruebo sacar metadata
exifTagNames(inDir = f_pandemia)

# leer metadata de conjunto de imágenes, armar tabla
# tabla_2m <- recordTable(inDir = f_pandemia,
#                          cameraID = "directory",
#                          camerasIndependent = F,
#                          IDfrom = "metadata",
#                          minDeltaTime = 2,
#                          writecsv = T,
#                          outDir = "G:/Particion d/Docs/Ushuaia - huillines/resultados zorros/zorrosTDF",
#                          deltaTimeComparedTo="lastIndependentRecord",
#                          metadataSpeciesTag = "Animal",
#                          metadataHierarchyDelimitor ="|" ,
#                          timeZone ="America/Argentina/Buenos_Aires" )
tabla_2m <- read.csv("record_table_2min_deltaT_2022-08-18.csv",header=T,sep=",",stringsAsFactors=FALSE)


# plot horario
library(ggplot2)

horarios <- tabla_2m %>%
  select(Station,DateTimeOriginal,metadata_Animal, metadata_Numero)%>% 
  rename(FechayHora=DateTimeOriginal, Species=metadata_Animal, Grupo=metadata_Numero)%>% 
  mutate(Cuando=dmy_hm(FechayHora))

# arreglar variable "Grupo" que no es numérica

horarios <- horarios %>% 
  mutate(Grupo = case_when(
    Grupo == "2"  ~ 2,
    Grupo == "3"  ~ 3,
    Grupo == "4" ~ 4,
    Grupo == "5" ~ 5,
    Grupo == "6" ~ 6,
    Grupo == "7 o mÃ¡s" ~ 7,
    is.na(Grupo) ~ 1)
  )

horarios_cat <- horarios%>%
  mutate_at(vars(Species),factor)%>%
  replace_na(list(Grupo=1))%>%
  group_by(Station,Species,.drop=FALSE)%>%
  mutate(hora=hour(Cuando))%>%
  filter(Species=='Zorro')


hor_zorro <- ggplot(horarios_cat, aes(x =hora, fill = Species)) +
  geom_bar( width =0.95 )+
  coord_polar(start = 0)+
  theme_minimal()+ 
  scale_fill_manual(values = alpha(c("blue", "red"), .5)) +
  scale_y_continuous("Detecciones por hora")+
  scale_x_continuous("",limits = c(0,24),  breaks = seq(0,24), labels = seq(0,24))+
  labs(fill = "",size=20)+
  theme(text = element_text(size = 15))

hor_zorro
