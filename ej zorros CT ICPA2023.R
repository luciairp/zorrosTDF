
# bibliotecas
# install.packages("camtrapR")
library(camtrapR)

# reemplazar ruta por la que corresponda a las imagenes
f_2020 <- file.path("H:/CT ICPA 2023/imag zorros 2020 para ejercicio")


# leer metadata de conjunto de imágenes, armar tabla
tabla_2m <- recordTable(inDir = f_2020,
                         cameraID = "directory",
                         camerasIndependent = F,
                         IDfrom = "metadata",
                         minDeltaTime = 2,
                         writecsv = T,
                         deltaTimeComparedTo="lastRecord",
                         metadataSpeciesTag = "Animal",
                         metadataHierarchyDelimitor ="|" ,
                         timeZone ="America/Argentina/Buenos_Aires" )

# explorar un poquito la tabla
names(tabla_2m)

library(tidyverse)

tabla_2m <- tabla_2m %>% 
  rename(metadata_Numero = metadata_NÃºmero) %>% 
  mutate(metadata_Numero = case_when(is.na(metadata_Numero) ~ 1))

# algunas tablas resumen

# por especie
resumen_sp <- tabla_2m %>% 
  group_by(metadata_Animal) %>% 
  summarise(
    n = n()
  )

# por sitio
unique(tabla_2m$metadata_Animal)
resumen_sitio_zorro <- tabla_2m %>% 
  group_by(Station) %>% 
  filter(metadata_Animal == 'Zorro') %>% 
  summarise(
    n = n()
  )


# armemos el gráfico de apariciones según el horario
library(lubridate)
library(ggplot2)

# extraigo información horaria de la tabla de registros independientes
# y le cambio los nombres para que sea más amable
horarios <- tabla_2m %>%
  select(Station,DateTimeOriginal,metadata_Animal, metadata_Numero)%>% 
  rename(FechayHora=DateTimeOriginal, Species=metadata_Animal, Grupo=metadata_Numero)%>% 
  mutate(Cuando=ymd_hms(FechayHora))


# construir recuento de registros de zorro por hora
horarios_cat <- horarios%>%
  mutate_at(vars(Species),factor)%>%
  group_by(Station,Species,.drop=FALSE)%>%
  mutate(hora=hour(Cuando))%>%
  filter(Species=='Zorro')

# ahora sí: gráfico de apariciones de zorro según horario
hor_zorro <- ggplot(horarios_cat, aes(x =hora, fill = Species)) +
  geom_bar( width =0.95 )+
  coord_polar(start = 0)+
  theme_minimal()+ 
  scale_fill_manual(values = alpha("darkgreen", .5)) +
  scale_y_continuous("Detecciones por hora")+
  scale_x_continuous("",limits = c(0,24),  breaks = seq(0,24), labels = seq(0,24))+
  labs(fill = "",size=20)+
  theme(text = element_text(size = 15))

hor_zorro
