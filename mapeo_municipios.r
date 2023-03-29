# Lista de paquetes que se necesitan
packages <- c("janitor", "rlang", "sf", "tidyverse", "ggplot2")

# Instalar paquetes faltantes
packages_needed <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(packages_needed) > 0) {
  lapply(packages_needed, install.packages)
}

# Cargar paquetes
lapply(packages, require, character.only = TRUE)

rm(list=ls())

#descargamos el shapefile de las secciones censales de españa
temp <- tempfile()
mydir <- getwd()
zip_file <- list.files(mydir, pattern = ".zip$", full.names = TRUE)
unzip(zipfile = zip_file, exdir = temp)
shp_file <- list.files(temp, pattern = ".shp$", full.names = TRUE)
censales_shp <- sf::read_sf(shp_file)


rm(mydir, temp, zip_file, shp_file)


#en un dataframe llamado municipios guarda los casos que tengan el valor en la variable NMUN igual a "Santiago de Compostela" o "Brión" o "Ames" o "Oroso" o "Teo"
municipios <- censales_shp %>%
    filter(NMUN=="Santiago de Compostela" | NMUN=="Brión" | NMUN=="Ames" | NMUN=="Oroso" | NMUN=="Teo")


#con st_union y summarize
municipios <- municipios %>% 
  group_by(CUMUN) %>% 
  summarize(geometry = st_union(geometry))

#cambiar el nombre de la columna CUMUN por municipio
colnames(municipios)[1] <- "municipio"


#importar datos de población
munc <- read.csv("mun.csv", header = TRUE, sep = ";", dec = ",")


#limpiar nombres de columnas en poblacion con janitor
munc <- munc %>% clean_names()

str(munc)


#unir distritos_union con poblacion
municipio_datos <- merge(municipios, munc, by = "municipio", all.x = TRUE) 



#en el dataframe municipios_2, en la columna "municipio, " cambair el valor 15002 por "Ames", el valor 15013 por "Brión", el valor 15060 por "Oroso",el valor  15078 por "Santiago de Compostela" y el valor 15082 por "Teo"
municipios$municipio <- ifelse(municipios$municipio == 15002, "Ames", 
                                ifelse(municipios$municipio == 15013, "Brión",
                                ifelse(municipios$municipio == 15060, "Oroso",
                                ifelse(municipios$municipio == 15078, "Santiago de Compostela",
                                ifelse(municipios$municipio == 15082, "Teo", 
                                       municipios$municipio)))))






municipios %>% 
  ggplot()  +
  geom_sf(color="#000000", fill="#3B9ADF", alpha=0.6) +
  theme_void()+
  theme(legend.position = "none")+
  geom_sf_label(aes(label = municipio), color = "#000000", size=6)

