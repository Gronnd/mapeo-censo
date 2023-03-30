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


# Leer el shapefile de las secciones censales de España directamente desde el archivo ZIP
zip_file <- "Seccionado_2021.zip"
unzip(zipfile = zip_file, exdir = tempdir())
shp_file <- list.files(tempdir(), pattern = ".shp$", full.names = TRUE)
censales_shp <- sf::read_sf(shp_file)

rm(shp_file, zip_file)

# Función para filtrar y unir
filter_and_union <- function(shp, CUSEC_values) {
  shp %>%
    filter(CUSEC %in% CUSEC_values) %>%
    st_union()
}

zonas <- list(
  "Distrito 1" = censales_shp %>% filter(grepl("^Santiago de Compostela", NMUN) & grepl("^01$", CDIS)) %>% pull(CUSEC),
  "Distrito 2" = censales_shp %>% filter(grepl("^Santiago de Compostela", NMUN) & grepl("^02$", CDIS)) %>% pull(CUSEC),
  "Distrito 3" = censales_shp %>% filter(grepl("^Santiago de Compostela", NMUN) & grepl("^03$", CDIS)) %>% pull(CUSEC),
  "Distrito 4" = censales_shp %>% filter(grepl("^Santiago de Compostela", NMUN) & grepl("^04$", CDIS)) %>% pull(CUSEC),
  "Distrito 5" = censales_shp %>% filter(grepl("^Santiago de Compostela", NMUN) & grepl("^05$", CDIS)) %>% pull(CUSEC),
  "Distrito 6" = censales_shp %>% filter(grepl("^Santiago de Compostela", NMUN) & grepl("^06$", CDIS)) %>% pull(CUSEC),
  "Os tilos" = c("1508201006", "1508201007", "1508201008"),
  "Sigüeiro" = c("1506001005", "1506001001"),
  "Milladoiro" = c("1500203005", "1500203006", "1500203008", "1500203009", "1500203013", "1500203014", "1500203016"),
  "Brión e Bertamiráns" = c("1501302002", "1500203015", "1500203010", "1500203011", "1500203012", "1500203007", "15002030062")
)


# Bucle para iterar sobre zonas y códigos CUSEC
geoms_df <- vector("list", length(zonas))
names(geoms_df) <- names(zonas)

for (zona in names(zonas)) {
  geom_temp <- filter_and_union(censales_shp, zonas[[zona]])
  geoms_df[[zona]] <- data.frame(zona = zona, stringsAsFactors = FALSE)
  st_geometry(geoms_df[[zona]]) <- st_geometry(geom_temp)
}

# Crear nuevo objeto sf con todas las geometrías y números de distrito
distritos_union <- do.call("rbind", geoms_df) %>% 
  mutate(distrito = 1:n())

# Verificar que el objeto sf tenga la columna distrito
head(distritos_union)



#importar datos de población
poblacion <- read.csv("pop.csv", header = TRUE, sep = ";", dec = ",")


#limpiar nombres de columnas en poblacion con janitor
poblacion <- poblacion %>% clean_names()

str(poblacion)



#unir distritos_union con poblacion
santiago_datos <- merge(distritos_union, poblacion, by = "zona", all.x = TRUE, sep = ";", dec = ",") 



#reordenar el dataframe prueba en base a la variable distrito
santiago_datos <- santiago_datos   %>% 
    arrange(distrito)

#borrar variable distrito
santiago_datos <- santiago_datos[,-2]

str(santiago_datos)

#set , as decimal separator
options(OutDec = ",")


#crear mapa de media de desplazamientos
santiago_datos %>% 
    ggplot() +
    geom_sf(aes(fill = media_de_desplazamientos), color = "black", linewidth = .6) +
    theme_void()+
    theme(legend.position = "bottom")+
    scale_fill_continuous(name = "Media de desplazamientos", trans = 'reverse')+
    theme(legend.text = element_text(size = 20),
          legend.title = element_text(size= 20))+
          guides(fill = guide_colorbar(barwidth = 2, barheight = 15, title.position = "top"))+
    theme(text = element_text(size = 24))+
    theme(legend.position = "none")+
#añadir etiquetas con decimales
          geom_sf_label(aes(label = paste0(zona,": ", format((round(media_de_desplazamientos, 2)), decimal.mark = getOption("OutDec")))), size = 6,  color = "#000000", inherit.aes = FALSE)
 #añadir etiquetas con porcentaje
          #geom_sf_label(aes(label = paste0(zona,": ", round(x_de_movilidad_activa_como_medio_principal * 100, 2), "%")),size = 6,  color = "black", inherit.aes = FALSE)


#crear mapa del % de desplazamientos sobre el total
santiago_datos %>% 
    ggplot() +
    geom_sf(aes(fill = x_desplazamientos_sobre_total), color = "black", linewidth = .6) +
    theme_void()+
    theme(legend.position = "bottom")+
    scale_fill_continuous(name = "Media de desplazamientos", trans = 'reverse')+
    theme(legend.text = element_text(size = 20),
          legend.title = element_text(size= 20))+
          guides(fill = guide_colorbar(barwidth = 2, barheight = 15, title.position = "top"))+
    theme(text = element_text(size = 24))+
    theme(legend.position = "none")+
    geom_sf_label(aes(label = paste0(round(x_desplazamientos_sobre_total * 100, 2), "%")),size = 6,  color = "black", inherit.aes = FALSE)


#crear mapa del % de desplazamientos sobre el total
santiago_datos %>% 
    ggplot() +
    geom_sf(aes(fill = ratio_de_pob_despl), color = "black", linewidth = .6) +
    theme_void()+
    theme(legend.position = "bottom")+
    scale_fill_continuous(name = "Media de desplazamientos", trans = 'reverse')+
    theme(legend.text = element_text(size = 20),
          legend.title = element_text(size= 20))+
          guides(fill = guide_colorbar(barwidth = 2, barheight = 15, title.position = "top"))+
    theme(text = element_text(size = 24))+
    theme(legend.position = "none")+
    geom_sf_label(aes(label = ratio_de_pob_despl),size = 6,  color = "#000000", inherit.aes = FALSE)
