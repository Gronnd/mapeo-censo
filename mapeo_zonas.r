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

#importar datos de población
poblacion <- read.csv("pop.csv", header = TRUE, sep = ";", dec = ",")

#limpiar nombres de columnas en poblacion con janitor
poblacion <- poblacion %>% clean_names()

str(poblacion)

#leer santigo.shp
santiago <- st_read("santiago.shp")

#unir distritos_union con poblacion
santiago_datos <- merge(santiago, poblacion, by = "zona", all.x = TRUE) 

#reordenar el dataframe prueba en base a la variable distrito
santiago_datos <- santiago_datos   %>% 
    arrange(distrito)

#borrar variable distrito
santiago_datos <- santiago_datos[,-2]

str(santiago_datos)

options("OutDec" = ",")

#mapa con la duración media de los desplazamientos
santiago_datos %>% 
    ggplot() +
    geom_sf(aes(fill = duracion_media_de_desplazamiento), color = "black", linewidth = .6) +
    theme_void()+
    theme(legend.position = "bottom")+
    scale_fill_continuous(name = "Media de desplazamientos", labels=percent, trans = 'reverse',)+
    theme(legend.text = element_text(size = 20),
          legend.title = element_text(size= 20))+
          guides(fill = guide_colorbar(barwidth = 2, barheight = 15, title.position = "top"))+
    theme(text = element_text(size = 24))+
    theme(legend.position = "none")+
#añadir etiquetas con decimales
          geom_sf_label(aes(label = paste0(format(duracion_media_de_desplazamiento,decimal.mark = getOption("OutDec")))), 
          size = 6,  
          color = "#000000", 
          hjust =0.35)

#crear mapas de movilidad
santiago_datos %>%
    ggplot() +
    geom_sf(aes(fill = x_de_movilidad_activa_como_medio_principal), color = "black", linewidth = .6) +
    theme_void()+
    theme(legend.position = "bottom")+
    scale_fill_continuous(name = "Movilidad activa",  labels=percent, trans = 'reverse')+
    theme(legend.text = element_text(size = 20),
          legend.title = element_text(size= 20))+
          guides(fill = guide_colorbar(barwidth = 2, barheight = 10, title.position = "top"))+
    theme(text = element_text(size = 24))+
     theme(legend.position = c(.89, 0.18))

santiago_datos %>% 
    ggplot() +
    geom_sf(aes(fill = x_de_vehiculo_privado_como_medio_principal), color = "black", linewidth = .6) +
    theme_void()+
    theme(legend.position = "bottom")+
    scale_fill_continuous(name = "Vehiculo privado",  labels=percent, trans = 'reverse')+
    theme(legend.text = element_text(size = 20),
          legend.title = element_text(size= 20))+
          guides(fill = guide_colorbar(barwidth = 2, barheight = 10, title.position = "top"))+
    theme(text = element_text(size = 24))+
     theme(legend.position = c(.89, 0.18))

santiago_datos %>% 
    ggplot() +
    geom_sf(aes(fill = x_de_transporte_publico_como_medio_principal), color = "black", linewidth = .6) +
    theme_void()+
    theme(legend.position = "bottom")+
    scale_fill_continuous(name = "Transporte público",  labels=percent, trans = 'reverse')+
    theme(legend.text = element_text(size = 20),
          legend.title = element_text(size= 20))+
          guides(fill = guide_colorbar(barwidth = 2, barheight = 10, title.position = "top"))+
    theme(text = element_text(size = 24)) +
     theme(legend.position = c(.89, 0.18))

#crear mapa de media de desplazamientos
santiago_datos %>% 
    ggplot() +
    geom_sf(aes(fill = media_de_desplazamientos), color = "black", linewidth = .6) +
    theme_void()+
    theme(legend.position = "bottom")+
    scale_fill_continuous(name = "Media de desplazamientos", labels=percent, trans = 'reverse', limits=c(3.4,2.2)  )+
    theme(legend.text = element_text(size = 20),
          legend.title = element_text(size= 20))+
          guides(fill = guide_colorbar(barwidth = 2, barheight = 15, title.position = "top"))+
    theme(text = element_text(size = 24))+
    theme(legend.position = "none")+
#añadir etiquetas con decimales
          geom_sf_label(aes(label = paste0(zona,": ", format((round(media_de_desplazamientos, 2)), decimal.mark = getOption("OutDec")))), size = 6,  color = "#000000", inherit.aes = FALSE)
 #añadir etiquetas con porcentaje
          #geom_sf_label(aes(label = paste0(zona,": ", round(x_de_movilidad_activa_como_medio_principal * 100, 2), "%")),size = 6,  color = "black", inherit.aes = FALSE)
