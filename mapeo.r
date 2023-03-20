# Lista de paquetes que se necesitan
packages <- c("scales","viridis","leaflet","janitor","rlang", "httpgd", "sf", "tidyverse", "languageserver", "skimr", "viridis", "ggplot2", "mapsf", "cartography")

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

#convertimos a WGS84
#censales_shp <- st_transform(censales_shp, '+proj=longlat +datum=WGS84')



#filtro si CUSEC es igual a 1508201006, 1508201007 o 1508201008
os_tilos_sh <- censales_shp %>% 
    filter(CUSEC=="1508201006" | CUSEC=="1508201007" | CUSEC=="1508201008") %>% 
    st_union()

#filtro si CUSEC es igual a 1506001005, 1506001001  y llamarlo Sigueiro
sigueiro_sh <- censales_shp %>% 
    filter(CUSEC=="1506001005" | CUSEC=="1506001001") %>% 
    st_union()

#filtro si CUSEC es igual a 1500203005, 1500203006, 1500203008, 1500203009, 15002030013, 1500203014, 1500203016 para milladoiro
milladoiro_sh <- censales_shp %>%
    filter(CUSEC=="1500203005" | CUSEC=="1500203006" | CUSEC=="1500203008" | CUSEC=="1500203009" | CUSEC=="1500203013" | CUSEC=="1500203014" | CUSEC=="1500203016") %>% 
    st_union()

#filtro si cusec es igual a 1501302002, 1500203015, 1500203010, 1500203011, 1500203012, 1500203007, 15002030062
brion_bertamirans_sh <- censales_shp %>%
    filter(CUSEC=="1501302002" | CUSEC=="1500203015" | CUSEC=="1500203010" | CUSEC=="1500203011" | CUSEC=="1500203012" | CUSEC=="1500203007" | CUSEC=="15002030062") %>% 
    st_union()

#crear un objeto sf con los distritos de Santiago
distrito1 <- censales_shp %>%
  filter(grepl("^Santiago de Compostela", NMUN) & grepl("^01$", CDIS)) %>% 
  st_union()

distrito2 <- censales_shp %>%
  filter(grepl("^Santiago de Compostela", NMUN) & grepl("^02$", CDIS)) %>% 
  st_union()

distrito3 <- censales_shp %>%
  filter(grepl("^Santiago de Compostela", NMUN) & grepl("^03$", CDIS)) %>% 
  st_union()

distrito4 <- censales_shp %>%
  filter(grepl("^Santiago de Compostela", NMUN) & grepl("^04$", CDIS)) %>% 
  st_union()

distrito5 <- censales_shp %>%
  filter(grepl("^Santiago de Compostela", NMUN) & grepl("^05$", CDIS)) %>% 
  st_union()

distrito6 <- censales_shp %>%
  filter(grepl("^Santiago de Compostela", NMUN) & grepl("^06$", CDIS)) %>% 
  st_union()



# Extraemos las columnas de geometría de cada objeto sf
geom_distrito1 <- st_geometry(distrito1, crs=25830)
geom_distrito2 <- st_geometry(distrito2, crs=25830)
geom_distrito3 <- st_geometry(distrito3, crs=25830)
geom_distrito4 <- st_geometry(distrito4, crs=25830)
geom_distrito5 <- st_geometry(distrito5, crs=25830)
geom_distrito6 <- st_geometry(distrito6, crs=25830)
geom_os_tilos <- st_geometry(os_tilos_sh, crs=25830)
geom_sigueiro <- st_geometry(sigueiro_sh, crs=25830)
geom_milladoiro <- st_geometry(milladoiro_sh, crs=25830)
geom_brion_bertamirans <- st_geometry(brion_bertamirans_sh, crs=25830)



# Unimos las columnas de geometría
geoms <- do.call("rbind", list(geom_distrito1, geom_distrito2, geom_distrito3, geom_distrito4, geom_distrito5, geom_distrito6, geom_os_tilos, geom_sigueiro, geom_milladoiro, geom_brion_bertamirans))

# Creamos un nuevo objeto sf con las columnas de geometría unidas
distritos_union <- st_as_sf(data.frame(geometria = geoms),crs=25830)


# Creamos un vector con el número de distrito para cada geometría
num_distritos <- rep(c(1:10), each = length(geoms)/10)

# Creamos un nuevo objeto sf con las columnas de geometría y número de distrito unidas
distritos_union <- st_as_sf(data.frame(geometria = geoms, distrito = num_distritos), crs=25830)



# Creamos un vector con el nombre de cada distrito
zonas <- c("distrito1", "distrito2", "distrito3", "distrito4", "distrito5", "distrito6", "os_tilos", "sigueiro", "milladoiro", "brion_bertamirans")

#crear nueva columna en distritos_unión con el nombre de la zona    
distritos_union$zona <- zonas

# Verificamos que el objeto sf tiene la columna distrito
head(distritos_union)



#importar datos de población
poblacion <- read.csv("pop.csv", header = TRUE, sep = ";", dec = ",")


#limpiar nombres de columnas en poblacion con janitor
poblacion <- poblacion %>% clean_names()

str(poblacion)


#unir distritos_union con poblacion
prueba <- merge(distritos_union, poblacion, by = "zona", all.x = TRUE) 





#crear mapa de densidad de población
prueba %>% 
    ggplot() +
    geom_sf(aes(fill = x_de_movilidad_activa_como_medio_principal)) +
    theme_void()+
    theme(legend.position = "bottom")+
    scale_fill_viridis(option = "mako", name = "Movilidad activa", labels=percent)+
    theme(plot.title = element_text(size = 30,hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 20,hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size= 20))+
          guides(fill = guide_colorbar(barwidth = 2, barheight = 15, title.position = "top"))+
    #texto de la leyenda más grnde  
    theme(text = element_text(size = 24)) +
    #leyenda en la derecha
    theme(legend.position = "right")
#mover leyenda
#theme(legend.position = c(0.985, 0.35))+
          #+geom_sf_label(aes(label = paste0(zona,": ", round(x_de_vehiculo_privado_como_medio_principal, 2))),
#añadir etiquetas con porcentaje
          #+geom_sf_label(aes(label = paste0(zona,": ", round(x_de_movilidad_activa_como_medio_principal * 100, 2), "%")),size = 6,  color = "black", inherit.aes = FALSE)


prueba %>% 
    ggplot() +
    geom_sf(aes(fill = x_de_vehiculo_privado_como_medio_principal)) +
    theme_void()+
    theme(legend.position = "bottom")+
    scale_fill_viridis(option = "mako", name = "Vehiculo privado",  labels=percent)+
    theme(plot.title = element_text(size = 30,hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 20,hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size= 20))+
          guides(fill = guide_colorbar(barwidth = 2, barheight = 15, title.position = "top"))+
    #texto de la leyenda más grnde  
    theme(text = element_text(size = 24)) +
    #leyenda en la derecha
    theme(legend.position = "right")
#mover leyenda
#theme(legend.position = c(0.985, 0.35))+
          #+geom_sf_label(aes(label = paste0(zona,": ", round(x_de_vehiculo_privado_como_medio_principal, 2))),
#añadir etiquetas con porcentaje
          #+geom_sf_label(aes(label = paste0(zona,": ", round(x_de_vehiculo_privado_como_medio_principal * 100, 2), "%")),size = 7,  color = "black", inherit.aes = FALSE)



prueba %>% 
    ggplot() +
    geom_sf(aes(fill = x_de_transporte_publico_como_medio_principal)) +
    theme_void()+
    theme(legend.position = "bottom")+
    scale_fill_viridis(option = "mako", name = "Transporte público",  labels=percent)+
    theme(plot.title = element_text(size = 30,hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 20,hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size= 20))+
          guides(fill = guide_colorbar(barwidth = 2, barheight = 15, title.position = "top"))+
    #texto de la leyenda más grnde  
    theme(text = element_text(size = 24)) +
    #leyenda en la derecha
    theme(legend.position = "right")
#mover leyenda
#theme(legend.position = c(0.985, 0.35))+
          #+geom_sf_label(aes(label = paste0(zona,": ", round(x_de_vehiculo_privado_como_medio_principal, 2))),
#añadir etiquetas con porcentaje
          #+geom_sf_label(aes(label = paste0(zona,": ", round(x_de_transporte_publico_como_medio_principal * 100, 2), "%")),size = 7,  color = "black", inherit.aes = FALSE)
#mapear los datos de población en el datafreme  "prueba" con  el paquete leaflet


library(tmap)
  tmap_mode("view")

tm_shape(prueba) +
    tm_polygons("zona", 
    alpha = 0.5,
    palette = hcl.colors(10, palette = "Dark 3"),
                    
                    # border definition: color and transparency
                    border.col = "#990099",
                    border.alpha = 0.1
        )

tmap_last() %>% 
    tmap_save("Santiago_map.html")
