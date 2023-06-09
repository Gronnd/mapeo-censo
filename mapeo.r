# Lista de paquetes que se necesitan
packages <- c("scales","viridis","leaflet","janitor","rlang", "httpgd", "sf", "tidyverse", "languageserver", "skimr", "ggplot2", "mapsf", "cartography")

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

#convertimos a WGS84, opcional
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
zonas <- c("Distrito 1", "Distrito 2", "Distrito 3", "Distrito 4", "Distrito 5", "Distrito 6", "Os tilos", "Sigüeiro", "Milladoiro", "Brión e Bertamiráns")

#crear nueva columna en distritos_unión con el nombre de la zona    
distritos_union$zona <- zonas

# Verificamos que el objeto sf tiene la columna distrito
head(distritos_union)



#guardar  distritos_union en un shapefile
st_write(distritos_union, "santiago.shp", driver = "ESRI Shapefile", overwrite_layer = TRUE)



#importar datos de población
poblacion <- read.csv("pop.csv", header = TRUE, sep = ";", dec = ",")


#limpiar nombres de columnas en poblacion con janitor
poblacion <- poblacion %>% clean_names()

str(poblacion)



#leer santigo.shp
santiago <- st_read("santiago.shp")


#unir distritos_union con poblacion
distritos_datos <- merge(distritos_union, poblacion, by = "zona", all.x = TRUE, sep = ";", dec = ",") 



#reordenar el dataframe prueba en base a la variable distrito
santiago_datos <- santiago_datos   %>% 
    arrange(distrito)

#borrar variable distrito
santiago_datos <- santiago_datos[,-2]

str(santiago_datos)


#crear mapa de densidad de población
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

    
options("OutDec" = ",")


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



#mapa de Santiago con tmap con limites de los distritos

  tmap_mode("view")

tm_shape(prueba) +
    tm_polygons("media_de_desplazamientos",
    legend.reverse = TRUE,
    title="Media de desplazamientos",
    alpha = 0.5,
    palette = hcl.colors(10, palette = "Mako"),
                    # border definition: color and transparency
                    border.col = "#990099",
                    border.alpha = 0.1
        )

tm_shape(santiago_datos)+
    tm_polygons("media_de_desplazamientos",
    legend.reverse = TRUE,
    title="Media de desplazamientos",
    alpha = 0.5,
    palette = hcl.colors(10, palette = "Mako"),
                    # border definition: color and transparency
                    border.col = "#990099",
                    border.alpha = 0.1
        )

#guardar mapa en html
tmap_last() %>% 
    tmap_save("Santiago_map.html")
