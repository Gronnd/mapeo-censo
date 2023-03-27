

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
