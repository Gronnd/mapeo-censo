# ==== 0. CABECERA ====
# =============================================================================
# mapeo_censo.R
# Mapas coropléticos de movilidad sobre las secciones censales del INE (2021).
# Sustituye a mapeo.r, mapeo_municipios.r, mapeo_zonas.r, mapeo_optimizado.r
# y mapeo_optimizado_vigo.r (ver carpeta legacy/).
#
# Uso:
#   Rscript mapeo_censo.R               # usa CONFIG$ciudad
#   Rscript mapeo_censo.R vigo          # santiago | municipios | vigo
#   En RStudio: editar la sección 2 y pulsar "Source".
# Entradas: Seccionado_2021.zip (INE), pop.csv / mun.csv / vigo.csv (sep ";", dec ",")
# Salidas:  <dir_salida>/<ciudad>_<id>.png, opcionalmente santiago.shp y *_map.html
# =============================================================================

# ==== 1. LIMPIEZA DEL ENTORNO ====
# Se hace ANTES de cargar nada para no borrar objetos del propio script.
rm(list = ls())

# ==== 2. CONFIGURACIÓN ====
# Lista con todos los parámetros editables. Se puede cambiar aquí o por CLI.
CONFIG <- list(
  ciudad = "santiago",   # "santiago", "municipios" o "vigo"
  ruta_zip = "Seccionado_2021.zip",
  rutas_csv = list(santiago = "pop.csv", municipios = "mun.csv", vigo = "vigo.csv"),
  csv_encoding = "UTF-8",   # "latin1" si el CSV viene de Excel
  dir_salida = "salida",
  mapas = NULL,   # NULL = todos; o p.ej. c("media_desplazamientos", "uso_autobus")
  guardar_shp = FALSE,   # escribe <dir_salida>/santiago.shp (solo santiago)
  mapa_html = FALSE,   # mapa interactivo con tmap (opcional)
  variable_html = "media_de_desplazamientos",
  instalar_paquetes = TRUE,
  mostrar_en_pantalla = TRUE,   # print() solo en sesión interactiva
  ancho = 10, alto = 10, dpi = 300
)
# Si se ejecuta con Rscript, el primer argumento sobrescribe la ciudad.
args_cli <- commandArgs(trailingOnly = TRUE)
if (length(args_cli) >= 1) CONFIG$ciudad <- tolower(args_cli[1])

# ==== 3. PAQUETES ====
# Carga los paquetes necesarios; si faltan, los instala (si CONFIG lo permite).
cargar_paquetes <- function(paquetes, instalar = TRUE) {
  faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltan) > 0) {
    if (!instalar) stop("Faltan paquetes: ", paste(faltan, collapse = ", "),
                        ". Instálalos o pon CONFIG$instalar_paquetes <- TRUE.", call. = FALSE)
    install.packages(faltan, repos = "https://cloud.r-project.org")
  }
  for (p in paquetes) suppressPackageStartupMessages(library(p, character.only = TRUE))
  invisible(TRUE)
}
cargar_paquetes(c("sf", "dplyr", "ggplot2", "janitor", "scales"), CONFIG$instalar_paquetes)
# tmap NO se carga aquí: solo se usa con tmap:: si CONFIG$mapa_html es TRUE.
# Nota: linewidth en geom_sf requiere ggplot2 >= 3.4.

# ==== 4. CONSTANTES ====
# Paletas de colores disponibles para los mapas coropléticos (inicio y fin del degradado).
PALETAS <- list(
  azul = c("#DEEBF7", "#08306B"),
  rosa = c("#F2769534", "#CC004E"),
  rojo = c("#FFD1CE", "#FF2E29")
)
# Códigos INE de los municipios del área de estudio y su nombre.
CODIGOS_MUNICIPIOS <- c("15078" = "Santiago de Compostela", "15013" = "Brión",
                        "15002" = "Ames", "15060" = "Oroso", "15082" = "Teo")
# Zonas de Santiago agrupadas a mano (además de los distritos).
# El nombre "Os tilos" (t minúscula) es la clave de unión con pop.csv: no cambiar.
ZONAS_SANTIAGO_EXTRA <- list(
  "Os tilos"   = c("1508201006", "1508201007", "1508201008"),
  "Sigüeiro"   = c("1506001005", "1506001001"),
  "Milladoiro" = c("1500203005", "1500203006", "1500203008", "1500203009",
                   "1500203013", "1500203014", "1500203016"),
  "Brión e Bertamiráns" = c("1501302002", "1500203015", "1500203010", "1500203011",
                            "1500203012", "1500203007",
                            "15002030062")  # REVISAR: 11 dígitos, código probablemente erróneo
)
# Catálogo de mapas: una fila por mapa y ciudad.
# tipo: "porcentaje" | "numero"; etiqueta: "ninguna" | "valor" | "nombre_valor" | "nombre"
# leyenda: "ninguna" | "dentro" | "derecha" | "abajo"; paleta: "azul" | "rosa" | "rojo"
CATALOGO_MAPAS <- dplyr::tribble(
  ~ciudad,      ~id,                    ~variable,                                    ~titulo,                                  ~tipo,        ~decimales, ~etiqueta,     ~leyenda,  ~paleta,
  "santiago",   "movilidad_activa",     "x_de_movilidad_activa_como_medio_principal", "Movilidad activa",                       "porcentaje", 1,          "ninguna",    "dentro",  "azul",
  "santiago",   "vehiculo_privado",     "x_de_vehiculo_privado_como_medio_principal", "Vehículo privado",                       "porcentaje", 1,          "ninguna",    "dentro",  "azul",
  "santiago",   "transporte_publico",   "x_de_transporte_publico_como_medio_principal", "Transporte público",                   "porcentaje", 1,          "ninguna",    "dentro",  "azul",
  "santiago",   "media_desplazamientos", "media_de_desplazamientos",                  "Media de desplazamientos",               "numero",     2,          "nombre_valor", "ninguna", "rosa",
  "santiago",   "duracion_media",       "duracion_media_de_desplazamiento",           "Duración media del desplazamiento",      "numero",     1,          "valor",      "ninguna", "azul",
  "santiago",   "desplazamientos_total", "x_desplazamientos_sobre_total",             "Desplazamientos sobre el total",         "porcentaje", 2,          "valor",      "ninguna", "azul",
  "santiago",   "ratio_pob_despl",      "ratio_de_pob_despl",                         "Ratio población / desplazamientos",      "numero",     2,          "valor",      "ninguna", "azul",
  "municipios", "movilidad_activa",     "x_de_movilidad_activa_como_medio_principal", "Movilidad activa",                       "porcentaje", 1,          "ninguna",    "dentro",  "azul",
  "municipios", "vehiculo_privado",     "x_de_vehiculo_privado_como_medio_principal", "Vehículo privado",                       "porcentaje", 1,          "ninguna",    "dentro",  "azul",
  "municipios", "transporte_publico",   "x_de_transporte_publico_como_medio_principal", "Transporte público",                   "porcentaje", 1,          "ninguna",    "dentro",  "azul",
  "municipios", "media_desplazamientos", "media_de_desplazamientos",                  "Media de desplazamientos",               "numero",     2,          "nombre_valor", "ninguna", "rosa",
  "municipios", "duracion_media",       "duracion_media_de_desplazamiento",           "Duración media del desplazamiento",      "numero",     1,          "valor",      "ninguna", "azul",
  "municipios", "desplazamientos_total", "x_desplazamientos_sobre_total",             "Desplazamientos sobre el total",         "porcentaje", 2,          "valor",      "ninguna", "azul",
  "municipios", "ratio_pob_despl",      "ratio_de_pob_despl",                         "Ratio población / desplazamientos",      "numero",     2,          "valor",      "ninguna", "azul",
  "vigo",       "uso_autobus",          "uso_autobus",                                "Uso del autobús",                        "porcentaje", 1,          "valor",      "derecha", "rojo",
  "vigo",       "desplazamientos_total", "x_desplazamientos_sobre_total",             "Desplazamientos sobre el total",         "porcentaje", 2,          "valor",      "ninguna", "azul",
  "vigo",       "ratio_pob_despl",      "ratio_de_pob_despl",                         "Ratio población / desplazamientos",      "numero",     2,          "valor",      "ninguna", "azul",
  "vigo",       "duracion_media",       "duracion_media_de_desplazamiento",           "Duración media del desplazamiento",      "numero",     1,          "valor",      "ninguna", "azul",
  "vigo",       "media_desplazamientos", "media_de_desplazamientos",                  "Media de desplazamientos",               "numero",     2,          "nombre_valor", "ninguna", "rosa",
  "vigo",       "movilidad_activa",     "x_de_movilidad_activa_como_medio_principal", "Movilidad activa",                       "porcentaje", 1,          "ninguna",    "dentro",  "rosa"
)

# ==== 5. UTILIDADES DE LECTURA ====
# Verifica que existe un fichero; si no, para con un mensaje claro indicando la ruta.
comprobar_fichero <- function(ruta, descripcion) {
  if (is.null(ruta) || !file.exists(ruta)) {
    stop(sprintf(paste0("No se encuentra %s en '%s' (directorio de trabajo: '%s'). ",
                        "Revisa la ruta en la sección 2 (CONFIG)."),
                 descripcion, ruta, getwd()), call. = FALSE)
  }
  invisible(ruta)
}
# Descomprime el zip en un directorio temporal, lee el .shp de secciones y lo devuelve como sf.
# El directorio temporal se borra al terminar la función.
leer_shapefile_zip <- function(ruta_zip) {
  comprobar_fichero(ruta_zip, "el shapefile de secciones censales del INE (zip)")
  dir_tmp <- tempfile("seccionado_")
  dir.create(dir_tmp)
  on.exit(unlink(dir_tmp, recursive = TRUE), add = TRUE)
  utils::unzip(ruta_zip, exdir = dir_tmp)
  shp <- list.files(dir_tmp, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  if (length(shp) == 0) stop("El zip '", ruta_zip, "' no contiene ningún fichero .shp.", call. = FALSE)
  if (length(shp) > 1) warning("El zip contiene varios .shp; se usa: ", basename(shp[1]))
  capas <- sf::read_sf(shp[1])
  message("Shapefile leído: ", nrow(capas), " secciones. CRS: ", sf::st_crs(capas)$input)
  capas
}
# Lee un CSV con separador ";" y decimal ","; los nombres de columna se normalizan con janitor.
# No usar check.names = FALSE: los nombres x_de_... dependen de make.names() + clean_names().
leer_csv_movilidad <- function(ruta, encoding = "UTF-8") {
  comprobar_fichero(ruta, "el CSV de datos de movilidad")
  read.csv(ruta, header = TRUE, sep = ";", dec = ",", fileEncoding = encoding) %>%
    janitor::clean_names()
}
# Verifica que un data frame contiene las columnas indicadas; si falta alguna, para con error.
validar_columnas <- function(df, columnas, nombre) {
  faltan <- setdiff(columnas, names(df))
  if (length(faltan) > 0) stop(sprintf("A '%s' le faltan columnas: %s. Disponibles: %s",
                                       nombre, paste(faltan, collapse = ", "),
                                       paste(names(df), collapse = ", ")), call. = FALSE)
  invisible(TRUE)
}
# Une la geometría con los datos por una clave común (left join), avisando de los no coincidentes.
# La clave se pasa a texto en ambos lados para evitar fallos de tipo numérico/carácter.
unir_datos <- function(geo, datos, clave) {
  geo[[clave]] <- as.character(geo[[clave]])
  datos[[clave]] <- as.character(datos[[clave]])
  sin_datos <- setdiff(geo[[clave]], datos[[clave]])
  sin_geo   <- setdiff(datos[[clave]], geo[[clave]])
  if (length(sin_datos)) warning("Sin datos en el CSV para: ", paste(sin_datos, collapse = ", "))
  if (length(sin_geo))   warning("Filas del CSV sin geometría: ", paste(sin_geo, collapse = ", "))
  dplyr::left_join(geo, datos, by = clave)
}

# ==== 6. GEOMETRÍAS ====
# Une las geometrías de las secciones indicadas en una sola zona.
# Avisa de los CUSEC que no existen en el shapefile y para si no queda ninguna sección.
filter_and_union <- function(shp, cusec, nombre = "") {
  no_encontrados <- setdiff(cusec, shp$CUSEC)
  if (length(no_encontrados) > 0)
    warning(sprintf("Zona '%s': CUSEC no encontrados: %s", nombre, paste(no_encontrados, collapse = ", ")))
  sel <- dplyr::filter(shp, CUSEC %in% cusec)
  if (nrow(sel) == 0) stop(sprintf("Zona '%s': ninguna sección encontrada.", nombre), call. = FALSE)
  sf::st_union(sel)
}
# Define las zonas de Santiago: los 6 distritos (por CDIS) más las zonas extra de la sección 4.
# Devuelve una lista nombrada de vectores de CUSEC.
definir_zonas_santiago <- function(censales) {
  stgo <- dplyr::filter(censales, startsWith(NMUN, "Santiago de Compostela"))
  distritos <- lapply(1:6, function(d) stgo$CUSEC[stgo$CDIS == sprintf("%02d", d)])
  names(distritos) <- paste("Distrito", 1:6)
  c(distritos, ZONAS_SANTIAGO_EXTRA)
}
# Construye un sf con una fila por zona (nombre, orden y geometría unida).
# Las zonas se generan en el orden de la lista de entrada.
construir_zonas <- function(censales, zonas) {
  geoms <- Map(function(cs, nm) filter_and_union(censales, cs, nm), zonas, names(zonas))
  sf::st_sf(zona = names(zonas), orden = seq_along(zonas), geometry = do.call(c, unname(geoms)))
}
# Prepara las zonas de Santiago y une los datos de pop.csv.
# Devuelve list(sf = <sf con datos>, col_nombre = <columna para etiquetas>).
preparar_santiago <- function(censales, config) {
  geo <- construir_zonas(censales, definir_zonas_santiago(censales))
  if (isTRUE(config$guardar_shp)) {
    dir.create(config$dir_salida, showWarnings = FALSE, recursive = TRUE)
    ruta <- file.path(config$dir_salida, "santiago.shp")
    sf::st_write(geo, ruta, delete_dsn = TRUE, quiet = TRUE, layer_options = "ENCODING=UTF-8")
    message("Guardado: ", ruta)
  }
  pop <- leer_csv_movilidad(config$rutas_csv$santiago, config$csv_encoding)
  validar_columnas(pop, "zona", config$rutas_csv$santiago)
  datos <- unir_datos(geo, pop, "zona") %>% dplyr::arrange(orden) %>% dplyr::select(-orden)
  list(sf = datos, col_nombre = "zona")
}
# Une las secciones de cada municipio de CODIGOS_MUNICIPIOS y, si existe mun.csv, sus datos.
# Si no hay CSV, devuelve solo la geometría (se genera el mapa base).
# Devuelve list(sf = <sf>, col_nombre = "nombre").
preparar_municipios <- function(censales, config) {
  geo <- censales %>%
    dplyr::filter(CUMUN %in% names(CODIGOS_MUNICIPIOS)) %>%
    dplyr::group_by(CUMUN) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
    dplyr::transmute(codigo = CUMUN, nombre = unname(CODIGOS_MUNICIPIOS[CUMUN]), geometry)
  if (nrow(geo) != length(CODIGOS_MUNICIPIOS)) warning("No se encontraron todos los municipios.")
  ruta_csv <- config$rutas_csv$municipios
  if (is.null(ruta_csv) || !file.exists(ruta_csv)) {
    message("No hay '", ruta_csv, "': solo se generará el mapa base de municipios.")
    return(list(sf = geo, col_nombre = "nombre"))
  }
  munc <- leer_csv_movilidad(ruta_csv, config$csv_encoding)
  validar_columnas(munc, "municipio", ruta_csv)
  munc$municipio <- as.character(munc$municipio)
  clave <- if (any(munc$municipio %in% geo$codigo)) "codigo" else "nombre"
  names(munc)[names(munc) == "municipio"] <- clave
  list(sf = unir_datos(geo, munc, clave), col_nombre = "nombre")
}
# Une las secciones de Vigo por distrito (CDIS) y los datos de vigo.csv.
# Devuelve list(sf = <sf con datos>, col_nombre = "cdis").
preparar_vigo <- function(censales, config) {
  geo <- censales %>%
    dplyr::filter(NMUN == "Vigo") %>%
    dplyr::group_by(CDIS) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
    dplyr::transmute(cdis = sprintf("Dist. %02d", as.integer(CDIS)), geometry)
  if (nrow(geo) == 0) stop("No hay secciones con NMUN == 'Vigo' en el shapefile.", call. = FALSE)
  vigo <- leer_csv_movilidad(config$rutas_csv$vigo, config$csv_encoding)
  validar_columnas(vigo, "cdis", config$rutas_csv$vigo)
  datos <- unir_datos(geo, vigo, "cdis") %>% dplyr::arrange(cdis)
  list(sf = datos, col_nombre = "cdis")
}

# ==== 7. MAPAS ====
# Crea la función de formato de etiquetas (porcentaje o número) con separador decimal español.
# Si los valores están en 0-1 se multiplican por 100 antes de mostrarlos como porcentaje.
crear_formateador <- function(tipo, decimales, valores) {
  precision <- 10^(-decimales)
  if (tipo == "porcentaje") {
    maximo <- suppressWarnings(max(abs(valores), na.rm = TRUE))
    escala <- if (is.finite(maximo) && maximo <= 1) 100 else 1
    scales::label_percent(accuracy = precision, scale = escala,
                          decimal.mark = ",", big.mark = ".", suffix = " %")
  } else {
    scales::label_number(accuracy = precision, decimal.mark = ",", big.mark = ".")
  }
}
# Devuelve el tema de ggplot2 que coloca la leyenda según la opción indicada.
# La posición "dentro" usa la API de inside() en ggplot2 >= 3.5.0.
tema_leyenda <- function(leyenda) {
  switch(leyenda,
    ninguna = theme(legend.position = "none"),
    derecha = theme(legend.position = "right"),
    abajo   = theme(legend.position = "bottom"),
    dentro  = if (utils::packageVersion("ggplot2") >= "3.5.0")
                theme(legend.position = "inside", legend.position.inside = c(0.89, 0.18))
              else theme(legend.position = c(0.89, 0.18)),
    stop("Leyenda no válida: ", leyenda, call. = FALSE))
}
# Dibuja un mapa coroplético de una variable con leyenda, etiquetas opcionales y paleta elegida.
# Devuelve un objeto ggplot (no lo guarda en disco).
mapa_coropletico <- function(datos, variable, titulo,
                             tipo = c("numero", "porcentaje"), decimales = 2,
                             etiqueta = c("ninguna", "valor", "nombre_valor", "nombre"),
                             col_nombre = NULL, paleta = "azul",
                             leyenda = c("ninguna", "dentro", "derecha", "abajo"),
                             tam_texto = 20, tam_etiqueta = 6) {
  tipo <- match.arg(tipo); etiqueta <- match.arg(etiqueta); leyenda <- match.arg(leyenda)
  validar_columnas(datos, variable, "los datos del mapa")
  if (!is.numeric(datos[[variable]])) stop("La variable '", variable, "' no es numérica.", call. = FALSE)
  if (etiqueta %in% c("nombre", "nombre_valor")) validar_columnas(datos, col_nombre, "los datos del mapa")
  colores <- PALETAS[[paleta]]
  if (is.null(colores)) stop("Paleta desconocida: ", paleta, call. = FALSE)
  fmt <- crear_formateador(tipo, decimales, datos[[variable]])
  datos$.etiqueta <- switch(etiqueta,
    ninguna      = NA_character_,
    valor        = fmt(datos[[variable]]),
    nombre       = as.character(datos[[col_nombre]]),
    nombre_valor = paste0(datos[[col_nombre]], ": ", fmt(datos[[variable]])))
  p <- ggplot(datos) +
    geom_sf(aes(fill = .data[[variable]]), color = "black", linewidth = 0.6) +
    scale_fill_gradient(name = titulo, low = colores[1], high = colores[2],
                        labels = fmt, na.value = "grey85") +
    guides(fill = guide_colorbar(title.position = "top")) +
    theme_void(base_size = tam_texto) +
    theme(legend.key.width = grid::unit(0.8, "cm"), legend.key.height = grid::unit(3, "cm")) +
    tema_leyenda(leyenda)
  if (etiqueta != "ninguna")
    p <- p + geom_sf_label(aes(label = .etiqueta), size = tam_etiqueta, color = "black", na.rm = TRUE)
  p
}
# Dibuja el mapa base de un conjunto de polígonos, con una etiqueta por polígono.
# Se usa para el mapa de municipios sin datos de movilidad.
mapa_base <- function(datos, col_nombre, color = "#3B9ADF", tam_etiqueta = 6) {
  ggplot(datos) +
    geom_sf(color = "black", fill = color, alpha = 0.6) +
    geom_sf_label(aes(label = .data[[col_nombre]]), color = "black", size = tam_etiqueta) +
    theme_void() + theme(legend.position = "none")
}
# Guarda un gráfico ggplot como PNG en el directorio de salida; lo muestra solo en sesión interactiva.
# Devuelve la ruta del fichero generado de forma invisible.
guardar_mapa <- function(p, ciudad, id, config) {
  ruta <- file.path(config$dir_salida, paste0(ciudad, "_", id, ".png"))
  ggsave(ruta, p, width = config$ancho, height = config$alto, dpi = config$dpi, bg = "white")
  message("Guardado: ", ruta)
  if (isTRUE(config$mostrar_en_pantalla) && interactive()) print(p)
  invisible(ruta)
}
# Genera un mapa interactivo HTML con tmap para una variable. Si tmap no está instalado, avisa y sigue.
# Usa argumentos de tmap v3; tmap v4 los acepta con avisos de obsolescencia.
guardar_mapa_interactivo <- function(datos, variable, titulo, ruta) {
  if (!requireNamespace("tmap", quietly = TRUE)) {
    warning("tmap no está instalado: no se genera ", ruta, ". Instala con install.packages('tmap').")
    return(invisible(NULL))
  }
  if (!variable %in% names(datos)) { warning("No existe la variable '", variable, "' para el HTML."); return(invisible(NULL)) }
  tmap::tmap_mode("view")
  m <- tmap::tm_shape(datos) +
    tmap::tm_polygons(variable, title = titulo, alpha = 0.5,
                      palette = grDevices::hcl.colors(10, palette = "Mako"),
                      legend.reverse = TRUE, border.col = "#990099", border.alpha = 0.1)
  tmap::tmap_save(m, ruta)
  message("Guardado: ", ruta)
}
# Genera todos los mapas del catálogo de la ciudad (o solo los de config$mapas).
# Si una variable no existe en los datos, se omite ese mapa con un mensaje.
generar_mapas <- function(datos, col_nombre, ciudad, config) {
  filas <- CATALOGO_MAPAS[CATALOGO_MAPAS$ciudad == ciudad, ]
  if (!is.null(config$mapas)) filas <- filas[filas$id %in% config$mapas, ]
  if (nrow(filas) == 0) { message("No hay mapas que generar para '", ciudad, "'."); return(invisible(NULL)) }
  for (i in seq_len(nrow(filas))) {
    f <- filas[i, ]
    if (!f$variable %in% names(datos)) {
      message("Se omite '", f$id, "': la variable '", f$variable, "' no está en los datos.")
      next
    }
    p <- mapa_coropletico(datos, f$variable, f$titulo, tipo = f$tipo, decimales = f$decimales,
                          etiqueta = f$etiqueta, col_nombre = col_nombre,
                          paleta = f$paleta, leyenda = f$leyenda)
    guardar_mapa(p, ciudad, f$id, config)
  }
}

# ==== 8. EJECUCIÓN ====
# Flujo principal: valida la ciudad y los ficheros, prepara los datos y genera los mapas.
# Devuelve de forma invisible el sf con los datos de la ciudad.
main <- function(config) {
  ciudades <- c("santiago", "municipios", "vigo")
  if (!config$ciudad %in% ciudades)
    stop("Ciudad no válida: '", config$ciudad, "'. Usa: ", paste(ciudades, collapse = ", "), call. = FALSE)
  ciudad <- config$ciudad
  comprobar_fichero(config$ruta_zip, "el shapefile de secciones censales del INE (zip)")
  if (ciudad != "municipios")
    comprobar_fichero(config$rutas_csv[[ciudad]], paste0("el CSV de ", ciudad))
  dir.create(config$dir_salida, showWarnings = FALSE, recursive = TRUE)
  censales <- leer_shapefile_zip(config$ruta_zip)
  prep <- switch(ciudad,
    santiago   = preparar_santiago(censales, config),
    municipios = preparar_municipios(censales, config),
    vigo       = preparar_vigo(censales, config))
  rm(censales)
  if (ciudad == "municipios" && (is.null(config$mapas) || "base" %in% config$mapas))
    guardar_mapa(mapa_base(prep$sf, prep$col_nombre), ciudad, "base", config)
  generar_mapas(prep$sf, prep$col_nombre, ciudad, config)
  if (isTRUE(config$mapa_html)) {
    ruta_html <- file.path(config$dir_salida, paste0(tools::toTitleCase(ciudad), "_map.html"))
    # Título: se toma del catálogo la fila con la variable de config$variable_html; si no existe, se usa el nombre de la variable.
    fila_html <- CATALOGO_MAPAS[CATALOGO_MAPAS$ciudad == ciudad & CATALOGO_MAPAS$variable == config$variable_html, ]
    titulo_html <- if (nrow(fila_html) > 0) fila_html$titulo[1] else config$variable_html
    guardar_mapa_interactivo(prep$sf, config$variable_html, titulo_html, ruta_html)
  }
  message("Proceso terminado. Resultados en: ", normalizePath(config$dir_salida))
  invisible(prep$sf)
}
# La ejecución se omite si la variable de entorno MAPEO_SOLO_FUNCIONES vale "1" (para cargar solo las funciones).
if (!identical(Sys.getenv("MAPEO_SOLO_FUNCIONES"), "1")) {
  resultado <- main(CONFIG)
}
