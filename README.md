# Creación de mapas
Creación de mapas a partir de las secciones censales publicadas en el ine

## Uso

Todo el proceso está en un único script: `mapeo_censo.R`. Los scripts antiguos (`mapeo.r`, `mapeo_municipios.r`, `mapeo_zonas.r`, `mapeo_optimizado.r`, `mapeo_optimizado_vigo.r`) se conservan en `legacy/` solo como referencia.

1. Copia en la misma carpeta del script:
   - `Seccionado_2021.zip` (secciones censales del INE).
   - El CSV de datos según la ciudad: `pop.csv` (Santiago), `mun.csv` (municipios, opcional) o `vigo.csv` (Vigo). Separador `;` y decimal `,`.
2. Elige la ciudad de una de estas formas:
   - Editar `CONFIG` en la sección 2 y ejecutar el script desde RStudio.
   - Desde la terminal: `Rscript mapeo_censo.R santiago` (o `municipios`, `vigo`).
3. Los mapas se guardan en `salida/` como `<ciudad>_<mapa>.png`.
4. Opcional: `guardar_shp = TRUE` escribe `salida/santiago.shp` y `mapa_html = TRUE` genera un mapa interactivo HTML (requiere el paquete `tmap`).

Requisitos: R >= 4.0 con los paquetes `sf`, `dplyr`, `ggplot2` (>= 3.4), `janitor` y `scales`. `tmap` es opcional.
