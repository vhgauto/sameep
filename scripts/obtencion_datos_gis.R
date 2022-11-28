#!/usr/bin/env Rscript

# librerías
library(sf)
library(sen2r)
library(lubridate)
library(glue)
library(raster)
library(rgdal)
library(tidyverse)

# día de la fecha
hoy <- today() # ymd(20221127) # today() - 1
# 20221112: NUBE
# 20221122: sin NUBE

# sitio de interés
lr <- st_sfc(st_point(c(305789.86931, 6965069.94723)), crs = 32721)

# función para generar mensajes en la consola, para separar las secciones
f_msj <- function(x) {
    a <- nchar(x)
    b <- str_flatten(rep("X", a + 8))
    c <- str_flatten(c("X", rep(" ", a + 6), "X"))
    d <- str_flatten(c("X   ", x, "   X"))
    e <- glue("\n\n{b}\n{c}\n{d}\n{c}\n{b}\n\n")

    return(e)
}

# DESCARGA DE PRODUCTO

print(glue("{f_msj('DESCARGA DE PRODUCTO')}"))

# credenciales
write_scihub_login('vhgauto', '6EVmlMfiDlrA7VzVBAEU') 

# busco producto Sentinel-2 MSI L2A, tile 21JUK, vía 'scihub'
lis <- s2_list(spatial_extent = lr,
               time_interval = c(hoy, hoy),
               level = "L2A",
               tile = "21JUK",
               server = "scihub")

# condición de ERROR
if (length(lis) == 0) {
   stop(glue("'\n\n\nNo hay producto disponible para el {hoy}.\n\n\n'"))
}

print(glue("\n\nDescargando producto {names(lis)}\n\n"))

# creo la carpeta de descarga
dir.create("safe")

# descarga el producto
s2_download(lis, service = "apihub", overwrite = FALSE, outdir = "safe/")

print(glue("\n\nProducto descargado\n\n"))

# VERIFICACIÓN DE NUBES

print(glue("\n\nVerifico nubosidad en la región de interés\n\n"))

# raster con porcentaje de nubes
nube_porc <-
  list.files(file.path(list.files(
    file.path(getwd(), "safe",
              names(lis), "GRANULE"), full.names = TRUE
  ),
  "QI_DATA"),
  full.names = TRUE,
  pattern = "MSK_CLDPRB_20m")

# cargo el ráster con los porcetajes de nubes, por píxel
nube_raster <- raster(nube_porc)

# cargo el vector de puntos muestrales
print(glue("\n\nVector de puntos muestrales\n\n"))

puntos <- shapefile("vectores/linea_puntos_LT1.shp")

# valores de píxel, p/los 20 sitios muestrales
nube_pix <- raster::extract(nube_raster, puntos)

# condición de STOP
if (mean(nube_pix) != 0) {
  print(glue("{f_msj('PRESENCIA DE NUBES')}"))

  # elimino el SAFE
  print(glue("\n\nElimino recorte\n\n"))
  unlink(list.files(path = "safe", full.names = TRUE), recursive = TRUE)

  stop()
}

# ausencia de nubes (OK!)
if (mean(nube_pix) == 0) {
  print(glue("\n\nImagen sin nubes\n\n"))
}

# RECORTE DE PRODUCTO

# conviene recortar el producto previa extracción,
# para que el  resampling se lleve a cabo en una
# imagen de menor tamaño (y no en la imagen entera)
# (el resampling de toda la escena tarda UN MONTÓN)
print(glue("{f_msj('RECORTE DE PRODUCTO')}"))

# vector
print(glue("\n\nCargo vertor de la región de inter\u00E9s\n\n"))
vec <- shapefile("vectores/roi_LT.shp", verbose = FALSE)

# solo me interesan R10m y R20m (R60m NO!)
print(glue("\n\nCargo las bandas de inter\u00E9s\n\n"))

reso <- list.files(file.path(list.files(file.path(getwd(), "safe",
                   names(lis), "GRANULE"), full.names = TRUE), "IMG_DATA"),
                   full.names = TRUE)

# orden correcto de las bandas:
# B01, B02, B03, B04, B05, B06, B07, B08, B8A, B11, B12 [11 elementos]
lis_1020 <- c(
    list.files(reso[3], full.names = T)[2],    # B01
    list.files(reso[1], full.names = T)[2:4],  # B02, B03, B04
    list.files(reso[2], full.names = T)[6:8],  # B05, B06, B07
    list.files(reso[1], full.names = T)[5],    # B08
    list.files(reso[2], full.names = T)[11],   # B8A
    list.files(reso[2], full.names = T)[9:10]  # B11, B12
)

# lista que contiene las bandas ráster
print(glue("\n\nGenero los r\u00E1ster para cada banda\n\n"))
ras_1020 <- map(.x = lis_1020, raster)

# lista que contiene el subset según el ROI
print(glue("\n\nRecorto las bandas a la regi\u00F3n de inter\u00E9s\n\n"))
subset <- map(.x = ras_1020, ~ crop(.x, vec))

# resampling de los ráster a 10 m
print(glue("\n\nResampling de bandas a 10 m\n\n"))
# p/crear un stack que contenga todas las bandas,
# tengo que cambiar la resolución de las bandas
# bandas con 60m: B01 [1]
# bandas con 20m: B05 [5], B06 [6], B07 [7] , B8A [9], B11 [10], B12 [11]
# bandas con 10m: B02 [2], B03 [3], B04 [4], B08 [8]
subset_B02 <- subset[[2]]
subset_B01 <- resample(subset[[1]], subset[[2]], method = "ngb")
subset_B03 <- subset[[3]]
subset_B04 <- subset[[4]]
subset_B05 <- resample(subset[[5]], subset[[2]], method = "ngb")
subset_B06 <- resample(subset[[6]], subset[[2]], method = "ngb")
subset_B07 <- resample(subset[[7]], subset[[2]], method = "ngb")
subset_B08 <- subset[[8]]
subset_B8A <- resample(subset[[9]], subset[[2]], method = "ngb")
subset_B11 <- resample(subset[[10]], subset[[2]], method = "ngb")
subset_B12 <- resample(subset[[11]], subset[[2]], method = "ngb")

# creo el stack
subset_stack <-
    raster::stack(subset_B01, subset_B02, subset_B03,
                    subset_B04, subset_B05, subset_B06,
                    subset_B07, subset_B08, subset_B8A,
                    subset_B11, subset_B12)

# nombre correcto de bandas, en el orden adecuado
nombre_banda <- c("B01", "B02", "B03", "B04", "B05", "B06",
                  "B07", "B08", "B8A", "B11", "B12")

names(subset_stack) <- nombre_banda

# creo la carpeta del recorte
# creo la carpeta de descarga
dir.create("recortes")

# escribir el stack
print(glue("\n\nEscribo el stack de bandas recortado\n\n"))
writeRaster(subset_stack,
            filename = "recortes/recorte.tif",
            format = "GTiff",
            overwrite = TRUE)

# EXTRACCIÓN

print(glue("{f_msj('EXTRACCIÓN DE REFLECTANCIAS')}"))

# archivo stack
print(glue("\n\nLevanto el stack subset\n\n"))
ras1 <- "recortes/recorte.tif"

# levanto el stack
rast <- raster::stack(ras1)

# creo el data.frame con los datos de valor de pixel
# nombre de las filas del data.frame

# extraigo los valores de reflectancia de superficie del ráster
print(glue("\n\nExtraigo los valores de p\u00EDxel\n\n"))
base <- raster::extract(rast, puntos)

# convierto a data.frame y agrego columna con los 20 puntos
base2 <- base |>
  as_tibble() |>
  # agrego la fecha
  mutate(fecha = hoy) |>
  # agrego los nombres de bandas
  rename("B01" = 1, "B02" = 2, "B03" = 3, "B04" = 4, "B05" = 5, "B06" = 6,
         "B07" = 7, "B08" = 8, "B8A" = 9, "B11" = 10, "B12" = 11) |>
  # tabla larga
  pivot_longer(cols = -fecha,
               names_to = "banda",
               values_to = "reflec") |>
  # premedio diario, por banda
  group_by(fecha, banda) |>
  summarise(reflec = mean(reflec), .groups = "drop") |>
  # acomodo las bandas
  mutate(banda = factor(banda, levels = nombre_banda)) |>
  arrange(banda) |>
  # factor de escala
  mutate(reflec = reflec / 10000)

# escribo los datos nuevos
write_tsv(base2, file = "datos/datos_nuevos.tsv")

# leo base de datos ##########################################################
base_de_datos <- read_tsv("datos/base_de_datos_gis.tsv")

# combino con la base de datos
print(glue("\n\nIncorporo a la base de datos\n\n"))
base_de_datos2 <- bind_rows(base_de_datos, base2)

# sobreescrivo el archivo .tsv
write_tsv(base_de_datos2, file = "datos/base_de_datos_gis.tsv")

# muestro la tabla en la consola
base2

# elimino el SAFE y el recorte
print(glue("\n\nElimino rasters\n\n"))
unlink(list.files(path = "safe", full.names = TRUE), recursive = TRUE)
unlink(list.files(path = "recortes", full.names = TRUE), recursive = TRUE)

# FIN DEL PROCESO

print(glue("{f_msj('PROCESO FINALIZADO')}"))
