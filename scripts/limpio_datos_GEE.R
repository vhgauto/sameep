library(lubridate)
library(tidyverse)

# los datos de reflectancia fueron obtenidos a partir de un script en GEE
# https://code.earthengine.google.com/?scriptPath=users%2Fvhgauto%2FGISTAQ%3Asameep_historico

# nubes
# https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_S2_SR#bands
# QA60 == 1024 -> Opaque clouds
# QA60 == 2048 -> Cirrus clouds
# QA60 == 0 -> No opaque clouds

# leo los datos descargados de GEE
gee_crudo <- read_csv("datos/ee-chart.csv")

# acomodo los datos
gee_crudo |>
    # acomodo la fecha
    rename(fecha = 1) |>
    mutate(fecha = mdy(fecha))  |>
    # aplico el factor de escala (1/100000)
    mutate(across(.cols = starts_with("B"), ~ ./10000)) |>
    # tabla larga con las bandas
    pivot_longer(cols = starts_with("B"),
                 names_to = "banda",
                 values_to = "reflec") |>
    # elimino fechas con nubes: QA60 == 1024 & QA60 == 2048
    filter(QA60 == 0) |>
    select(-QA60) |> 
    # arreglo los nombres de bandas
    mutate(banda = case_when(banda == "B1" ~ "B01",
                             banda == "B2" ~ "B02",
                             banda == "B3" ~ "B03",
                             banda == "B4" ~ "B04",
                             banda == "B5" ~ "B05",
                             banda == "B6" ~ "B06",
                             banda == "B7" ~ "B07",
                             banda == "B8" ~ "B08",
                             TRUE ~ banda)) |>
    mutate(banda = factor(banda, levels = c("B01", "B02", "B03", "B04", "B05",
                               "B06", "B07", "B08", "B8A", "B11", "B12"))) |>
    # guardo la base de datos GEE
    write_tsv("datos/reflec_gee.tsv")
