library(corrr)
library(lubridate)
library(tidyverse)

# datos SAMEEP, solo me interesa turbidez (NTU)
sameep <- read_tsv("datos/sameep_historicos.tsv") |>
    filter(param == "turbidez") |>
    select(-param, fecha, turb = valor)

# datos GIS, obtenidos de GEE
# https://code.earthengine.google.com/?scriptPath=users%2Fvhgauto%2FGISTAQ%3Asameep_historico
gee <- read_tsv("datos/reflec_gee.tsv")

# combino los datos
datos <- inner_join(sameep, gee, by = "fecha") |>
    pivot_wider(names_from = banda,
                values_from = reflec) |>
    select(-fecha)

# verifico outliers en turb
datos |>
    filter(turb < 500) |>
    ggplot(aes(turb)) +
    geom_boxplot()

# coeficientes de correlación
datos |>
    # filter(turb < 500) |>
    correlate() |>
    shave() |>
    fashion(na_print = ".") # B04, B05

datos2 <- datos |>
    mutate(aa = B04^2 + B04)

# acuaciones lineales
mod1 <- lm(formula = turb ~ B04+B05+B12, data = datos)
sum1 <- summary(mod1)
sum1

tibble(turb = datos$turb,
       pred = predict(mod1)) |>
       ggplot(aes(x = pred, y = turb)) +
       geom_point(size = 2, alpha = .5) +
       geom_abline() +
       coord_fixed()

# cuadráticas
mod2 <- lm(formula = turb ~ I(B05^3) + I(B05^2) + B05, data = datos)
sum2 <- summary(mod2)
sum2

tibble(turb = datos$turb,
       pred = predict(mod2)) |>
       ggplot(aes(x = pred, y = turb)) +
       geom_point(size = 2, alpha = .5) +
       geom_abline() +
       coord_fixed()
