library(lubridate)
library(glue)
library(corrr)
library(tidyverse)

# datos SAMEEP, solo me interesa turbidez (NTU)
sameep <- read_tsv("datos/sameep_historicos.tsv") |>
    filter(param == "turbidez") |>
    select(-param, fecha, turb = valor)

sameep |>
    mutate(mes = month(fecha)) |>
    mutate(año = year(fecha)) |>
    mutate(dia = day(fecha)) |>
    mutate(fecha_X = ymd(glue("2020-{mes}-{dia}"))) |>
    mutate(año = as.factor(año)) |>
    ggplot(aes(fecha_X, turb, color = año)) +
    geom_point(alpha = .3) +
    geom_smooth(se = FALSE)

sameep |>
    mutate(mes = month(fecha))

lista_corr <- inner_join(sameep, gee, by = "fecha") |>
    pivot_wider(names_from = banda,
                values_from = reflec) |>
    mutate(mes = month(fecha)) |>
    group_by(mes) |>
    nest() |>
    mutate(corr = map(.x = data, ~ correlate(x = .x))) |>
    select(-data)

lista_corr |>
    unnest(corr) |>
    filter(mes == 1)

sameep_mes <- sameep |>
    mutate(mes = month(fecha))

sameep_enero <- sameep_mes |>
    filter(mes == 1) |>
    select(-mes)

sameep_enero |>
    filter(turb < 1000) |>
    mutate(fecha_X = ymd(glue("2020-{month(fecha)}-{day(fecha)}"))) |>
    ggplot(aes(fecha_X, turb)) +
    geom_point()

gee_enero |>
    mutate(año = year(fecha)) |>
    mutate(año = as.factor(año)) |>
    pivot_longer(cols = starts_with("B"),
                 names_to = "banda",
                 values_to = "reflec") |>
    ggplot(aes(año, reflec)) +
    geom_boxplot() +
    facet_wrap(~banda)

gee_mes <- gee |>
    pivot_wider(names_from = banda,
                values_from = reflec) |>
    mutate(mes = month(fecha))

gee_enero <- gee_mes |>
    filter(mes == 1) |>
    select(-mes)

gee_enero |>
    distinct(fecha) |>
    nrow() # 25

sameep_enero |>
    distinct(fecha) |>
    nrow() # 155

datos_enero <- inner_join(gee_enero, sameep_enero, by = "fecha") |>
    select(-fecha)

datos_enero |>
    correlate() |>
    shave() |>
    filter(term == "turb")

mod_enero1 <- lm(formula = turb ~ B04, data = datos_enero)
sum_enero <- summary(mod_enero1)
sum_enero
