#!/usr/bin/env Rscript

# librerías
library(lubridate)
library(ggtext)
library(glue)
library(showtext)
library(tidymodels)
library(tidyverse)

# tema general de las figuras
theme_set(theme_bw())

# fuentes
font_add(family = "playfair", regular = "extras/PlayfairDisplay.ttf") # título
font_add(family = "victor", regular = "extras/VictorMono.ttf") # números de eje
font_add(family = "ubuntu", regular = "extras/Ubuntu.ttf") # resto del texto

showtext_auto()
showtext_opts(dpi = 300)

# datos -------------------------------------------------------------------

# datos SAMEEP, solo me interesa turbidez (NTU)
sameep_tidy <- read_tsv("datos/sameep_historicos.tsv") |>
  filter(param == "turbidez") |>
  select(-param, fecha, turb = valor)

# datos GIS, obtenidos de GEE
# https://code.earthengine.google.com/?scriptPath=users%2Fvhgauto%2FGISTAQ%3Asameep_historico
gee <- read_tsv("datos/base_de_datos_gis.tsv")

# acomodo los datos, columnas -> bandas/turb/fecha
gee_tidy <- gee |>
  pivot_wider(names_from = banda, values_from = reflec) |>
  unnest(cols = everything())

# combino los datos
datos <- inner_join(gee_tidy, sameep_tidy, by = "fecha") |>
  arrange(fecha)

# remuevo la fecha p/operar con RF
datos2 <- datos |>
  select(-fecha)

# random forest -----------------------------------------------------------

# para asegurar reproducibilidad
set.seed(123)

# división de los datos
turb_split <- initial_time_split(datos2)
turb_train <- training(turb_split)
turb_test <- testing(turb_split)

# receta
# uso los datos de entrenamiento
# considero todas las bandas
turb_rec <- recipe(turb ~ ., data = turb_train)

# afinamiento de hiperparámetros ------------------------------------------
# tune/tuning de los parámetros requeridos para el random-forest, que NO pueden
# obtenerse de los datos gee/sameep
# mtry: the number of predictors to sample at each split
# mtry tiene que ser menor o igual a la cantidad de variables (13 columnas)
# min_n: the number of observations needed to keep splitting nodes
# empleo 1000 árboles

tune_spec <- rand_forest(
  trees = 1000,
  mtry = tune(),
  min_n = tune()) |>
  set_mode("regression") |>
  set_engine("ranger")

# workflow
# útil p/manipular modelos predictivos
tune_wf <- workflow() |>
  add_recipe(turb_rec) |>
  add_model(tune_spec)

# entrenamiento de hiperparámetros ----------------------------------------
# el ajuste requiere de validaciones cruzadas, usando remuestreo
# 'vfold_cv' = V-fold cross-validation, 10 particiones
# a partir de los datos de entrenamiento
set.seed(234)

turb_folds <- vfold_cv(turb_train, v = 10, strata = turb)

# se realizan 10 particiones, ya que hacen falta muchas pruebas para definir
# el afinamiento más adecuado
doParallel::registerDoParallel() # procesamiento en paralelo

set.seed(345)

# uso una grilla de 20 puntos para elegir los hiperparámetros, al tanteo
tune_res <- tune_grid(
  tune_wf,
  resamples = turb_folds,
  grid = 20)

# .metrics: rsq = R^2; rmse = root mean square error

# elijo el rango que genere: rsq -> 1 & rmse -> más bajo
# creo una tabla con todas las combinaciones para verificar
rf_grid <- grid_regular(
  mtry(range = c(5, 10)),
  min_n(range = c(1, 10)),
  levels = 5)

set.seed(456)

# afino los resultados usando la tabla de valores
# más acotada para 'mtry' y 'min_n'
regular_res <- tune_grid(
  tune_wf,
  resamples = turb_folds,
  grid = rf_grid)

# modelo final ------------------------------------------------------------

# elijo como criterio el mayor (~ 1) rsq (R^2)
# mtry = 5, min_n = 10
best_auc <- select_best(regular_res, metric = "rmse")

# actualizo el modelo con los resultados del afinamiento de los hiperparámetros
final_rf <- finalize_model(tune_spec, best_auc)

# 'final_rf' contiene los hiperparámetros afinados/tuneados, p/el RF

# el mes es el parámetros MÁS importante, seguido de las bandas B05 y B04

# modelado final
final_wf <- workflow() |>
  add_recipe(turb_rec) |>
  add_model(final_rf)

# con el último workflow, entreno una vez más el test de entrenamiento
final_res <- final_wf |>
  last_fit(turb_split)

# métricas del random forest, con hiperparámetros afinados
final_res |>
  collect_metrics()

# RMSE: 143.
# R^2: 0.914

# c/los hiperparámetros afinados, paso a {randomForest}
best_mtry <- best_auc$mtry
beat_min_n <- best_auc$min_n

rf_refit <- randomForest::randomForest(
  turb ~ ., data = turb_train, 
  mtry = best_mtry,
  nodesize = beat_min_n,
  ntree = 1000,
  keep.inbag = TRUE)

# predicciones, de todos los árboles
pred.rf <- predict(rf_refit, turb_test, predict.all = TRUE)

# R^2
tibble(turb = turb_test$turb,
       pred = pred.rf$aggregate) |>
  lm(turb ~ pred, data = _) |> # nolint
  summary() |>
  broom::glance() |>
  select(r.squared) |>
  pull()
# R^2 =  0.9118121

# figura SAMEEP vs RF (c/intervalos)
# límites superior e inferior
pred_inf <- map_dbl(
  .x = 1:nrow(turb_test),
  ~ mean(pred.rf$individual[.x, ]) - 1.96 * sd(pred.rf$individual[.x, ]))

pred_sup <- map_dbl(.x = 1:nrow(turb_test),
~ mean(pred.rf$individual[.x, ]) + 1.96 * sd(pred.rf$individual[.x, ]))

# en un único tibble: fecha, turb, pred, pred_sup & pred_inf
# del test split

fechas_t <- inner_join(turb_test, datos) |>
  pull(fecha)

turb_nuevos <- tibble(fecha = fechas_t,
  turb = turb_test$turb,
  pred = pred.rf$aggregate,
  pred_inf = pred_inf,
  pred_sup = pred_sup)

# R^2
r2 <- lm(turb ~ pred, data = turb_nuevos) |>
  summary() %>%
  .$r.squared |>
  round(digits = 3) |>
  sub(pattern = "\\.", replacement = ",", x = _)

# máx eje vertical
turb_m <- max(sameep_tidy$turb) |> round(digits = -2)

# máx eje horizontal
fecha_m <- max(gee_tidy$fecha) |> ceiling_date(unit = "month")

# etiquta R^2
etq <- tibble(
  x = ymd(20211001),
  y = turb_m,
  label = glue("R<sup>2</sup> = {r2}<br>{{randomForest}}"))

fecha_turb <- read_tsv("datos/datos_nuevos.tsv") |>
  distinct(fecha) |>
  pull(fecha)
  
# creo la carpeta para almacenar la firma espectral
dir.create("figuras")

gg_test_RF <- turb_nuevos |>
  select(fecha, SAMEEP = turb, RF = pred, pred_inf, pred_sup) |>
  pivot_longer(
    cols = c(SAMEEP, RF),
    names_to = "param",
    values_to = "turb") |> 
  ggplot(aes(x = fecha, y = turb, color = param, shape = param)) +
  geom_line(data = . %>% filter(param == "SAMEEP")) +
  geom_point(size = 1, alpha = .8) +
  geom_errorbar(
    data = . %>% filter(param == "RF"),
    aes(x = fecha, ymin = pred_inf, ymax = pred_sup),
    inherit.aes = FALSE, alpha = 1, color = "darkblue",
    linewidth = .1, width = 2) +
  geom_richtext(
    data = etq, aes(x = x, y = y, label = label),
    inherit.aes = FALSE, show.legend = FALSE,
    hjust = 1, vjust = 1, fill = NA, label.color = NA,
    family = "victor", size = 3) +
  geom_text(
    aes(x = ymd(20190101), y = 1000, label = "OPCION 1"), show.legend = FALSE) +
  scale_x_date(
    date_labels = "%m\n%Y", date_breaks = "1 month", expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0, turb_m, 250),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_shape_manual(values = c(20, NA)) +
  scale_color_manual(values = c("darkblue", "grey60")) +
  coord_cartesian(
    ylim = c(0, turb_m),
    xlim = c(min(fechas_t), ymd(20211001)),
    expand = FALSE) +
  labs(
    x = NULL, y = "Turbidez (NTU)", color = NULL, shape = NULL,
    title = "Turbidez estimada mediante 
    <span style='color:darkblue'>**Random Forest**</span> (RF),
    comparada con <br> las mediciones diarias de 
    <span style='color:grey60'>**SAMEEP**</span>",
    caption = glue(
      "{format(now(tzone = 'America/Argentina/Buenos_Aires'), '%d/%m/%Y %T')}")) +
  guides(
    color = guide_legend(
      override.aes = list(shape = c(4, NA), linetype = c(NA, 1), size = c(3, 9)))) +
  theme(
    axis.text = element_text(color = "black", family = "victor"),
    axis.text.x = element_text(hjust = 1),
    axis.text.y = element_text(vjust = 0),
    panel.grid.major = element_line(linewidth = .25),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "ivory"),
    legend.position = c(0, 1),
    legend.justification = c(-.05, 1.05),
    legend.text = element_text(family = "ubuntu"),
    legend.key.height = unit(0, "line"),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.key = element_blank(),
    legend.direction = "vertical",
    legend.background = element_rect(
      fill = "ivory", linetype = 2, color = "darkgrey", linewidth = .1),
    plot.title = element_markdown(family = "playfair", size = 16),
    plot.subtitle = element_markdown(family = "ubuntu", size = 8),
    plot.caption = element_markdown(family = "ubuntu", size = 6),
    plot.margin = ggplot2::margin(5, 5, 5, 5),
    plot.background = element_rect(fill = "ivory")
  )

ggsave(
  plot = gg_test_RF,
  filename = "figuras/gg_test_RF_001.png",
  width = 20,
  height = 10,
  units = "cm",
  dpi = 300)

# valores predichos VS valores reales (split=test)
final_res %>%
  collect_predictions() |>
  lm(turb ~ .pred, data = _) |>
  summary() # R2: 0.914

# predicción VS fecha -----------------------------------------------------

# extraigo el workflow del último entrenamiento, ya afinado
workflow_final <- final_res |>
  extract_workflow()

# creo el set de datos GIS para predecir turb
# formado a partir de: test split + datos SIN turb
fecha_turb <- read_tsv("datos/datos_nuevos.tsv") |>
  distinct(fecha) |>
  pull(fecha)

gee_new <- gee_tidy |>
  filter(fecha > max(sameep_tidy$fecha))

gee_test_new <- turb_test |>
  select(-turb) |>
  bind_rows(gee_new)

# aplico el workflow al nuevo dataset
pred_new <- predict(workflow_final, gee_test_new)

# DATO DE TURB NUEVO

pred_turb <- full_join(
  sameep_tidy, pred_new |> bind_cols(gee_test_new), 
  by = "fecha")

dato_turb <- pred_turb |>
  filter(fecha == fecha_turb) |>
  pull(.pred)

dato_turb2 <- format(
  round(dato_turb, 1), nsmall = 1) |>
  sub(pattern = "\\.", replacement = ",", x = _)

# estadísticos
r2 <- final_res |>
  collect_metrics() |>
  filter(.metric == "rsq") |>
  pull(.estimate) |>
  round(digits = 3) |>
  sub(pattern = "\\.", replacement = ",", x = _)

rmse <- final_res |>
  collect_metrics() |>
  filter(.metric == "rmse") |>
  pull(.estimate) |>
  round(digits = 1) |>
  sub(pattern = "\\.", replacement = ",", x = _)

aa <- final_res %>%
  collect_predictions()

mae <- Metrics::mae(
  actual = aa$turb,
  predicted = aa$.pred) |>
  round(digits = 1) |>
  sub(pattern = "\\.", replacement = ",", x = _)

bias <- Metrics::bias(
  actual = aa$turb,
  predicted = aa$.pred) |> round(digits = 1) |>
  sub(pattern = "\\.", replacement = ",", x = _)

# figuras -----------------------------------------------------------------

# máx eje vertical
turb_m <- max(sameep_tidy$turb) |> round(digits = -2)

# máx eje horizontal
fecha_m <- max(gee_tidy$fecha) |> ceiling_date(unit = "month")

# etiquta R^2
etq <- tibble(x = fecha_m, y = turb_m, label = glue("R<sup>2</sup> = {r2}"))

# creo la carpeta para almacenar la firma espectral
dir.create("figuras")

# turb SAMEEP + turb PRED vs fecha
gg_rf <- full_join(
  sameep_tidy, pred_new |> bind_cols(gee_test_new), 
  by = "fecha") |>
  select(fecha, SAMEEP = turb, RF = .pred) |>
  pivot_longer(
    cols = c(SAMEEP, RF),
    names_to = "param",
    values_to = "turb") |>
  ggplot(aes(x = fecha, y = turb, color = param, shape = param)) +
  geom_vline(
    xintercept = range(sameep_tidy$fecha)[2], color = "darkgrey",
    linetype = 2) +
  geom_line(data = . %>% filter(param == "RF"), alpha = .2) +
  geom_line(data = . %>% filter(param == "SAMEEP")) +
  geom_point(size = 1, alpha = .8) +
  geom_richtext(
    data = etq, aes(x = x, y = y, label = label),
    inherit.aes = FALSE, show.legend = FALSE, hjust = 1, vjust = 1, 
    fill = NA, label.color = NA, family = "victor", size = 3) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0, turb_m, 250),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_shape_manual(values = c(4, NA)) +
  scale_color_manual(values = c("darkblue", "darkgrey")) +
  coord_cartesian(
    ylim = c(0, turb_m),
    xlim = c(ymd(20170101), fecha_m),
    expand = FALSE) +
  labs(
    x = NULL, y = "Turbidez (NTU)", color = NULL, shape = NULL,
    title = "Turbidez estimada mediante 
    <span style='color:darkblue'>**Random Forest**</span> (RF),
    comparada con <br> las mediciones diarias de 
    <span style='color:darkgrey'>**SAMEEP**</span>",
    subtitle = glue("Actualizado al {format(fecha_turb, '%d/%m/%Y')}"),
    caption = glue(
      "{format(now(tzone = 'America/Argentina/Buenos_Aires'), '%d/%m/%Y %T')}")) +
  guides(
    color = guide_legend(
      override.aes = list(
        shape = c(4, NA), linetype = c(NA, 1), size = c(3, 9)))) +
  theme(
    axis.text = element_text(color = "black", family = "victor"),
    axis.text.x = element_text(hjust = 1),
    axis.text.y = element_text(vjust = 0),
    panel.grid.major = element_line(linewidth = .25),
    panel.background = element_rect(fill = "ivory"),
    legend.position = c(0, 1),
    legend.justification = c(-.05, 1.05),
    legend.text = element_text(family = "ubuntu"),
    legend.key.height = unit(0, "line"),
    legend.margin = margin(0, 0, 0, 0),
    legend.key = element_blank(),
    legend.direction = "vertical",
    legend.background = element_rect(
      fill = "ivory", linetype = 2, color = "darkgrey", linewidth = .1),
    plot.title = element_markdown(family = "playfair", size = 16),
    plot.subtitle = element_markdown(family = "ubuntu", size = 8),
    plot.caption = element_markdown(
      family = "ubuntu", size = 6, color = "lightgrey"),
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = "ivory")
  )

ggsave(
  plot = gg_rf,
  filename = "figuras/gg_rf__001.png",
  width = 20,
  height = 10,
  units = "cm",
  dpi = 300)

# turb PRED, cuadrado
gg_turb <- dato_turb2 |>
  as_tibble() |>
  ggplot(
    aes(
      x = 0,
      y = 0,
      label = glue(
        "<span style='font-size:20pt'>Turbidez = 
        {value} NTU</span><br>
        <span style='font-size:7pt; color:grey30'>Fecha = 
        {format(fecha_turb, '%d/%m/%Y')}</span>"))) +
  geom_richtext(
    label.color = NA, fill = "ivory", family = "ubuntu", hjust = .5) +
  coord_fixed(xlim = c(-5, 5), ylim = c(-5, 5), clip = "off") +
  labs(
    caption = glue(
      "{format(now(tzone = 'America/Argentina/Buenos_Aires'), '%d/%m/%Y %T')}")) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "ivory", color = NA),
    plot.caption = element_markdown(
      family = "ubuntu", size = 6, color = "ivory")
  )

ggsave(
  plot = gg_turb,
  filename = "figuras/gg_turb__001.png",
  width = 10,
  height = 3,
  units = "cm",
  dpi = 300)

# # FIGURA TURB .PRED VS TIEMPO

# agrego el dato nuevo de .pred a la base de datos de
# predicciones de turbidez
t_p <- read_tsv("datos/turb_pred.tsv")

# si la fecha nueva NO está en la base de datos de turbidez 'turb_pred.tsv',
# que sobrescriba el archivo con el dato nuevo
if(sum(t_p$fecha == fecha_turb) == 0) {
  t_p_nuevo <- pred_turb |>
  filter(fecha == fecha_turb) |>
  select(fecha, .pred)

  t_p2 <- bind_rows(t_p, t_p_nuevo)
  write_tsv(t_p2, "datos/turb_pred.tsv")

  print("Agrego nueva estimación a la base de datos de turbidez")
} else {
   print("Estimación ya presente en la base de datos de turbidez")
}

