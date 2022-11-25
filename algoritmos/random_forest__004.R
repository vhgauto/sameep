# librería
library(lubridate)
library(vip)
library(ggtext)
library(glue)
library(tidymodels)
library(tidyverse)

theme_set(theme_bw())

# datos -------------------------------------------------------------------

# datos SAMEEP, solo me interesa turbidez (NTU)
sameep_tidy <- read_tsv("datos/sameep_historicos.tsv") |>
  filter(param == "turbidez") |>
  select(-param, fecha, turb = valor)

# datos GIS, obtenidos de GEE
# https://code.earthengine.google.com/?scriptPath=users%2Fvhgauto%2FGISTAQ%3Asameep_historico
gee <- read_tsv("datos/reflec_linea_puntos_gee.tsv")

# acomodo los datos, columnas -> bandas/turb/fecha
gee_tidy <- gee |>
  pivot_wider(names_from = banda, values_from = reflec)

# combino los datos
datos <- inner_join(gee_tidy, sameep_tidy, by = "fecha")

# random forest -----------------------------------------------------------

set.seed(123)

# división de los datos
turb_split <- initial_split(datos, strata = turb)
turb_train <- training(turb_split)
turb_test <- testing(turb_split)

# receta
# uso los datos de entrenamiento
# considero todas las bandas y las fechas (meses)
turb_rec <- recipe(turb ~ ., data = turb_train) |>
  step_date(fecha, features = "month")

turb_prep <- prep(turb_rec)
baked <- bake(turb_prep, new_data = NULL)

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

turb_folds <- vfold_cv(turb_train)

# se realizan 10 particiones, ya que hacen falta muchas pruebas para definir
# el afinamiento más adecuado
doParallel::registerDoParallel() # procesamiento en paralelo

set.seed(345)

# uso una grilla de 20 puntos para elegir los hiperparámetros, al tanteo
tune_res <- tune_grid(
  tune_wf,
  resamples = turb_folds,
  grid = 20) # ¡¡¡LLEVA TIEMPO!!!

# .metrics: rsq = R^2; rmse = root mean square error
tune_res |>
  collect_metrics() |>
  pivot_longer(cols = min_n:mtry,
               values_to = "value",
               names_to = "parameter") |>
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_grid(.metric ~ parameter, scales = "free", switch = "y") +
  labs(x = NULL, y = NULL)

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
  grid = rf_grid) # ¡¡¡LLEVA TIEMPO!!!

regular_res |>
  collect_metrics() |>
  mutate(min_n = factor(min_n)) |>
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, linewidth = 1.5) +
  geom_point() +
  facet_wrap(~ .metric, scales = "free") +
  labs(y = NULL)

# modelo final ------------------------------------------------------------

# los valores más altos de rsq y los valores más bajos de rmse se dan con:
# mtry ~ 10 ; min_n ~ 1

# elijo como criterio el major (mayor) rsq (R^2)
# mtry = 10, min_n = 1
best_auc <- select_best(regular_res, metric = "rsq")

# actualizo el modelo con los resultados del afinamiento de los hiperparámetros
final_rf <- finalize_model(
  tune_spec,
  best_auc)

# 'final_rf' contiene los hiperparámetros afinados/tuneados, p/el random forest

# verifico la importancia de las variables
gg_vip <- final_rf |>
  set_engine("ranger", importance = "permutation") |>
  fit(turb ~ ., data = juice(turb_prep)) |>
  vip(geom = "col") +
  scale_fill_viridis_d() +
  labs(title = "El <span style='color:darkblue'>**mes**</span> es el parámetro más
   importante para estimar la turbidez",
       y = "Importancia") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  coord_flip(expand = FALSE, ylim = c(0, 50000)) +
  theme(
    aspect.ratio = .5,
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(color = "black"),
    plot.title = element_markdown(),
    plot.margin = margin(5, 20, 5, 0)
  )

ggsave(plot = gg_vip,
       filename = "figuras/vip_param__001.png",
       width = 20,
       height = 10,
       units = "cm",
       dpi = 300)
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

# RMSE = 139
# R^2 = 0.902

# valores predichos VS valores reales (split=test)
turb_m <- max(sameep_tidy$turb)

final_res |>
  collect_predictions() |>
  ggplot(aes(.pred, turb)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_abline() +
  coord_fixed(xlim = c(0, turb_m), ylim = c(0, turb_m)) +
  theme(
    aspect.ratio = 1
  )

final_res %>%
  collect_predictions() |>
  lm(turb ~ .pred, data = _) |>
  summary() # R2 = 0.9024

# turb VS fecha -----------------------------------------------------------

# predicciones
pred_rf <- final_res |>
  collect_predictions() |>
  select(.pred, turb)

# datos de testeo, con fechas
turb_rf <- turb_test |>
  select(fecha, turb)

# combino las predicciones con las fechas, grafico
inner_join(pred_rf, turb_rf, by = "turb") |>
  ggplot(aes(x = fecha, y = .pred)) +
  geom_line(data = sameep_tidy, aes(x = fecha, y = turb),
            color = "darkgrey") +
  geom_point(size = 2, color = "darkblue", alpha = 1, shape = 4,
             show.legend = TRUE) +
  scale_x_date(date_breaks = "4 month", date_labels = "%m-%y") +
  scale_y_continuous(breaks = seq(0, 1500, 250),
                     labels = scales::label_number(big.mark = ".",
                     decimal.mark = ",")) +
  coord_cartesian(expand = FALSE, ylim = c(0, 1500)) +
  labs(x = NULL, y = "Turbidez (NTU)", title =
         "Turbidez predecida mediante random forest, en el set de testeo") +
  theme(
    aspect.ratio = .5,
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = .25),
    plot.title = element_markdown(),
    legend.position = "top"
  )

# predicción VS fecha -----------------------------------------------------

# extraigo el workflow del último entrenamiento, ya afinado
workflow_final <- final_res |>
  extract_workflow()

# creo el set de datos GIS para predecir su turb
# formado a partir de: test split + GIS sin turb
gee_new <- gee_tidy |>
  filter(fecha > max(sameep_tidy$fecha))

gee_test_new <- turb_test |>
  select(-turb) |>
  bind_rows(gee_new)

# aplico el workflow al nuevo dataset
pred_new <- predict(workflow_final, gee_test_new)

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

mae <- Metrics::mae(actual = aa$turb,
                    predicted = aa$.pred) |>
  round(digits = 1) |>
  sub(pattern = "\\.", replacement = ",", x = _)

bias <- Metrics::bias(actual = aa$turb,
                      predicted = aa$.pred) |> round(digits = 1) |>
  sub(pattern = "\\.", replacement = ",", x = _)

gg_rf <- full_join(sameep_tidy, pred_new |> bind_cols(gee_test_new), by = "fecha") |>
  select(fecha, SAMEEP = turb, RF = .pred) |>
  pivot_longer(cols = c(SAMEEP, RF),
               names_to = "param",
               values_to = "turb") |>
  ggplot(aes(x = fecha, y = turb, color = param, shape = param)) +
  geom_vline(xintercept = range(sameep_tidy$fecha)[2],
             color = "darkgrey", linetype = 2) +
  geom_line(data = . %>% filter(param == "RF"), alpha = .2) +
  geom_line(data = . %>% filter(param == "SAMEEP")) +
  geom_point(size = 1, alpha = .8) +
  scale_x_date(breaks = seq(ymd(20170101), ymd(20230101), by = "6 month"),
               date_labels = "%b\n%y") +
  scale_y_continuous(breaks = seq(0, 1500, 250), 
                     labels = scales::label_number(big.mark = ".",
                                                   decimal.mark = ",")) +
  scale_shape_manual(values = c(4, NA)) +
  scale_color_manual(values = c("darkblue", "darkgrey")) +
  coord_cartesian(ylim = c(0, 1500), 
                  xlim = c(ymd(20170101), ymd(20230101)),
                  expand = FALSE) +
  labs(x = NULL, y = "Turbidez (NTU)", color = NULL, shape = NULL,
       title = "Turbidez estimada mediante 
       <span style='color:darkblue'>**Random Forest**</span> (RF),
       comparada con <br> las mediciones diarias de 
       <span style='color:darkgrey'>**SAMEEP**</span>",
       caption = glue("R<sup>2</sup> = {r2}; RMSE = {rmse} NTU; MAE = {mae} NTU")) +
  guides(color = guide_legend(override.aes =
          list(shape = c(4, NA), linetype = c(NA, 1), size = c(3, 9)))) +
  theme(
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = .25),
    legend.position = c(.25, .85),
    legend.margin = margin(0, 0, 0, 0),
    legend.key = element_blank(),
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = "black",
                                     linetype = 2, linewidth = .1),
    plot.title = element_markdown(),
    plot.caption = element_markdown(),
    plot.margin = margin(5, 15, 5, 5)
  )

ggsave(plot = gg_rf,
       filename = "figuras/pred_turb__001.png",
       width = 20,
       height = 10,
       units = "cm",
       dpi = 300)

browseURL("figuras/pred_turb__001.png")

# intervalo de confianza?
# browseURL("http://optimumsportsperformance.com/blog/confidence-intervals-for-random-forest-regression-using-tidymodels-sort-of/")
# browseURL("https://stats.stackexchange.com/questions/56895/do-the-predictions-of-a-random-forest-model-have-a-prediction-interval")