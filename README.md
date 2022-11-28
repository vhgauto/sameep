# Proyecto GISTAQ SAMEEP 2020

El presente repositorio contiene los scripts para el desarrollo de un algoritmo que permite estimar la turbidez del agua a la entrada de la planta potabilizadora La Toma, de la empresa SAMEEP (Servicio de Agua y Mantenimiento Empresa del Estado Provincial), en ciudad de Barranqueras, provincia del Chaco.

Los datos satelitales provienen de la plataforma espacial Sentinel-2, desarrollada por la Agencia Espacial Europea. El producto utilizado es [L2A](https://sentinels.copernicus.eu/web/sentinel/user-guides/sentinel-2-msi/processing-levels/level-2), en reflectancia de superficie, con corrección atmosférica automática.

La extracción de los valores de reflectancia se lleva a cabo en 20 píxeles en cercanía de La Toma, sobre la línea central a lo largo del riacho Barranqueras.

Los valores extraídos son almacenados en una base de datos para su posterior uso en la estimación de turbidez del agua.

Mediante un modelado a partir de la técnica de *random forest* se obtienen valores de turbidez estimados a partir de datos satelitales.

El entrenamiento del modelo se realizó con datos históricos provistos por SAMEEP de La Toma, y sus correspondientes datos adquiridos por Sentinel-2.

Las tareas que se ejecutan automáticamente en este resopositorio son:

- Descarga del producto Sentinel-2 MSI, nivel de procesamiento L2A, tile 21JUK, del día de la fecha.
- Recorte del producto a la región de interés y generación de un stack con las bandas espectrales de importancia (B01, B02, B03, B04, B05, B06, B07, B08, B8A, B11, B12).
- Verifico la nubosidad del sitio. En caso de haber nubes presentes, se detiene la operación.
- Extracción de los valores de píxel (reflectancia de superficie) de los puntos muestrales ubicados en el riacho Barranqueras, en la entrada de agua a la planta potabilizadora [La Toma](https://goo.gl/maps/WMDzJCJnPCbFjQhb8).
- Generación y actualización de la base de datos (.tsv) unificada.
- Entrenamiento y estimación de la turbidez a partir de los datos satelitales últimos, mediante la técnica de *random forest*.
- Creación de website con el último dato disponible de turbidez (NTU).

El valor de turbidez actualizado correspondiente a la última fecha disponible se encuentra en el siguiente [link](https://vhgauto.github.io/sameep/).

Contacto:  
GISTAQ: [gistaq@ca.frre.utn.edu.ar](mailto:gistaq@ca.frre.utn.edu.ar)  
Dirección: French 414, Resistencia, Chaco, CP 3500  
Encargado del presente repositorio: [Víctor Gauto](mailto:victor.gauto@outlook.com)

[![Ejecuto el Proyecto GISTAQ SAMEEP 2020](https://github.com/vhgauto/sameep/actions/workflows/run_pipeline.yml/badge.svg)](https://github.com/vhgauto/sameep/actions/workflows/run_pipeline.yml)
