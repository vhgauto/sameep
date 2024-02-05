# Proyecto GISTAQ-SAMEEP 2021

El **Grupo de Investigación Sobre Temas Ambientas y Químicos** ([GISTAQ](https://www.facebook.com/GISTAQ/)), perteneciente a la **Universidad Tecnológica Nacional Facultad Regional Resistencia** ([UTN-FRRe](https://www.frre.utn.edu.ar/)), en el marco del proyecto de investigación "Caracterización fisicoquímica de cuerpos de aguas continentales para la evaluación de la utilización de algoritmos en el monitoreo satelital de la calidad del agua" (MSPPBRE0008091), logró el desarrollo de un algoritmo para la estimación de turbidez en el agua de ingreso a la planta de tratamiento de La Toma, ciudad de Barranqueras, Provincia del Chaco, Argentina.

Dicho algoritmo combina datos de laboratorio con técnicas de teledetección mediante plataformas satelitales y aprendizaje automático. Todas las herramientas empleadas son de código libre, abierto y gratuito.

La empresa adoptante es **Servicio de Agua y Mantenimiento Empresa del Estado Provincial** ([SAMEEP](https://sameep.gob.ar/)), que ha colaborado activamente en el presente proyecto.

## Descripción del repositorio

El repositorio contiene los scripts para la descarga de los productos satelitales, extracción y almacenamiento de datos espectrales, generación del algoritmo, obtención del valor estimado de turbidez en el agua y creación del sitio web para visualizar los resultados.

Los datos satelitales provienen de la plataforma espacial **Sentinel-2**, desarrollada por la Agencia Espacial Europea. El producto utilizado es [L2A](https://sentinels.copernicus.eu/web/sentinel/user-guides/sentinel-2-msi/processing-levels/level-2), en reflectancia de superficie, con corrección atmosférica automática.

La extracción de los valores de reflectancia de superficie se lleva a cabo en 20 píxeles en cercanía al ingreso del agua, sobre la línea central a lo largo del riacho Barranqueras.

Los valores espectrales extraídos son almacenados en una base de datos para su posterior uso en la estimación de turbidez del agua.

El algoritmo consiste en un modelo de **random forest** que estima turbidez (NTU) a partir de los valores de reflectancia de superficie de todas las bandas espectrales disponibles.

El entrenamiento del modelo se realizó con datos históricos provistos por SAMEEP, y los correspondientes datos espectrales adquiridos por Sentinel-2 MSI.

Los hiperparámetros del modelo fueron optimizados mediante la técnica de búsqueda en cuadrícula (*grid search*).

[GitHub Actions](https://docs.github.com/es/actions) permite la ejecución automática de los scripts, que llevan a cabo las siguientes tareas:

- Descarga del producto Sentinel-2 MSI, nivel de procesamiento L2A, tile 21JUK, del día de la fecha. Se emplea la [API](https://documentation.dataspace.copernicus.eu/APIs/OData.html) de Copernicus Data Space Ecosystem.
- Recorte del producto a la región de interés y generación de un stack con las bandas espectrales de importancia (B01, B02, B03, B04, B05, B06, B07, B08, B8A, B11, B12).
- Verifico la nubosidad del sitio. En caso de haber nubes presentes, se detiene la operación.
- Extracción de los valores de píxel (reflectancia de superficie) de los puntos muestrales ubicados en el riacho Barranqueras, en la entrada de agua a la planta potabilizadora [La Toma](https://goo.gl/maps/WMDzJCJnPCbFjQhb8).
- Generación y actualización de la [base de datos](https://github.com/vhgauto/sameep/blob/main/datos/base_de_datos_gis.tsv) unificada.
- Entrenamiento y estimación de la turbidez a partir de los datos satelitales últimos, mediante la técnica de *random forest*.
- Creación de website con el último dato disponible de turbidez (NTU) y serie temporal de las estimaciones del último año. Creado con [*rmarkdown*](https://rmarkdown.rstudio.com/).


La herramienta [Snakemake](https://snakemake.github.io/) permite la gestión del *workflow*, para asegurar la reproducibilidad y la correcta ejecución de los scripts en el orden adecuado.

El valor estimado de turbidez actualizado correspondiente a la última fecha disponible se encuentra a continuación.

## <p style="text-align: center;">[LINK](https://vhgauto.github.io/sameep/)</p>

## Contacto

GISTAQ: [gistaq@ca.frre.utn.edu.ar](mailto:gistaq@ca.frre.utn.edu.ar)  
Dirección: French 414, Resistencia, Chaco, CP 3500  
Encargado del repositorio: [Mgrt. Víctor Gauto](mailto:victor.gauto@outlook.com)

[![Ejecuto el Proyecto GISTAQ SAMEEP 2020](https://github.com/vhgauto/sameep/actions/workflows/run_pipeline.yml/badge.svg)](https://github.com/vhgauto/sameep/actions/workflows/run_pipeline.yml)
