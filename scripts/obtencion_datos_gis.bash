#!/usr/bin/env bash

python scripts/descarga_safe.py

R --no-save --no-restore < scripts/obtencion_datos_gis.R

R --no-save --no-restore < scripts/random_forest.R
