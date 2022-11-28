#!/usr/bin/env bash

rm -rf safe/*

R --no-save --no-restore < scripts/obtencion_datos_gis.R

R --no-save --no-restore < scripts/random_forest.R
