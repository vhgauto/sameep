#!/usr/bin/env bash

rm -rf safe/*

scripts/obtencion_datos_gis.R

scripts/random_forest.R
