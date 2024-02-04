#!/usr/bin/env python
# coding: utf-8

# solicitudes HTTP
import requests

# manejo de datos
import pandas as pd

# fechas
from datetime import datetime, timedelta

# acceso a las credenciales
import os

# acceso al token
import certifi

# lectura de JSON, se usa al obtener el token
import json

# https://documentation.dataspace.copernicus.eu/APIs/OData.html#query-collection-of-products

# URL base del catálogo
catalogue_odata_url = "https://catalogue.dataspace.copernicus.eu/odata/v1"

# fechas para la búsqueda de productos
fecha_i = (datetime.today() + timedelta(days=-1)).strftime('%Y-%m-%d')
fecha_f = datetime.today().strftime('%Y-%m-%d')

# parámetros de búsqueda: S2, L2A, cobertura de nubes, ROI, rango de fechas
collection_name = "SENTINEL-2"
product_type = "S2MSI2A"
max_cloud_cover = 1
aoi = "POINT(-58.81348666883592 -27.488354054598737)"
search_period_start = f"{fecha_i}T00:00:00.000Z"
search_period_end = f"{fecha_f}T00:00:00.000Z"

# término de búsqueda
search_query = f"{catalogue_odata_url}/Products?$filter=Collection/Name eq '{collection_name}' and Attributes/OData.CSC.StringAttribute/any(att:att/Name eq 'productType' and att/OData.CSC.StringAttribute/Value eq '{product_type}') and OData.CSC.Intersects(area=geography'SRID=4326;{aoi}') and ContentDate/Start gt {search_period_start} and ContentDate/Start lt {search_period_end}"

# respuesta del servidor y resultado
response = requests.get(search_query).json()
result = pd.DataFrame.from_dict(response["value"])

# credenciales, nombre de usuario y contraseña
username = os.environ['S2MSI_USERNAME']
password = os.environ['S2MSI_PASSWORD']

# obtengo el token
auth_server_url = "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"
data = {
    "client_id": "cdse-public",
    "grant_type": "password",
    "username": username,
    "password": password,
}

response_cred = requests.post(auth_server_url, data=data, verify=True, allow_redirects=False)
access_token = json.loads(response_cred.text)["access_token"]

if not os.path.exists("safe"):
    os.makedirs("safe")

if len(result) == 0:
    print("\n\n---NO HAY PRODUCTO DISPONIBLE PARA EL DÍA DE LA FECHA---\n\n")
elif os.path.isfile("safe/producto.zip") == True:
    print("\n\n---PRODUCTO YA DESCARGADO---\n\n")
else:
    # ID y nombre del producto a descargar
    producto_id = result["Id"][0]
    producto_nombre = result["Name"][0]

    print("ID del producto", producto_id)
    print("Nombre del producto", producto_nombre)

    # https://documentation.dataspace.copernicus.eu/APIs/OData.html#product-download

    # URL de descarga del producto
    url = f"https://zipper.dataspace.copernicus.eu/odata/v1/Products({producto_id})/$value"

    headers = {"Authorization": f"Bearer {access_token}"}

    session = requests.Session()
    session.headers.update(headers)
    response_prod = session.get(url, headers=headers, stream=True)

    print("\n\n---DESCARGANDO PRODUCTO---\n\n")
    # descarga de .zip con SAFE

    with open("safe/producto.zip", "wb") as file:
        for chunk in response_prod.iter_content(chunk_size=8192):
            if chunk:
                file.write(chunk)

    print("\n\n---PRODUCTO DESCARGADO---\n\n")

"safe/producto.zip"
