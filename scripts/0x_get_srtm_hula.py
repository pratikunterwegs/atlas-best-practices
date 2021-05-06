#!/usr/bin/env python
# coding: utf-8

import subprocess

try:
    import geemap
except ImportError:
    print('geemap package not installed. Installing ...')
    subprocess.check_call(["python", '-m', 'pip', 'install', 'geemap'])

# Checks whether this notebook is running on Google Colab
try:
    import google.colab
    import geemap.eefolium as geemap
except:
    import geemap

# Authenticates and initializes Earth Engine
import ee

try:
    ee.Initialize()
except Exception as e:
    ee.Authenticate()
    ee.Initialize()

# get data
dataset = ee.Image('USGS/SRTMGL1_003');
elevation = dataset.select('elevation');
table = ee.FeatureCollection("users/pratik_unterwegs/hula_valley");

# clip data
elevation_clip = elevation.clip(table.geometry());

# visualisation
sld_ramp =   '<RasterSymbolizer>' +     '<ColorMap type="ramp" extended="false" >' +       '<ColorMapEntry color="#0D0887" quantity="0" label="0"/>' +       '<ColorMapEntry color="#CC4678" quantity="200" label="300" />' +       '<ColorMapEntry color="#F0F921" quantity="400" label="500" />' +     '</ColorMap>' +   '</RasterSymbolizer>';

# print layers
vis = {'bands': ['elevation']}
Map = geemap.Map(center=[36.0005,-78.9], zoom=12)
Map.addLayer(elevation_clip.sldStyle(sld_ramp), {}, 'elevation')
Map.addLayer(table)
Map.addLayerControl()

Map

# export image to drive
downConfig = {'scale': 30, "maxPixels": 1.0E13, 'driveFolder': 'srtm_30'}  # scale means resolution.
name = "srtm_30"
# print(name)
task = ee.batch.Export.image(elevation_clip, name, downConfig)
task.start()
