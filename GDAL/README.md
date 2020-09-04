# Geospatial processing using GDAL

This page contains the slides and resources from the GDAL stats club presentation on 01/09/2020. 

For installation of GDAL, please refer to:
https://trac.osgeo.org/osgeo4w/ (OSGeo4W)
https://sandbox.idre.ucla.edu/sandbox/tutorials/installing-gdal-for-windows (Windows)
https://github.com/domlysz/BlenderGIS/wiki/How-to-install-GDAL (Windows/Linux/Mac)

The powerpoint slides contain sample GDAL scripts that could be copied and ran in command prompt or OSGeo4W shell. Remember to change the filepaths if you decide to give it a go. The testing dataset includes:
(1) A Sentinel 2 image of Sharp Peak, Hong Kong in HK80 projection (Sentinel_epsg_2326.tif)
(2) DTM tiles of the same region (in the DTM_tiles folder, in GeoTiff format under WGS84 projection)
(3) Output raster/vector files produced during the presentation

GDAL code could also be called from R using system() provided that the path variables are set correctly.


Author: Aland Chan
alandchan798@gmail.com
04/09/2020