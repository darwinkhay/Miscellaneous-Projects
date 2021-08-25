Information about the Hurricane RShiny Dashboard
================
Darwin Khay

## About the dashboard
URL: https://darwinkhay.shinyapps.io/evacorder/

This RShiny dashboard shows the hurricane data for a chosen hurricane
and the mandatory evacuation orders. The hurricane data includes: the
uncertainty cone, path, projected centers, wind speed probabilities,
surface wind fields, tropical storm/hurricane watches and warnings, and
storm surge watches and warnings.

Each map has either a drop down menu or a slider (slider is only for the
mandatory evacuation order map) to render/update the map for a desired
advisory number (drop down menu) or datetime (slider).

The first map shows the mandatory evacuation orders within a certain
datetime range. The second map shows all the hurricane data in one map
with different layers, where each layer can be toggled on or off. The
third map shows the advisory data for the chosen hurricane, and the
fourth map shows the wind speed probabilities. The fifth map shows the
surface wind field; the sixth and seventh map show the tropical storm
and/or hurricane watch and warning data and storm surge watch and
warning data, respectively.

**Each map has layers that can be toggled!**

## Where to get the hurricane data and how to import it

The website to get the hurricane data for any past hurricane is in this
link: <https://www.nhc.noaa.gov/gis/>. Choose a year in the “Archive”
column, then choose the hurricane name.

To use the data, it must be downloaded onto your local machine and then
imported in RStudio. There are mainly two types of files that are
imported: shp files and kml files.

-   To import shp files, use the st\_read() function from the “sf”
    package or use the shapefile() function from the “raster” package
    (st\_read() usually works better).

-   For kml files, they usually come in a kmz file first, but this just
    needs to be unzipped and you’ll get the kml file. Otherwise, to
    import kml files, use the readOGR() function in the “rgdal” package.

## How to use the hurricane data

Once the data has been imported, shp file or kml file, they can then be
used to plot/map. For this dashboard, the maps were rendered using the
“leaflet” package. To use the hurricane data with leaflet, the leaflet
map must first have a base layer, usually in the form of:

leaflet()%&gt;% addTiles()

Everything after the addTiles() part is where the different layers of
the hurricane data are added. Some of the functions to add layers are:
addPolygons(), addPolylines(), addCircles(), etc. To add legends, use
addLegend(). To find more about how to add other kinds layers to a
leaflet map (like markers), or how to manipulate the map in other ways,
use this link: <https://rstudio.github.io/leaflet/>. **Some hurricane
data files are added to a leaflet map using only certain functions!**
