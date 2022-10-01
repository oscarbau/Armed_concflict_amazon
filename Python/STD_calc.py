## Calculates cell statistics
## by: Oscar Bautista
## date: 18/01/2017

print"importing libs"
import arcpy
from arcpy import env
from arcpy.sa import *
print"checking extensions"
arcpy.CheckOutExtension("Spatial")
print""

print "Variables"
print""
### Path an names definition
arcpy.env.workspace = r"C:\ITC\Thesis\gisdata\Amazon_01_05\run_1\prob"#raw_input(r"Path to files: ")
print""

out_path = r"C:\ITC\Thesis\gisdata\Amazon_01_05\run_1" #raw_input(r"Path for result: ")
print""
out_name ="STD.tif"#raw_input(r"mosaic name: ")+".tif"
print""
output = out_path+"\\"+out_name
print output
#### Make a list of rasters in workspace
grid_list = arcpy.ListRasters("*", "")
for rasters in grid_list:
    print rasters
print""

#### calculate statistics
arcpy.env.overwriteOutput = True
print
outCellStatistics = CellStatistics(grid_list, "STD", "NODATA")
outCellStatistics.save(r"C:\ITC\Thesis\gisdata\Amazon_01_05\run_1\prob\STD.tif")
##
print out_name, " was created"

outmean = CellStatistics(grid_list, "MEAN", "NODATA")
outmean.save(r"C:\ITC\Thesis\gisdata\Amazon_01_05\run_1\MEAN.tif")
