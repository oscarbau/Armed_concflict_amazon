## Mosaic all files in directory
## by: Oscar Bautista
## date: 05/10/2017

import arcpy
print""

### Path an names definition
arcpy.env.workspace = raw_input(r"Path to files: ")
print""
out_path =raw_input(r"Path for result: ")
print""
out_name =raw_input(r"mosaic name: ")+".tif"
print""
output = out_path+"\\"+out_name
print""
#### Make a list of rasters in workspace
grid_list = arcpy.ListRasters("*", "")

#### New mosaic from List of rasters

coor = arcpy.SpatialReference(4326)
arcpy.env.overwriteOutput = True
arcpy.MosaicToNewRaster_management(grid_list, out_path, out_name,
                                       coor, "16_BIT_UNSIGNED",
                                       "0,00025", "1", "BLEND","FIRST")
print out_name, " was created"
