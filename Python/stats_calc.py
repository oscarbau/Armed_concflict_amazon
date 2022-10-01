## Calculates cell statistics
## by: Oscar Bautista
## date: 18/01/2017

print"importing libs"
import arcpy, os
from arcpy import env
from arcpy.sa import *
print"checking extensions"
arcpy.CheckOutExtension("Spatial")
print""

print "Loading variables..."
print""
### Path an names definition
arcpy.env.workspace = raw_input(r"Path to files: ")
print""

out_path = raw_input(r"Path for result: ")
print""
out_name_sd = "prob_stdev.tif"#raw_input(r"output name: ")+".tif"
out_name_mean = "prob_mean.tif"
print""
output_sd = os.path.join(out_path,out_name_sd)
output_mean = os.path.join(out_path,out_name_mean)
#print output_sd
#print output_mean
#print ""
## Make a list of rasters in workspace
grid_list = arcpy.ListRasters("*", "")
for rasters in grid_list:
    print rasters
print""

#### calculate statistics
arcpy.env.overwriteOutput = True
print ""
std_d = CellStatistics(grid_list, "STD", "DATA")
std_d.save(out_name_sd)
print out_name_sd, " was created"

mean_d = CellStatistics(grid_list, "MEAN", "DATA")
mean_d.save(output_mean)
print out_name_mean, " was created"
print""
print "Task completed"
